var SEPARATOR_MULTIPLE_PARAMETERS = "==="; //used to separate multiple attribute VALUES
var SEPARATOR_MULTIPLE_VALUES = ";";
var DATE_FORMAT = 'MM/DD/YYYY';
var TIME_FORMAT = 'HH:ss';

var ACTIVELY_SAVED_FIELD_ID = "collect_boolean_actively_saved";
var NESTED_ATTRIBUTE_ID_PATTERN = /\w+\[\w+\]\.\w+/;
var EXTRA_FIELD_CLASS = "extra";
var MAX_DATA_UPDATE_RETRY_COUNT = 5;

var DEFAULT_STATE = "default";
var LOADING_STATE = "loading";
var COLLECT_EARTH_NOT_RUNNING_STATE = "collectEarthNotRunning";

var $form = null; //to be initialized
var stateByInputFieldName = {};
var lastUpdateRequest = null; //last update request sent to the server
var lastUpdateInputFieldName = null;
var currentStepIndex = null;

//To be used by the method that saves the data automatically when the user
//interacts with the form
//DO NOT REMOVE
var ajaxTimeout = null;
var consoleBox = null;

$(function() {
	if (DEBUG) {
		initLogConsole();
		log("initializing");
		log("using host: " + HOST);
	}
	$form = $("#formAll");
	$stepsContainer = $(".steps");

	initSteps();
	fillYears();
	initCodeButtonGroups();
	initDateTimePickers();
	initBooleanButtons();
	initializeChangeEventSaver();
	// Declares the Jquery Dialog ( The Bootstrap dialog does
	// not work in Google Earth )
	$("#dialogSuccess").dialog({
		modal : true,
		width : "400",
		autoOpen : false,
		buttons : {
			Ok : function() {
				$(this).dialog("close");
			}
		}
	});
	// SAVING DATA WHEN USER SUBMITS
	$form.submit(function(e) {
		e.preventDefault();
		
		submitData();
	});

	$(".code-item").tooltip();

	checkIfPlacemarkAlreadyFilled();
});

var submitData = function() {
	sendDataUpdateRequest(findById(ACTIVELY_SAVED_FIELD_ID), true, true);
};

var updateData = function(inputField, delay) {
	sendDataUpdateRequest(inputField, false, true, delay);
};

var sendCreateNewRecordRequest = function() {
	sendDataUpdateRequest(findById(ACTIVELY_SAVED_FIELD_ID), false, true);
};

var sendDataUpdateRequest = function(inputField, activelySaved, blockUI, delay, retryCount) {
	delay = defaultIfNull(delay, 100);
	retryCount = defaultIfNull(retryCount, 0);
	if (DEBUG) {
		log("sending update request (delay=" + delay + ")");
	}
	var inputFieldName = $(inputField).attr("id");
	if (lastUpdateInputFieldName == inputFieldName) {
		abortLastUpdateRequest(); 	// So that the form
									// is not saved twice
									// if the user
									// clicks the submit
									// button before the
									// auto-save timeout
									// has started
									// Mark this as the "real submit" (as opposed
									// when saving data just because the user closes
									// the window) so we can show the placemark as
									// interpreted
	}
	setActivelySaved(activelySaved);

	// Set a timeout so that the data is only sent to the server
	// if the user stops clicking for over one second

	ajaxTimeout = setTimeout(function() {
		var data = createPlacemarkUpdateRequest(inputField);
		
		lastUpdateRequest = $.ajax({
			data : data,
			type : "POST",
			url : $form.attr("action"),
			timeout: 2000,
			dataType : 'json',
			beforeSend : function() {
				if (blockUI) {
					if (activelySaved) {
						$.blockUI({
							message : 'Submitting data..'
						});
					} else {
						$.blockUI({
							message : null,
							overlayCSS: { backgroundColor: 'transparent' }
						});
					}
				}
			}
		})
		.done(function(json) {
			if (json.success) {
				handleSuccessfullDataUpdateResponse(json, activelySaved, blockUI);
			} else {
				handleFailureDataUpdateResponse(inputField, activelySaved, blockUI, retryCount, 
					json.message);
			}
		})
		.fail(function(xhr, textStatus, errorThrown) {
			// try again
			if("abort" != errorThrown) {
				if (isSuccessfullResponse(xhr.responseText)) {
					if (DEBUG) {
						log("failed but the response is successfull: " + xhr.responseText);
					}
					handleSuccessfullDataUpdateResponse($.parseJSON(xhr.responseText), activelySaved, blockUI);
				} else {
					handleFailureDataUpdateResponse(inputField, activelySaved, blockUI, retryCount, 
						errorThrown, xhr, textStatus, errorThrown);
				}
			}
		})
		.always(function() {
			lastUpdateRequest = null;
			lastUpdateInputFieldName = null;

		});
	}, delay);
	
	lastUpdateInputFieldName = inputFieldName;
};

var isValidResponse = function(text) {
	try {
		var json = $.parseJSON(text);
		return json.hasOwnProperty("success");
	} catch(error) {
		return false;
	}
};

var isSuccessfullResponse = function(text) {
	try {
		var json = $.parseJSON(text);
		return json.success;
	} catch(error) {
		return false;
	}
};

var handleSuccessfullDataUpdateResponse = function(json, showFeedbackMessage, unblockWhenDone) {
	if (DEBUG) {
		log("data updated successfully, updating UI...");
	}
	interpretJsonSaveResponse(json, showFeedbackMessage);
	
	changeState(DEFAULT_STATE);
	
	if (unblockWhenDone) {
		$.unblockUI();
	}
	if (DEBUG) {
		log("UI update complete");
	}
};

var handleFailureDataUpdateResponse = function(inputField, activelySaved, blockUI, retryCount, errorMessage, xhr, textStatus, errorThrown) {
	if (retryCount < MAX_DATA_UPDATE_RETRY_COUNT){
		if (DEBUG) {
			log("error updating data. retrying for the " + (retryCount+1) + " time");
		}
		sendDataUpdateRequest(inputField, activelySaved, blockUI, 1000, retryCount + 1);
	} else {
		undoChanges();
		var logErrorMessage = "error updating data: " + errorMessage;
		if (xhr) {
			logErrorMessage += " Status = " + xhr.status + "; Text status = " 
					+ textStatus + "; error thrown = " + errorThrown + "; response = " + xhr.responseText;
		}
		logError(logErrorMessage);

		if (blockUI) {
			$.unblockUI();
		}
	}
};

var createPlacemarkUpdateRequest = function(inputField) {
	var values;
	if (inputField == null) {
		values = serializeFormToJSON($form);
	} else {
		values = {};
		values[encodeURIComponent($(inputField).attr('name'))] = $(inputField).val();
		values[encodeURIComponent(ACTIVELY_SAVED_FIELD_ID)] = findById(ACTIVELY_SAVED_FIELD_ID).val();
		$form.find("." + EXTRA_FIELD_CLASS).each(function() {
			var $this = $(this);
			values[encodeURIComponent($this.attr('name'))] = $this.val()
		});
	}
	var data = {
		placemarkId : getPlacemarkId(),
		values : values,
		currentStep : currentStepIndex,
		partialUpdate : true
	};
	return data;
}

var abortLastUpdateRequest = function() {
	clearTimeout(ajaxTimeout);

	if (lastUpdateRequest != null) {
		if (DEBUG) {
			log("abort last update request");
		}
		lastUpdateRequest.abort();
		lastUpdateRequest = null;
	}
};

var undoChanges = function() {
	fillDataWithJson(stateByInputFieldName);
};

var interpretJsonSaveResponse = function(json, showFeedbackMessage) {
	updateFieldStateCache(json.inputFieldInfoByParameterName);
	updateInputFieldsState(json.inputFieldInfoByParameterName);
	fillDataWithJson(json.inputFieldInfoByParameterName);

	if (showFeedbackMessage) { // show feedback message
		if (json.success) {
			if (isAnyErrorInForm()) {
				var message = "";
				for(var key in stateByInputFieldName) {
					var info = stateByInputFieldName[key];
					if (info.inError) {
						var inputField = findById(key);
						var label;
						if (NESTED_ATTRIBUTE_ID_PATTERN.test(key)) {
							label = getEnumeratedEntityNestedAttributeErrorMessageLabel(inputField);
						} else {
							label = inputField.length > 0 ? OF.UI.Forms.getFieldLabel(inputField): "";
						}
						message += label + " : " + info.errorMessage + "<br>";
					}
				}
				showErrorMessage(message);

				// Resets the "actively saved" parameter to false so that it is
				// not sent as true when the user fixes the validation
				setActivelySaved(false);
			} else {
				showSuccessMessage(json.message);
				forceWindowCloseAfterDialogCloses($("#dialogSuccess"));
			}
		} else {
			showErrorMessage(json.message);
		}
	}
};

var isAnyErrorInForm = function() {
	for(var key in stateByInputFieldName) {
		var info = stateByInputFieldName[key];
		if (info.visible && info.inError) {
			return true;
		}
	};
	return false;
};

var getEnumeratedEntityNestedAttributeErrorMessageLabel = function(inputField) {
	var columnIndex = inputField.closest("td").index();
	var rowHeadingEl = inputField.closest("tr").find("td").eq(0);
	var rowHeading = rowHeadingEl.text();
	var attributeHeadingEl = inputField.closest("table").find("thead tr th").eq(columnIndex);
	var attributeHeading = attributeHeadingEl.text();
	var entityHeading = inputField.closest("section").find("legend").text();
	var label = entityHeading + " (" + rowHeading + ") " + attributeHeading;
	return label;
};

var updateInputFieldsState = function(inputFieldInfoByParameterName) {
	if (DEBUG) {
		log("updating the state of " + Object.keys(inputFieldInfoByParameterName).length + " input fields...");
	}
	if (DEBUG) {
		log("updating possible values in parent-child coded variables");
	}
	$.each(inputFieldInfoByParameterName, function(fieldName, info) {
		var el = findById(fieldName);
		if (el.length == 1) {
			var parentCodeFieldId = el.data("parentIdFieldId");
			var hasParentCode = parentCodeFieldId && parentCodeFieldId != "";
			if (hasParentCode) {
				switch (el.data("fieldType")) {
				case "CODE_SELECT":
					var oldValue = el.val();
					var possibleItems = info.possibleCodedItems ? info.possibleCodedItems: [];
					OF.UI.Forms.populateSelect(el, possibleItems, "code", "label");
					el.val(oldValue);
					if (el.val() == null) {
						//TODO set first option
						el.val("-1"); //set N/A option by default
					}
					break;
				case "CODE_BUTTON_GROUP":
					var parentCodeInfo = inputFieldInfoByParameterName[parentCodeFieldId];
					var parentCodeItemId = parentCodeInfo.codeItemId;
					var groupContainer = el.closest(".code-items-group");
					
					var itemsContainers = groupContainer.find(".code-items");
					//itemsContainers.hide();
					itemsContainers.css( "display", "none")

					var validItemsContainer = groupContainer.find(".code-items[data-parent-id='" + parentCodeItemId + "']");
					if (validItemsContainer.length > 0 && validItemsContainer.is(':hidden')) {
						//validItemsContainer.show();
						validItemsContainer.css( "display", "block")
					}
					break;
				}
			}
		}
	});
	if (DEBUG) {
		log("updating errors feedback");
	}
	var changedFieldNames = [];
	var errors = [];
	$.each(inputFieldInfoByParameterName, function(fieldName, info) {
		changedFieldNames.push(fieldName);
		errors.push({
			field : fieldName,
			defaultMessage : info.errorMessage
		});
	});
	OF.UI.Forms.Validation.updateErrorMessageInFields($form, changedFieldNames, errors);

	updateStepsErrorFeedback();

	if (DEBUG) {
		log("updating fields relevance");
	}

	// manage fields visibility
	$.each(inputFieldInfoByParameterName, function(fieldName, info) {
		var field = findById(fieldName);
		var formGroup = field.closest('.form-group');
		formGroup.toggleClass("notrelevant", !(info.visible));
	});

	// manage tabs/steps visibility
	$form.find(".step").each(function(index, value) {
		var stepBody = $(this);
		var hasNestedVisibleFormFields = stepBody
				.find(".form-group:not(.notrelevant)").length > 0;
		toggleStepVisibility(index, hasNestedVisibleFormFields);
	});
	
	if (DEBUG) {
		log("input fields state updated successfully");
	}
};

var updateFieldStateCache = function(inputFieldInfoByParameterName) {
	$.each(inputFieldInfoByParameterName, function(fieldName, info) {
		stateByInputFieldName[fieldName] = info;
	});
};

var getStepHeading = function(index) {
	var stepHeading = $form.find(".steps .steps ul li").eq(index);
	return stepHeading;
};

var toggleStepVisibility = function(index, visible) {
	var stepBody = $form.find(".step").eq(index);
	var stepHeading = getStepHeading(index);
	if (visible) {
		if (stepHeading.hasClass("notrelevant")) {
			stepHeading.removeClass("notrelevant");
			if (stepHeading.hasClass("done")) {
				stepHeading.removeClass("disabled");
			}
		}
	} else {
		stepHeading.addClass("disabled notrelevant");
	}
	stepHeading.toggle(visible);
	
	if (! stepHeading.hasClass("current")) {
		stepBody.hide();
	}
};

var showCurrentStep = function() {
	if (currentStepIndex != null && currentStepIndex > 0) {
		var stepHeading = getStepHeading(currentStepIndex);
		var relevant = ! stepHeading.hasClass("notrelevant");
		if (relevant) {
			$stepsContainer.steps("setCurrentIndex", currentStepIndex);
		}
	}
};

var updateStepsErrorFeedback = function() {
	$form.find(".step").each(function(index, value) {
		var stepHeading = getStepHeading(index);
		if (! stepHeading.hasClass("disabled")) {
			var hasErrors = $(this).find(".form-group.has-error").length > 0;
			stepHeading.toggleClass("error", hasErrors);
		}
	});
};

var initCodeButtonGroups = function() {
	$form.find("button.code-item").click(function(event) {
		event.preventDefault();
		// update hidden input field
		var btn = $(this);
		var value = null;
		var wasActive = btn.hasClass("active");
		var itemsContainer = btn.closest(".code-items");
		var groupContainer = itemsContainer.closest(".code-items-group");
		var inputField = groupContainer.find("input[type='hidden']");
				
		if (itemsContainer.data("toggle") == "buttons") {
			if (! wasActive) {
				if (btn.val() == "none") {
					//remove the other active buttons
					// deselect all code item buttons
					groupContainer.find(".code-item").removeClass('active');
				} else {
					// If none was selected and a value different than none was selected
					var activeNoneButton = itemsContainer.find("button[value='none'].active");
					activeNoneButton.removeClass('active');
				}
			} else if (btn.val() != "none") {
				// Check that if there are no values selected then none is selected!
				var buttons = itemsContainer.find("button.active");
				if(buttons.length == 1 ) { // Only the current button, which will be deselected, is selected now
					var noneButton = itemsContainer.find("button[value='none']");
					if( noneButton && !noneButton.hasClass('active') ){
						noneButton.addClass('active');
					}
				}
			}
			// multiple selection
			var buttons = itemsContainer.find("button.active");
			var valueParts = [];
			buttons.each(function() {
				var activeButton = $(this);
				var activeButtonValue = activeButton.val();
				var clickedButtonValue = btn.val();
				if(clickedButtonValue !== activeButtonValue){ // Do not add the button clicked here, do it on the next block so we check that it was activated
					valueParts.push(activeButtonValue);
				}
			});
			if( !wasActive) { // means that the button was not active and will be activated
				valueParts.push(btn.val());
			}
			
			value = valueParts.join(SEPARATOR_MULTIPLE_PARAMETERS);
		} else {
			// single selection
			value = btn.val();
		}
		inputField.val(value);
		if (! wasActive) {
			btn.toggleClass("active", true);
		}
		
		updateData(inputField);
		
		return false;
	});
};

var initBooleanButtons = function() {
	$('.boolean-group').each(function() {
		var group = $(this);
		var hiddenField = group.find("input[type='hidden']");
		group.find("button").click(function() {
			var btn = $(this);
			hiddenField.val(btn.val());
			var wasSelected = btn.hasClass('active');
			group.find('button').removeClass('active');
			if (! wasSelected) {
				btn.addClass('active');
			}
			updateData(hiddenField);
			return false;
		});
	});
};

var initDateTimePickers = function() {
	// http://eonasdan.github.io/bootstrap-datetimepicker/
	$('.datepicker').datetimepicker({
		format : DATE_FORMAT
	}).on('dp.change', function(e) {
		var inputField = $(this).find(".form-control");
		// inputField.change();
		updateData(inputField);
	});

	$('.timepicker').datetimepicker({
		format : TIME_FORMAT
	}).on('dp.change', function(e) {
		var inputField = $(this).find(".form-control");
		updateData(inputField);
	});
};

var initSteps = function() {
	$steps = $stepsContainer.steps({
		headerTag : "h3",
		bodyTag : "section",
		transitionEffect : "none",
		autoFocus : true,
		titleTemplate : "#title#",
		labels : {
			// These values come from the balloon.html file as they need to be localized (spanish,english,portuguese and french)
			finish : SUBMIT_LABEL,
		    next: NEXT_LABEL,
		    previous: PREVIOUS_LABEL
		},
		onStepChanged : function(event, currentIndex, priorIndex) {
			var stepHeadings = $form.find(".steps .steps ul li");
			var stepHeading = $(stepHeadings[currentIndex]);
			if (stepHeading.hasClass("notrelevant")) {
				var nextStepIndex;
				var firstRelevantHeadingIdx = findFirstRelevantElementIndex(stepHeadings, currentIndex, currentIndex < priorIndex);
				if (firstRelevantHeadingIdx >= 0) {
					nextStepIndex = firstRelevantHeadingIdx;
				} else {
					//show last card
					nextStepIndex = stepHeadings.length - 1;
				}
				$stepsContainer.steps('setCurrentIndex', nextStepIndex);
			}
			updateStepsErrorFeedback();
		},
		onFinished : function(event, currentIndex) {
			submitData();
		}
	});
	$stepsContainer.find("a[href='#finish']").addClass("btn-finish");
};

var findFirstRelevantElementIndex = function(group, startFromIndex, reverseOrder) {
	var idx = reverseOrder ? startFromIndex - 1 : startFromIndex + 1;
	while (reverseOrder ? idx >= 0 : idx < group.length) {
		var el = $(group[idx]);
		if (! el.hasClass("notrelevant")) {
			return idx;
		}
		idx = reverseOrder ? idx - 1 : idx + 1;
	}
	return -1;
};

var checkIfPlacemarkAlreadyFilled = function(checkCount) {
	checkCount = defaultIfNull(checkCount, 0);
	
	var placemarkId = getPlacemarkId();

	$.ajax({data : {id : placemarkId},
		type : "GET",
		url : HOST + "placemark-info-expanded",
		dataType : 'json',
		timeout : 2000
	})
	.fail(function(xhr, textStatus, errorThrown) {
		if (isValidResponse(xhr.responseText)) {
			//valid response but for some (unknown) reason not handled properly by jquery
			handleValidResponse($.parseJSON(xhr.responseText));
		} else if (checkCount < 5) {
			// try again
			checkIfPlacemarkAlreadyFilled(checkCount + 1);
		} else {
			changeState(COLLECT_EARTH_NOT_RUNNING_STATE);
		}
	})
	.done(function(json) {
		handleValidResponse(json);
	});
	
	function handleValidResponse(json) {
		if (json.success) {
			// placemark exists in database
			if (json.activelySaved
					&& json.inputFieldInfoByParameterName.collect_text_id.value != 'testPlacemark') {
	
				showErrorMessage(PLACEMARK_ALREADY_FILLED);
	
				if (json.skipFilled) {
					forceWindowCloseAfterDialogCloses($("#dialogSuccess"));
				}
			}
			// Pre-fills the form and after that initilizes the
			// change event listeners for the inputs
			interpretJsonSaveResponse(json, false);
				
			currentStepIndex = json.currentStep == null ? null
					: parseInt(json.currentStep);
			showCurrentStep();
			updateStepsErrorFeedback();

			changeState(DEFAULT_STATE);
		} else {
			// if no placemark in database, force the creation
			// of a new record
			sendCreateNewRecordRequest();
		}
	}
};

var getPlacemarkId = function() {
	var id = findById("collect_text_id").val();
	return id;
};

var isActivelySaved = function() {
	var activelySaved = findById(ACTIVELY_SAVED_FIELD_ID).val() == 'true';
	return activelySaved;
};

var setActivelySaved = function(value) {
	findById(ACTIVELY_SAVED_FIELD_ID).val(value == true || value == 'true');
};

var showSuccessMessage = function(message) {
	showMessage(message, "success");
};

var showWarningMessage = function(message) {
	showMessage(message, "warning");
};

var showErrorMessage = function(message) {
	showMessage(message, "error");
};

var showMessage = function(message, type) {
	var color;
	switch (type) {
	case "error":
		color = "red";
		break;
	case "warning":
		color = "yellow";
		break;
	case "success":
	default:
		color = "green";
	}
	$('#succ_mess').css("color", color).html(message ? message : "");
	$("#dialogSuccess").dialog("open");
};

var fillDataWithJson = function(inputFieldInfoByParameterName) {
	if (DEBUG) {
		log("setting values in input fields...");
	}

	$.each(inputFieldInfoByParameterName, function(key, info) {
		var value = info.value;
		// Do this for every key there might be different
		// type of elements with the same key than a hidden
		// input

		// var values = value == null ? []:
		// value.split(SEPARATOR_MULTIPLE_VALUES);// In
		// case
		// of
		// value
		// being
		// 0;collect_code_deforestation_reason=burnt
		var inputField = findById(key);
		if (inputField.length == 1) {
			setValueInInputField(inputField, value);
		}
	});
	if (DEBUG) {
		log("values set in input fields");
	}
}

var setValueInInputField = function(inputField, value) {
	var tagName = inputField.prop("tagName");
	switch (tagName) {
	case "INPUT":
		if (inputField.val() != value) {
			inputField.val(value);
		}
		switch (inputField.data("fieldType")) {
		case "BOOLEAN":
			// if (inputField.prop("type") == "checkbox") {
			// inputField.prop("checked", value == "true");
			// }
			var group = inputField.closest(".boolean-group");
			group.find("button").removeClass('active');
			if (value != null && value != "") {
				group.find("button[value='" + value + "']").addClass('active');
			}
			break;
		case "CODE_BUTTON_GROUP":
			var itemsGroup = inputField.closest(".code-items-group");
			// deselect all code item buttons
			itemsGroup.find(".code-item").removeClass('active');
			if (value != null && value != "") {
				// select code item button with value equals to the specified one
				var codeItemsContainers = itemsGroup.find(".code-items");
				var activeCodeItemsContainer = getVisibleComponent(codeItemsContainers);
				var splitted = value.split(SEPARATOR_MULTIPLE_PARAMETERS);
				if (activeCodeItemsContainer != null) {
					splitted.forEach(function(value, index) {
						var button = activeCodeItemsContainer.find(".code-item[value='" + escapeRegExp(value) + "']");
						button.addClass('active');
					});
				}
			}
			break;
		}
		break;
	case "TEXTAREA":
		inputField.val(value);
		break;
	case "SELECT":
		inputField.val(value);
		if (inputField.val() == null) {
			inputField.val("-1");
		}
		break;
	}
};

function getVisibleComponent(components) {
	if (components.length == 1) {
		return $(components[0]);
	}
	for (i = 0; i < components.length; i++) { 
		var component = $(components[i]);
		if (component.css("display")=="block") {
			return component;
		}
	}
	return null;
}

function escapeRegExp(string){
	return string.replace(/([.*+?^=!:${}()|\[\]\/\\])/g, "\\$1");
}

var initializeChangeEventSaver = function() {
	// SAVING DATA WHEN DATA CHANGES
	// Bind event to Before user leaves page with function parameter e
	// The window onbeforeunload or onunload events do not work in Google Earth
	// OBS! The change event is not fired for the hidden inputs when the value
	// is updated through jQuery's val()
	$('input[name^=collect], textarea[name^=collect], select[name^=collect], select[name^=hidden], button[name^=collect]').change(function(e) {
		updateData(e.target);
	});
	$('input:text[name^=collect], textarea[name^=collect]').keyup(function(e) {
		updateData(e.target, 1500);
	});
};

var fillYears = function() {
	for (var year = new Date().getFullYear(); year > 1980; year--) {
		$('.fillYears').each(function() {
			$(this).append(
				$("<option></option>")
					.attr("value", year)
					.text(year));
		});
	}
};

var forceWindowCloseAfterDialogCloses = function($dialog) {
	$dialog.on("dialogclose", function(event, ui) {
		window.open("#" + NEXT_ID + ";flyto"); 	// balloonFlyto - annoying to have
												// the balloon open, doesn't let you
												// see the plot
	});
};

var changeState = function(state) {
	switch(state) {
		case LOADING_STATE:
			$("#formAll").hide();
			$("#collectEarthNotRunningPanel").hide();
			$("#loadingPanel").show();
			break;
		case COLLECT_EARTH_NOT_RUNNING_STATE:
			$("#loadingPanel").hide();
			$("#formAll").hide();
			$("#collectEarthNotRunningPanel").show();
			break;
		default:
			$("#loadingPanel").hide();
			$("#collectEarthNotRunningPanel").hide();
			$("#formAll").show();
	}
}

/**
 * Utility functions
 */
var serializeFormToJSON = function(form) {
   var o = {};
   var a = form.serializeArray();
   $.each(a, function() {
	   var key = encodeURIComponent(this.name);
	   var value = this.value || '';
       if (o[key]) {
           if (!o[key].push) {
               o[key] = [o[key]];
           }
           o[key].push(value);
       } else {
           o[key] = value;
       }
   });
   //include unchecked checkboxes
   form.find('input[type=checkbox]:not(:checked)').each(function() {
	   o[this.name] = this.checked;  
   });
   return o;
};

/*
var serializeForm = function(formId) {
	var form = findById(formId);
	var result = form.serialize();
	form.find('input[type=checkbox]:not(:checked)').each(function() {
		result += "&" + this.name + "=" + this.checked;
	});
	return result;
};
*/

var enableSelect = function(selectName, enable) { // #elementsCover
	$(selectName).prop('disabled', !enable);
	// $(selectName).selectpicker('refresh');
};

var findById = function(id) {
	var newId = id.replace(/(:|\.|\[|\]|,)/g, "\\$1");
	return $("#" + newId);
};

var initLogConsole = function() {
	consoleBox = $("<div>");
	consoleBox.css("overflow", "auto");
	consoleBox.css("height", "100px");
	consoleBox.css("width", "400px");
	$("body").append(consoleBox);
};

var log = function(message) {
	var oldContent = consoleBox.html();
	var date = new Date().toJSON();
	var newContent = oldContent + "<br/>" + date + " - " + message;
	newContent = limitString(newContent, 2000);
	consoleBox.html(newContent);
	consoleBox.scrollTop(consoleBox[0].scrollHeight);
}

var logError = function(message) {
	if (consoleBox == null) {
		initLogConsole();
	}
	log(message);
};

var limitString = function(str, limit) {
	var length = str.length;
	if (length > limit) {
		return str.substring(length - limit);
	} else {
		return str;
	}
};

var defaultIfNull = function(obj, defaultValue) {
	if (typeof obj == "undefined" || obj == null) {
		return defaultValue;
	} else {
		return obj;
	}
};
