####################################################################################
#######          ZONAL STATISTICS                               ####################
#######    contributors:  Remi d'Annunzio                       ####################
#######              FAO Open Foris SEPAL project               ####################
#######    remi.dannunzio@fao.org | yelena.finegold@fao.org     ####################
####################################################################################

####################################################################################
# FAO declines all responsibility for errors or deficiencies in the database or 
# software or in the documentation accompanying it, for program maintenance and 
# upgrading as well as for any # damage that may arise from them. FAO also declines 
# any responsibility for updating the data and assumes no responsibility for errors 
# and omissions in the data provided. Users are, however, kindly asked to report any 
# errors or deficiencies in this product to FAO.
####################################################################################

####################################################################################
## Last update: 2018/01/18
## zonal_stats / ui
####################################################################################


print("Starting the process")

options(stringsAsFactors=FALSE)
options(shiny.launch.browser=T)

source("load_packages.R",echo = TRUE)



####################################################################################
####### Start User Interface

shinyUI(
  
  dashboardPage(
    skin='green',
    
    ####################################################################################
    #######       General title of the application            ##########################
    dashboardHeader(
      title= textOutput('title'),
      titleWidth = 350),
    
    ####################################################################################
    #######       Side Bar definition with all TABS           ##########################
    dashboardSidebar(
      width = 350,
      sidebarMenu(
        menuItem(textOutput('t0_title',inline=T), tabName = "Zonal_tab", icon = icon("dashboard")),
        hr(),
        br(),
        br(),
        menuItem(textOutput('source_code',inline=T), icon = icon("file-code-o"),href = "https://github.com/openforis/accuracy-assessment"),
        menuItem(textOutput('bug_reports',inline=T), icon = icon("bug")        ,href = "https://github.com/openforis/accuracy-assessment/issues")
      )
    ),
    
    ####################################################################################
    #######       Body structure of the Dashboard: tabItems   ##########################
    dashboardBody(
      tabItems(
        ####################################################################################
        # New Tab
        tabItem(tabName = "Zonal_tab",
                fluidRow(
                  # ####################################################################################
                  # Change style of the CSS style of the tabBox, making the color green
                  tags$style(".nav-tabs-custom .nav-tabs li.active {border-top-color: #00994d;}"),
                  
                  ## CSS format for errors, making the message in purple
                  tags$head(tags$style(HTML(".shiny-output-error-validation {color: #cc00ff;font-family:courier;font-size: 120%;}"))),
                  
                  ####################################################################################
                  # New box
                  box(
                    title= textOutput('t1_b0_title'), width=4,status = "success", solidHeader= TRUE,
                    selectInput(
                      'language','',choices = c("English","Français","Español")),
                    uiOutput("chosen_language")
                  ),
                  
                  ####################################################################################
                  # New box
                  box(
                    title= textOutput('t1_b1_title'), width=4,status = "success", solidHeader= TRUE,
                    htmlOutput('t1_b1_body')
                  ),
                  
                  ####################################################################################
                  # New box
                  box(
                    title= textOutput('t2_b2_title'), width=4,status = "success", solidHeader= TRUE,
                    actionButton("download_test_button",textOutput('download_testdata_button')),
                    uiOutput("dynUI_download_test")
                  )
                  
                ),
                ####################################################################################
                # End of the fluid row
                
                fluidRow(
                  ####################################################################################
                  # New box
                  box(title= textOutput('t1_b2_title'),width=6, status = "success", solidHeader= TRUE,
                      htmlOutput('t1_b2_body'),
                      shinyFilesButton(id = 'map_file',
                                       label = "Input thematic map (raster)",  #htmlOutput('t2_b1_button'), TO TRY TO IMPLEMENT
                                       title = "Browse", #htmlOutput('select_a_file'),
                                       multiple = FALSE),
                      br(),
                      textOutput("map_file_path"),
                      br(),
                      htmlOutput('t1_b5_body'),
                      br(),
                      textOutput("outdirpath")
                  ),
                  
                  ####################################################################################
                  # New box
                  box(title= textOutput('t1_b3_title'),width=6, status = "success", solidHeader= TRUE,
                      htmlOutput('t1_b3_body'),
                      shinyFilesButton(id = 'zone_file',
                                       label = "Input zones (vector or raster)",  #htmlOutput('t2_b1_button'), TO TRY TO IMPLEMENT
                                       title = "Browse", #htmlOutput('select_a_file'),
                                       multiple = FALSE),
                      br(),
                      textOutput("zone_file_path"),
                      uiOutput("selectUI_attr_zones")
                  )
                  
                ),
                ####################################################################################
                # End of the fluid row
                
                fluidRow(
                  ####################################################################################
                  # New box
                  box(title=textOutput('t1_b5_title'),width=12,status = "success", solidHeader= TRUE,
                      uiOutput("StartZonal"),
                      dataTableOutput("show_table"),
                      uiOutput("ui_download_csv")
                  )
                  ####################################################################################
                  # End of the Box
                  
                ),
                ####################################################################################
                # End of the fluid row
                
                fluidRow(
                  ####################################################################################
                  # New box
                  box(title=textOutput('t1_b4_p1_title'),width=12,status = "success", solidHeader= TRUE,
                      br(),
                      htmlOutput('t1_b4_p1_body'),
                      br(),
                      br(),
                      img(src="thumbnails/sepal-logo-EN-white.jpg", height = 100, width = 210),
                      img(src="thumbnails/UNREDD_LOGO_COLOUR.jpg",  height = 80,  width = 100),
                      img(src="thumbnails/Open-foris-Logo160.jpg",  height = 70,  width = 70),
                      br()
                  )
                  ####################################################################################
                  # End of the Box
                  
                )
                ####################################################################################
                # End of the fluid row
                
        )
        ####################################################################################
        # End of the tabItem 
        
      )
      ####################################################################################
      # End of the tabItem list
      
    )
    ####################################################################################
    # End of the Dashboard Body
    
  )
  ####################################################################################
  # End of the Dashboard Page 
  
)
####################################################################################
# End of the User Interface