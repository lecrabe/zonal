####################################################################################
####### Zonal statistics
####### SEPAL Branch
####### FAO Open Foris SEPAL project
####### remi.dannunzio@fao.org
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
## zonal_stats / server
####################################################################################


####################################################################################
####### Start Server

shinyServer(function(input, output, session) {
  ####################################################################################
  ##################### Choose language option             ###########################
  ####################################################################################
  output$chosen_language <- renderPrint({
    if (input$language == "English") {
      source("text_english.R",
             local = TRUE,
             encoding = "UTF-8")
      #print("en")
    }
    if (input$language == "Français") {
      source("text_french.R", local = TRUE, encoding = "UTF-8")
      #print("fr")
    }
    if (input$language == "Español") {
      source("text_spanish.R",
             local = TRUE,
             encoding = "UTF-8")
      #print("sp")
    }
  })
  
  ##################################################################################################################################
  ############### Stop session when browser is exited
  
  session$onSessionEnded(stopApp)
  
  ##################################################################################################################################
  ############### Show progress bar while loading everything
  
  progress <- shiny::Progress$new()
  progress$set(message = "Loading maps/data", value = 0)
  
  ####################################################################################
  ####### Step 0 : read the map file and store filepath    ###########################
  ####################################################################################
  
  ##################################################################################################################################
  ############### Find volumes
  osSystem <- Sys.info()["sysname"]
  
  volumes <- list()
  media <- list.files("/media", full.names = T)
  names(media) = basename(media)
  volumes <- c(media)
  
  volumes <- c('Home' = Sys.getenv("HOME"),
               volumes)
  
  my_zip_tools <- Sys.getenv("R_ZIPCMD", "zip")
  
  
  
  ##################################################################################################################################
  ############### Select input file (raster OR vector)
  shinyFileChoose(
    input,
    'map_file',
    filetype = c(
      'tif',
      'img',
      'pix',
      'rst',
      'jpeg2000',
      'grd',
      'vrt',
      'hdf'
    ),
    roots = volumes,
    session = session,
    restrictions = system.file(package = 'base')
  )
  
  ################################# Display the file path
  output$map_file_path <- renderPrint({
    validate(need(input$map_file, "Missing input: Please select the map file"))
    df <- parseFilePaths(volumes, input$map_file)
    file_path <- as.character(df[, "datapath"])
    nofile <- as.character("No file selected")
    if (is.null(file_path)) {
      cat(nofile)
    } else{
      cat(file_path)
    }
  })
  
  ##################################################################################################################################
  ############### Select input file (raster OR vector)
  shinyFileChoose(
    input,
    'zone_file',
    filetype = c(
      'shp',
      'sqlite',
      'gdb'
    ),
    roots = volumes,
    session = session,
    restrictions = system.file(package = 'base')
  )
  
  ################################# Display the file path
  output$zone_file_path <- renderPrint({
    validate(need(input$zone_file, "Missing input: Please select the zone file"))
    df <- parseFilePaths(volumes, input$zone_file)
    file_path <- as.character(df[, "datapath"])
    nofile <- as.character("No file selected")
    if (is.null(file_path)) {
      cat(nofile)
    } else{
      cat(file_path)
    }
  })
  
  ##################################################################################################################################
  ############### Find out map type and store the variable
  zoneType <- reactive({
    print('Check: zoneType')
    
    req(input$zone_file)
    raster_type <- c('tif', 'img', 'pix', 'rst', 'jpeg2000', 'grd', 'hdf','vrt')
    vector_type <- c('shp', 'sqlite','gdb')
    
    df <- parseFilePaths(volumes, input$zone_file)
    file_path <- as.character(df[, "datapath"])
    ending <- str_sub(file_path, -3)
    print(paste0('File extension is : ', ending))
    if (ending %in% raster_type) {
      zoneType <- 'raster_type'
    } else
      if (ending %in% vector_type) {
        zoneType <- 'vector_type'
      }
    zoneType
  })
  
  ################################# Output directory path
  outdir <- reactive({
    req(input$map_file)
    df <- parseFilePaths(volumes, input$map_file)
    file_path <- as.character(df[, "datapath"])
    dirn <- dirname(file_path)
    base <- substr(basename(file_path), 0, nchar(basename(file_path)) - 4)
    subDir <- paste0('zonal_', base)
    dir.create(file.path(dirn, subDir))
    paste0(dirn, '/', subDir)
  })
  
  ################################# Display output directory path
  output$outdirpath = renderPrint({
    outdir()
  })
  
  ################################# Display output directory path
  output$zoneTypeprint = renderPrint({
    zoneType()
  })
  
  ##################################################################################################################################
  ## Allow to download test data
  output$dynUI_download_test <- renderPrint({
    req(input$download_test_button)
    
    dir.create(file.path("~", "zonal_data_test"))
    
    if (osSystem == "Linux") {
      withProgress(message = paste0('Downloading data in ', dirname("~/zonal_data_test/")),
                   value = 0,
                   {
                     system("wget -O ~/zonal_data_test/zonal_test_map.tif https://github.com/openforis/data_test/raw/master/zonal_test_map.tif")
                     system("wget -O ~/zonal_data_test/zones_test.shp https://github.com/openforis/data_test/raw/master/zones_test.shp")
                     system("wget -O ~/zonal_data_test/zones_test.shx https://github.com/openforis/data_test/raw/master/zones_test.shx")
                     system("wget -O ~/zonal_data_test/zones_test.dbf https://github.com/openforis/data_test/raw/master/zones_test.dbf")
                     system("wget -O ~/zonal_data_test/zones_test.prj https://github.com/openforis/data_test/raw/master/zones_test.prj")
                     
                   })
    } else
      if (osSystem == "Windows") {
        # download.file("http://github.com/openforis/data_test/blob/master/aa_test_congo.tif?raw=true",
        #               "~/sae_data_test/test_map_congo.tif",
        #               "wininet")  ## THAT DOES NOT DOWNLOAD THE RASTER PROPERLY. BUT ALMOST
        # TBA
      }
    
    list.files("~/zonal_data_test/")
  })
  
  
  ##################################################################################################################################
  ############### Read the input raster map under reactive variable 'lcmap'
  lcmap <- reactive({
    print("Check: lcmap")
    req(input$map_file)
    withProgress(message = 'Reading the map file',
                 value = 0,
                 {
                   setProgress(value = .1)
                   df <- parseFilePaths(volumes, input$map_file)
                   file_path <- as.character(df[, "datapath"])
                   lcmap <- raster(file_path)
                 })
  })
  
  ##################################################################################################################################
  ############### Read the input raster or vector data under reactive variable 'lcmap'
  zones <- reactive({
    print("Check: zones")
    
    ############### Read the name chosen from dropdown menu
    ############### Load the raster corresponding to the selected name
    ## raster
    if (zoneType() == "raster_type") {
      req(input$zone_file)
      withProgress(message = 'Reading the zone file',
                   value = 0,
                   {
                     setProgress(value = .1)
                     df <- parseFilePaths(volumes, input$zone_file)
                     file_path <- as.character(df[, "datapath"])
                     zones <- raster(zone_file_path)
                   })
    } else{
      ## vector
      if (zoneType() == "vector_type") {
        req(input$zone_file)
        df <- parseFilePaths(volumes, input$zone_file)
        file_path <- as.character(df[, "datapath"])
        basen <-
          substr(basename(file_path), 0, nchar(basename(file_path)) - 4)
        direc <- dirname(file_path)
        
        withProgress(message = 'Reading the shapefile',
                     value = 0,
                     {
                       setProgress(value = .1)
                       zones <- readOGR(direc, basen)
                     })
      }
    }
  })
  
  
  ##################################################################################################################################
  ############### Read the attribute of the shapefile
  output$selectUI_attr_zones <- renderUI({
    req(zoneType() == "vector_type")
    shp <- zones()
    categories <- names(shp@data)
    
    selectizeInput(
      "attr_zones",
      label = h5(textOutput("field_zone_attr_value")),
      choices = categories,
      options = list(
        placeholder = '',
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
  })
  
  ##################################################################################################################################
  ############### Insert the start button
  output$StartZonal <- renderUI({
    req(input$attr_zones)
    req(input$map_file)
    actionButton('zonalStartButton', textOutput('zonal_button'))
  })
  
  ##################################################################################################################################
  ############### Create a legend for the zones
  legend <- reactive({
    req(zoneType() == "vector_type")
    req(input$zonalStartButton)
    
    zones    <- zones()
    attr_nme <- input$attr_zones
    
    legend   <- levels(as.factor(zones@data[,attr_nme]))
    legend   <- data.frame(cbind(legend,1:length(legend)))
    names(legend) <- c(attr_nme,"zonal_code")
    
    zones@data$sort_id <- row(zones@data)[,1]
    zones@data <- arrange(merge(zones@data,legend,all.x=T),sort_id)[,c(attr_nme,"zonal_code")]
    
    ras_proj <- proj4string(lcmap())
    shp_proj <- proj4string(zones)
    
    ######################### Project if necessary
    if(ras_proj != shp_proj){
      zones <- spTransform(zones,ras_proj)
      shp_proj <- proj4string(zones)
    }
    
    writeOGR(zones,paste0(outdir(),"/","zonal.shp"),"zonal","ESRI Shapefile",overwrite_layer = T)
    legend
  })
  
  
  ################################# Display output directory path
  output$show_table = renderDataTable({
    zonal()
  })
  
  ################################# Compute the zonal stats
  zonal <- reactive({
    req(legend())
    print("Check: zonal")
    
    legend   <- legend()
    zones    <- zones()
    attr_nme <- input$attr_zones
    
    df <- parseFilePaths(volumes, input$map_file)
    lcmap_name <- as.character(df[, "datapath"])
    
    ######################### Compute Zonal stats
    withProgress(message = 'Computing the zonal stats, be patient',
                 value = 0,
                 {
                   setProgress(value = .1)
                   
                   system(sprintf("python www/scripts/oft-zonal_large_list.py -i %s -um %s -o %s -a %s",
                                  lcmap_name,
                                  paste0(outdir(),"/","zonal.shp"),
                                  paste0(outdir(),"/","stats.txt"),
                                  "zonal_code"
                   ))
                 })
    
    ######################### Read Zonal stats
    df <- read.table(paste0(outdir(),"/","stats.txt"))
    names(df) <- c("zonal_code","total","no_data",paste0("map_",1:(ncol(df)-3)))
    df <- df[,colSums(df)!=0]
    
    df1 <- merge(df,legend)
    df2 <- df1[,c(ncol(df1),2:ncol(df))]
    df3 <- df2
    pix  <- res(lcmap())[1]
    
    df3[,2:ncol(df)] <- df2[,2:ncol(df)]*pix*pix
    write.csv(df3,paste0(outdir(),"zonal_stats.csv"),row.names = F)
    df3
  })
  
  
  
  ##################################################################################################################################
  ############### Enable to download the CE file (csv)
  output$ui_download_csv <- renderUI({
    req(zonal())
    downloadButton('download_csv',
                   label = textOutput('download_csv_button'))
  })
  
  ##################################################################################################################################
  ############### Enable to download the CE file (csv)
  output$download_csv <- downloadHandler(
    filename = function() {
      "zonal_stats.csv"
    },
    content  = function(xx) {
      to_export <- zonal()
      write.csv(to_export, xx, row.names = FALSE)
    }
  )
  
  ##################################################################################################################################
  ############### Turn off progress bar
  
  progress$close()
  ################## Stop the shiny server
  ####################################################################################
  
})
