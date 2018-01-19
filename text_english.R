############################ Text boxes ENGLISH version

## t == TAB
## b == BOX
## p == PARAGRAPH
## w == WARNING


############################ TITLES
output$title    <- reactive({  "Zonal statistics" })

output$t0_title <- reactive({  "Zonal" })

output$source_code <- reactive({  "Source code" })
output$bug_reports <- reactive({  "Bug reports" })

############################ BUTTONS
output$download_testdata_button <- reactive({"Download test dataset"})
output$download_csv_button      <- reactive({'Download as tabular data (.csv)'})
output$zonal_button             <- reactive({'Launch zonal calculation'})
output$box_with_zones           <- reactive({"Do you want to compute areas by zones ?"})

############################ SERVER FIELDS
output$field_zone_attr_value   <- reactive({"Attribute column defining the zones"})

#################################################################################### 
############################ INTRODUCTION TAB
#################################################################################### 

############################ INTRODUCTION TAB - BOX 0
output$title_language <- reactive({"Language"})

############################ INTRODUCTION TAB - BOX 1
output$title_description <- reactive({"Description"})

output$body_description  <- reactive({
  HTML(paste0(
    "Compute zonal statistics of a raster over a given vector.
    <br/>
    <br/>
    Areas are computed in the projection system, so make sure the map is in an equal-area projection.
    <br/>
    Not projected maps will result in area computed in degrees 
    <br/>
    <br/>
    For support ask",a(href="http://www.openforis.org/support"," Open Foris support forum",target="_blank")
    ))})


output$title_download_testdata <- reactive({"Download test data"})

############################ INTRODUCTION TAB - BOX 2
output$title_map_input <- reactive({"Raster map input"})

output$body_map_input  <- reactive({
  HTML(paste0(
    "Choose the raster categorical map input: each class should be an integer"
    )
    )})

output$body_output_dir  <- reactive({
  HTML(paste0(
    "Output directory where the results are stored"
  )
  )})

############################ INTRODUCTION TAB - BOX 3
output$title_vector_input <- reactive({"Vector map input"})

output$body_vector_input  <- reactive({
  HTML(paste0(
    "Choose the vector zonal input: shapefile defining the zones"
  )
  )})

############################ INTRODUCTION TAB - BOX 5
output$title_zonal_result <- reactive({"Zonal table"})


############################ INTRODUCTION TAB - BOX 4
output$title_disclaimer <- reactive({"Disclaimer"})

output$body_disclaimer  <- reactive({
  HTML(paste0(
    "FAO declines all responsibility for errors or deficiencies in the database 
    or software or in the documentation accompanying it for program maintenance and 
    upgrading as well as for any damage that may arise from them.<br/>
    FAO also declines any responsibility for updating the data and assumes 
    no responsibility for errors and omissions in the data provided.<br/>
    Users are, however, kindly asked to report any errors or deficiencies in this product to FAO."
))})







