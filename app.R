library(shiny)
library(rgee)
library(leaflet)
library(sf)
library(terra)

# Define UI
ui <- fluidPage(
  titlePanel("Manual Remote Sensing Data Downloader"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Earth Engine Initialization"),
      textInput("email", "Enter Email for Earth Engine Initialization:", 
                placeholder = "Example: your-email@gmail.com"),
      actionButton("init_gee", "Initialize Earth Engine"),
      verbatimTextOutput("init_status"),
      
      h3("Enter Dataset Information"),
      textInput("data_type", "Enter GEE Dataset ID:", 
                placeholder = "Example: LANDSAT/LC09/C02/T1_L2"),
      
      tags$a(href = "https://developers.google.com/earth-engine/datasets/catalog?hl=en", 
             "Browse GEE Datasets Catalog", target = "_blank"),
      
      textInput("bands", "Enter Bands (comma-separated):", 
                placeholder = "Example: SR_B2,SR_B3,SR_B4"),
      
      dateRangeInput(
        "date_range", 
        "Select Date Range:", 
        start = "2023-01-01", 
        end = Sys.Date()
      ),
      
      numericInput("scale", "Scale (meters per pixel):", value = 30, min = 10, step = 10),
      
      h3("Manual ROI Input"),
      numericInput("roi_min_lon", "Min Longitude:", value = -121.9),
      numericInput("roi_min_lat", "Min Latitude:", value = 38.0),
      numericInput("roi_max_lon", "Max Longitude:", value = -121.5),
      numericInput("roi_max_lat", "Max Latitude:", value = 38.3),
      
      actionButton("update_map", "Visualize ROI on Map"),
      actionButton("download", "Download and Save Locally"),
      verbatimTextOutput("status")
    ),
    
    mainPanel(
      h3("ROI Visualization"),
      leafletOutput("map")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Initialize Earth Engine
  observeEvent(input$init_gee, {
    req(input$email)
    tryCatch({
      ee_Initialize(email = input$email, drive = TRUE)
      output$init_status <- renderText("Earth Engine successfully initialized!")
      showNotification("Earth Engine successfully initialized!", type = "message")
    }, error = function(e) {
      output$init_status <- renderText(paste("Initialization failed:", e$message))
      showNotification("Failed to initialize Earth Engine.", type = "error")
    })
  })
  
  # Visualize ROI
  observeEvent(input$update_map, {
    bbox_coords <- rbind(
      c(input$roi_min_lon, input$roi_min_lat),
      c(input$roi_max_lon, input$roi_min_lat),
      c(input$roi_max_lon, input$roi_max_lat),
      c(input$roi_min_lon, input$roi_max_lat),
      c(input$roi_min_lon, input$roi_min_lat)
    )
    
    bbox_polygon <- st_polygon(list(bbox_coords))
    bbox_sf <- st_sf(geometry = st_sfc(bbox_polygon, crs = 4326))
    
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addPolygons(data = bbox_sf, color = "blue", fillOpacity = 0.2, weight = 2) %>%
        setView(
          lng = mean(c(input$roi_min_lon, input$roi_max_lon)),
          lat = mean(c(input$roi_min_lat, input$roi_max_lat)),
          zoom = 10
        )
    })
    
    showNotification("ROI updated on map!", type = "message")
  })
  
  # Download and Save Locally
  observeEvent(input$download, {
    req(input$data_type, input$date_range, input$bands)
    output$status <- renderText("Starting download...")
    
    tryCatch({
      start_date <- as.character(input$date_range[1])
      end_date <- as.character(input$date_range[2])
      selected_bands <- unlist(strsplit(input$bands, ","))
      scale <- input$scale
      
      # Define the ROI
      bbox_coords <- rbind(
        c(input$roi_min_lon, input$roi_min_lat),
        c(input$roi_max_lon, input$roi_min_lat),
        c(input$roi_max_lon, input$roi_max_lat),
        c(input$roi_min_lon, input$roi_max_lat),
        c(input$roi_min_lon, input$roi_min_lat)
      )
      
      bbox_polygon <- st_polygon(list(bbox_coords))
      bbox_sf <- st_sf(geometry = st_sfc(bbox_polygon, crs = 4326))
      roi_ee <- sf_as_ee(bbox_sf)$geometry()$bounds()
      
      # Fetch Data from GEE
      collection <- ee$ImageCollection(input$data_type)$
        filterDate(start_date, end_date)$
        filterBounds(roi_ee)
      
      if (collection$size()$getInfo() == 0) {
        stop("No valid images found for the selected dataset, ROI, and date range.")
      }
      
      image <- collection$median()$select(selected_bands)
      raster_file <- ee_as_rast(
        image = image$clip(roi_ee),
        region = roi_ee,
        scale = scale,
        via = "drive"
      )
      
      # Save File Locally
      local_file <- file.path(getwd(), "downloaded_image.tif")
      writeRaster(raster_file, filename = local_file, overwrite = TRUE)
      
      output$status <- renderText(paste("Download complete! File saved at:", local_file))
      showNotification(paste("Download complete! File saved at:", local_file), type = "message")
      
    }, error = function(e) {
      output$status <- renderText(paste("Error during download:", e$message))
      showNotification("Error during download.", type = "error")
    })
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
