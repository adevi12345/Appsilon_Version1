library(shiny)
library(shinydashboard)
library(dplyr)
library(leaflet)
library(htmltools)
library(shinyjs)
library(readxl)

title<-tags$img(src="icon.png",height='50',width='150')
ui <- dashboardPage(

    dashboardHeader(title=title),
    dashboardSidebar(width = 400,

                     tags$br(),
                     uiOutput("selectize_nameui")
                     
    ),
    dashboardBody(
        includeCSS("www/main.css"),
        useShinyjs(),
        
        box(
            title = "Selected Species Observations on the map", width = 9, solidHeader = TRUE,status = "primary",
            uiOutput("plottext_map"),
            )
        )
)


server <- function(input, output,session) {
      values <- reactiveValues()
    
      values$input_dataset<-read_xlsx("D:/FL/Appsilon/Shiny_Developer/dummy_input.xlsx")
      disable("download.graphDPS")
  
    output$selectize_nameui<-renderUI({
      label<-tags$h4('Search with ScientificName:')
      selectInput('distinct_vars',label, choices = unique(values$input_dataset$`Scientific name`), multiple=TRUE)
    })
    
    
    output$plottext_map<-renderUI({
      
      if(is.null(input$distinct_vars )){
        infoBoxOutput("progressBox2")
      }else{
        enable("download.graphDPS")
        leafletOutput("species_map", height = "550px")
      }
      })
    
    output$progressBox2 <- renderInfoBox({
      infoBox(
        "Please Select Scientific Name", paste0("Enter your search to view the Observations on the Map "),icon = icon("map-marker"),
        color = "purple", fill = TRUE
      )
    })
     observeEvent(input$distinct_vars,{
        
       values$checkbox_values<-input$distinct_vars
       
        
        values$checkbox_species<-filter(values$input_dataset,`Scientific name` %in% values$checkbox_values)
        
      
    
          output$species_map <- renderLeaflet({
            
            m<-leaflet(values$checkbox_species) %>%
              addTiles() %>%
              addCircleMarkers(lng= ~log, lat= ~lal, label = ~htmlEscape(Species),
                               labelOptions = labelOptions(noHide = T, direction = "bottom",
                                                           style = list(
                                                             "color" = "black",
                                                             "font-family" = "serif",
                                                             "font-style" = "italic",
                                                             "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                             "font-size" = "12px",
                                                             "border-color" = "rgba(0,0,0,0.5)"
                                                           ))
              )
            
            
            m %>%addMiniMap(
              tiles = providers$Esri.WorldStreetMap,
              toggleDisplay = TRUE)
            })
         
         
          })

      
    }
shinyApp(ui, server)
