library(shiny)
library(shinydashboard)
library(dplyr)
library(leaflet)
library(readxl)

ui <- dashboardPage(

    dashboardHeader(title="R Shiny - Biodiversity dashboard assignment (2021)"),
    dashboardSidebar(width = 400,
                     
                     fluidRow(
                         column(width = 7,
                                textInput(
                                    "search_name", "Search with Scientific Name", placeholder = "Search for Species"
                                )),
                         
                         column(width = 2,
                                tags$div(class="search_button",
                                         actionButton("searchbutton","Search"))
                         )
                         
                     ),
                     uiOutput("checkbox_ui")
    ),
    dashboardBody(
        includeCSS("www/main.css"),
        
        box(
            title = "Title 2", width = 6, solidHeader = TRUE,status = "primary",
            leafletOutput("species_map", height = "550px")   )
        )
)


server <- function(input, output,session) {
      values <- reactiveValues()
    
    values$input_dataset<-read_xlsx("D:/FL/Appsilon/Shiny_Developer/dummy_input.xlsx")
    observe({
        
        if( is.null(values$map_table )){
            return("Hi")
        }
    })
     observeEvent(input$searchbutton,{
        
        print(input$search_name)
         
        values$checkbox_species<-filter(values$input_dataset,`Scientific name` == input$search_name)

        output$checkbox_ui<-renderUI({
            checkboxInput("all_none","select ALL/None",value = TRUE)

            checkboxGroupInput("checkbox_speciesgroup","select one",choices =  values$checkbox_species$Species)
            
        })
       
    })
    
     
     
      # observe({


          
         
         # updateCheckboxGroupInput(
     #     session,'checkbox_species',choices = values$checkbox_species$Species,selected = if(input$all_none) values$checkbox_species$Species
     # )
     #     
      # })
    observeEvent(input$checkbox_speciesgroup,{
        
        values$checkbox_values<-input$checkbox_speciesgroup
        
        values$map_table<-filter(values$checkbox_species, Species %in%  values$checkbox_values)
        
        output$species_map <- renderLeaflet({
            
            leaflet(values$map_table) %>%
                addTiles() %>%
                addMarkers(lng= ~log, lat= ~lal, popup="Sample Place")
        })  
    })
      
    }
shinyApp(ui, server)