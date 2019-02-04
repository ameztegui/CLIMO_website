#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(stringr)
library(htmltools)
library(forcats)
library(leaflet)
library(readr)
library(scales)


# Load the dataset
load("./data/climo_data.Rdata")


all_species <- c("All",sort(unique(str_extract(clean_data$Species, "[aA-zZ]+[[:space:]]+[aA-zZ]+"))))
all_data <- sort(unique(unlist(strsplit(as.character(clean_data$Data),split = ";."))))
all_countries <- c("All",sort(unique(clean_data$Country)))

# User Interface ----------------------------------------------------------

PAGE_TITLE = "CLIMo: Climate-smart forestry in mountain regions"
# Define UI for application that draws a histogram
ui <- navbarPage(
           titlePanel(windowTitle = PAGE_TITLE,
                      title = div(img(src = "cropped-Untitled-1.png", 
                                      height = 60,width = 60,
                                      style = "margin: -50px 0px 0px 0px"
                                      ),
           div(PAGE_TITLE, style = "padding: 0px 0px 0px 0px ; margin: -50px 0px 0px 90px")
         )
       ),
           
       

  ## First tab: Interactive map             
  tabPanel("Interactive Map", 
    sidebarLayout(
      sidebarPanel(width= 2,
                   h4("Select input data"),
                   selectInput("Country", label = "Country",
                               choices = all_countries),
                   selectInput("Species", label = "Species",
                               choices = "", selected = ""),
                   selectInput("Data_menu", label = "Data available",
                               choices = "", selected = ""),
                   # h4("Available data:"),
                   # textOutput(outputId="Data"),
                   plotOutput(outputId = "climate")
                   ),

      mainPanel(width = 10,
                leafletOutput(outputId = "mymap", height =800)
                # DT::dataTableOutput("mytable")
                )
      )
    ),
  
  tabPanel("Explore the data available", 
    div(DT::dataTableOutput("exttable"), style = "font-size:90%")),
  
  tabPanel("Some statistics", 
    fluidPage(
      fluidRow(
        column(width = 2, 
               h4("Select input data"),
               selectInput("Country2", label = "Country",
                           choices = all_countries)),
        column(width = 5, plotOutput(outputId = "species_dist")),
        column(width = 5, plotOutput(outputId = "elev_country_hist"))),
      fluidRow(
        column(width = 2, 
               h4("Select input data"),
               selectInput("Species2", label = "Species",
                           choices = all_species)),
        column(width = 5, plotOutput(outputId = "countries_dist")),
        column(width = 5, plotOutput(outputId = "elev_species_hist"))
      )
    )
  )
)


   # # Application title
   # h3("Long-term research plots established in Europe for the study of mountain forest dynamics and management"),

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  ## Define the input options to be visible
      observeEvent(input$Country,
                 updateSelectInput(session, "Species", "Species",
                                   choices = c("All",unique(clean_data %>%
                                       filter(if (input$Country == "All") Country %in% all_countries else Country == input$Country) %>%
                                       arrange(Species) %>%
                                       pull(Species) %>%
                                       str_extract("[aA-zZ]+[[:space:]]+[aA-zZ]+")))))
  
       observeEvent(input$Country,
                    updateSelectInput(session, inputId = "Data_menu", "Data available",
                                   choices = c("All",unique(clean_data %>%
                                       filter(if (input$Country == "All") Country %in% all_countries else Country == input$Country) %>%
                                         pull(Data) %>%
                                         strsplit(split = ";.") %>%
                                         unlist()))))
      
       observeEvent(input$Species,
                    updateSelectInput(session, inputId = "Data_menu", "Data available",
                                   choices = c("All",unique(clean_data %>%
                                        filter(if (input$Species == "All") Species != "All" else grepl(input$Species,Species)) %>%
                                pull(Data) %>%
                                  strsplit(split = ";.") %>%
                                  unlist()))))
  
  
  ## Define the data to be used depending on the filters defined by user (input menus)
 filteredData <- reactive({
   clean_data %>%
     filter(if (input$Country == "All") Country %in% all_countries else Country == input$Country,
            if (input$Species == "All") Species != "All" else grepl(input$Species,Species),
            if (input$Data_menu == "All") Data != "All" else grepl(input$Data_menu,Data))
 })
 

output$Data <- renderText({ 
  if(input$Country == "All" & input$Species == "All") {
    "Select a country or species to see the available data" 
  } else {
    paste0(unique(filteredData() %>%
             pull(Data) %>%
             strsplit(split = ";.") %>%
             unlist()),";")
  }
  })

output$histo <- renderPlot({
  filteredData() %>% 
    ggplot()+
    geom_histogram(aes(Elevation))
    
})


output$climate <- renderPlot({
    clean_data %>% 
        ggplot() +
        geom_point(aes(x=Temp, y=Prec), color="steelblue", alpha = 0.7, size =3.5) +
        geom_point(data=filteredData(), aes(x=Temp, y=Prec), color="red", size=5, alpha = 0.8) +
        scale_y_continuous(breaks= pretty_breaks()) +
        ylab("Precipitation") + xlab("Temperature") +
        theme_light() + 
        theme(axis.text = element_text(size=10),
              axis.title = element_text(size=12),
              panel.grid.major.y = element_blank(),
              panel.border = element_blank(),
              axis.ticks.y = element_blank())
}) 


output$mymap <- renderLeaflet({
    leaflet(clean_data) %>%
      addProviderTiles(providers$Stamen.Terrain) %>%
      setView(lat = 55, 
              lng = 8,
              zoom = 4) %>%
       addCircles(color = "steelblue",group = "Plots", fillOpacity = 0.7)
      
  })


  observe({
      leafletProxy("mymap" ) %>% clearMarkers() %>% clearPopups() %>%
       addCircleMarkers(data = filteredData(),radius = 3.5, 
                        color = "red",fillOpacity = 0.8, group = "Plots",
                        popup = ~paste0(
                 "<b>Country: </b>",filteredData()$Country,"</br>",
                 "<b>Species: </b>", "<i>",filteredData()$Species,"</i>", "</br>",
                "<b>Data: </b>", filteredData()$Data, "</br>",
                "<b>Elevation: </b>", filteredData()$Elevation," m a.s.l.","</br>",
                 "<b>Forest Type: </b>",filteredData()$Structure," (",filteredData()$Age," yr.)","</br>",
                 "<b>Institution: </b>",'<a href="',filteredData()$Website, '">',
                filteredData()$Organisation,"</a>","</br>",
                 "<b>Responsible: </b>",filteredData()$Responsible,' (<a href="mailto:',filteredData()$Mail, '">',
                 filteredData()$Mail,"</a>)")) %>% 
      flyToBounds(min(filteredData()$Longitude,na.rm=T),min(filteredData()$Latitude,na.rm=T),
                max(filteredData()$Longitude,na.rm=T),max(filteredData()$Latitude,na.rm=T),
                options=list(maxZoom = 6))
  })
  
  output$mytable = DT::renderDataTable({
      filteredData() %>%
      select(Country, Species, Organisation, Responsible)
      }, escape = FALSE)
    
  output$exttable = DT::renderDataTable({
      filteredData() %>%
          mutate(Responsible = paste0(Responsible,' (<a href="mailto:',Mail,'">',
             Mail,"</a>)")) %>%
      dplyr::select(Country, Name, Year, Species, Temp, Prec, Structure, "Stand Age" = Age, Elevation, 
             Data, Responsible, Organisation)
  }, escape = FALSE, options = list(pageLength = 50))
  
############## Third panel ###

  filteredCountry <- reactive({
  if (input$Country2 == "All") {
    clean_data 
  } else {
      clean_data %>%
        filter(Country == input$Country2)}
  })
  
  filteredSpecies <- reactive({
  if (input$Species2 == "All") {
    clean_data 
  } else {
      clean_data %>%
        filter(grepl(input$Species2,Species))}
  })
  
  output$species_dist <- renderPlot({
      hey <- data.frame(Species=factor(filteredCountry() %>%
          pull(Species) %>%
          strsplit(split = ";.") %>%
          unlist()))
      
      hey %>%
        count(Species) %>%
        arrange(n) %>%
        mutate(Species = factor(Species, Species)) %>%
        ggplot() +
        geom_segment(aes(x=Species, xend=Species, y=0, yend=n), color="grey") +
        geom_point( aes(x=Species, y = n), color="steelblue", size=4) +
    # geom_bar(aes(x = Species), stat= "count", fill="steelblue") +
        scale_y_continuous(breaks= pretty_breaks()) +
        ylab("Number of plots") +
        coord_flip( ) +
        theme_light() + 
        theme(axis.text = element_text(size=10),
                              axis.title = element_text(size=12),
              panel.grid.major.y = element_blank(),
              panel.border = element_blank(),
              axis.ticks.y = element_blank())
    })  
  
  output$countries_dist <- renderPlot({
      filteredSpecies() %>%
      count(Country) %>%
      arrange(n) %>%
      mutate(Country = factor(Country, Country)) %>%
      ggplot() +
      geom_segment(aes(x=Country, xend=Country, y=0, yend=n), color="grey") +
      geom_point( aes(x=Country, y = n), color="dark green", size=4) +
      #geom_bar(aes(x = Country), stat= "count", fill="dark green") +
      scale_y_continuous(breaks= pretty_breaks()) +
      ylab("Number of plots") +
      coord_flip( ) +
      theme_light() + 
      theme(axis.text = element_text(size=10),
            axis.title = element_text(size=12),
            panel.grid.major.y = element_blank(),
            panel.border = element_blank(),
            axis.ticks.y = element_blank())
    }) 
    
  output$elev_country_hist <- renderPlot({  
    filteredCountry() %>% 
      ggplot()+
      geom_histogram(aes(Elevation), fill= "steelblue") +
      theme_bw() +
      ylab("Number of plots") +
      theme(axis.text = element_text(size=10),
            axis.title = element_text(size=12))
      })
    
   output$elev_species_hist <- renderPlot({  
    filteredSpecies() %>% 
      ggplot()+
      geom_histogram(aes(Elevation), fill="dark green") +
       theme_bw() +
       ylab("Number of plots") +
       theme(axis.text = element_text(size=10),
             axis.title = element_text(size=12))
      }) 
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


