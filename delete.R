library(leaflet)
library(shiny)
library(dplyr)
library(readr)

ui <- fluidPage(
  titlePanel("Melbourne Urban Tree Visualisation"),
  leafletOutput("treedat"),
  uiOutput("precinct")
  #Giving an input name and listing out types to choose in the Shiny app
)

server <- function(input, output){
  
  td <- data.frame(
    LifeExpectencyValue = sample(20:100, 10), 
    Precinct = c(rep("CBD", 3), rep("ABC", 4), rep("XYZ", 3)),
    CommonName = sapply(1:10, function(x){paste(sample(LETTERS, 10, replace = TRUE), collapse = "")}),
    Genus = rep(c("m","f"), each = 5),
    lat = seq(5, 50, 5),
    lng = seq(2, 65, 7)
  )
  
  pal <- colorNumeric(palette = "RdYlGn", domain = td$LifeExpectencyValue)
  
  output$precinct <- renderUI({
    
    choices <- as.character(unique(td$Precinct))  
    choices <- c('All', choices)
    selectInput(inputId = "precinct", label = "Precinct", choices = choices, selected = "CBD")
    
  })
  
  output$treedat <- renderLeaflet({
    
    leaflet() %>% 
      addTiles(
        urlTemplate = 'http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
        attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
      ) %>%
      addLegend(pal = pal, values = td$LifeExpectencyValue, opacity = 1, title = "Life Expectency")
    
  })
  
  observeEvent(input$precinct, {
    
    #if(is.null(td)) return()
    ## get the choice from teh drop-down box
    PRECINCT = input$precinct
    
    ## supbset the data based on the choice
    if(PRECINCT != 'All'){
      td2 <- td[td$Precinct == PRECINCT, ]
    }else{
      td2 <- td
    }
    ## plot the subsetted ata
    leafletProxy("treedat") %>%
      clearMarkers() %>%
      addCircleMarkers(lat = td2$lat, lng = td2$lng,
                       radius= 5, fillOpacity = 0.5, stroke = FALSE, color=pal(td2$LifeExpectencyValue),
                       popup = paste("<b>", td2$CommonName,"</b>", "<br>", 
                                     "<b>","Years Left:", "</b>", td2$LifeExpectency, "<br>", 
                                     "<b>","Genus:","</b>", td2$Genus))
  })
}

shinyApp(ui = ui, server = server)