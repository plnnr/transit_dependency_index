##### Begin app ####
## Load functions and data
source("global.R")

##### Begin UI #####

vulnerability_data <- readRDS("acs_transit_dependency_weighting_data.rds")

routes <- st_read("./data/shapes/Fall21RouteShape.shp") %>% 
    st_transform(crs = '+proj=longlat +datum=WGS84') 

ui <- navbarPage(
    theme = shinytheme("lumen"), ##(lumen, paper, simplex, flatly, yeti) ## See interactive themes: https://gallery.shinyapps.io/117-shinythemes/
    "Transit Dependency Mapping",
    
    tabPanel("Weighting Map", value = "vulnerability", 
             sidebarPanel(
                 shiny::helpText("Explore the demographic variables that go into calculating",
                                 "transit-dependency indices."),
                 radioButtons(inputId = "geo_filter", label = "Filter for Region, Place or Transit District:", choices = c("SF Bay Area" = "Region", "Oakland" = "Primary Place", "AC Transit District" = "Transit District"), selected = "Transit District"),
                 radioButtons(inputId = "show_routes", label = "Show Bus Routes?", choices = c("Yes (loads slower)" = "Yes", "No" = "No"), selected = "No"),
                 hr(),
                 shiny::helpText("Slide bar to desired weight for each variable",
                                 "you wish to add to the transit-dependency model.",
                                 "The weight corresponds to the relative importance",
                                 "of that variable. 5 = very important, 0 = turned off.",
                                 "Download the results using button at bottom."),
                 sliderInput("wt_bipoc", "RACE: People of color weight", min = 0, max = 5, value = 0),
                 sliderInput("wt_youth", "AGE: Youth age 15 to 21", min = 0, max = 5, value = 0),
                 sliderInput("wt_elder", "AGE: Elders age 65+", min = 0, max = 5, value = 0),
                 sliderInput("wt_age_dep", "AGE: Youth and elders", min = 0, max = 5, value = 0),
                 sliderInput("wt_disability", "DISABILITY: People with disability", min = 0, max = 5, value = 0),
                 sliderInput("wt_poverty", "INCOME: People under 2x poverty rate", min = 0, max = 5, value = 0),
                 sliderInput("wt_0veh", "VEHICLES: Households with 0 vehicles", min = 0, max = 5, value = 0),
                 sliderInput("wt_insuff_veh", "VEHICLES: Households with insufficient vehicles", min = 0, max = 5, value = 0),
                 sliderInput("wt_unemployed", "EMPLOYMENT: Unemployed people", min = 0, max = 5, value = 0),
                 sliderInput("wt_edu_hs", "EDUCATION: Adults with HS diploma or less", min = 0, max = 5, value = 0),

                 downloadButton("download_geojson", label = "Download")
                 
             ),
             mainPanel(
                 tabsetPanel(
                     tabPanel("Weighting Tool", id = "tv_map1", tags$style(type = "text/css", "#vulnerabilitymap {height: calc(95vh - 90px) !important;}"), leafletOutput("vulnerabilitymap")), # https://stackoverflow.com/questions/36469631/how-to-get-leaflet-for-r-use-100-of-shiny-dashboard-height/36471739#36471739
                     tabPanel("Table", id = "tv_table2", DT::dataTableOutput('composite_vulnerability_table'))
                 )
             ))
    
)


##### Begin server #####
server <- function(input, output, session) {
    
    ########## Transit Vulnerability Mapping ##########
    
    reweighted <- reactive({
        if (input$geo_filter == "Primary Place"){
            vulnerability_data <- vulnerability_data %>%
                filter(in_primary_place == TRUE)
        }
        
        if (input$geo_filter == "Transit District"){
            vulnerability_data <- vulnerability_data %>%
                filter(in_transit_district == TRUE)
        }
        
        vulnerability_data %>%
            mutate(weighted_bipoc = ntile(pct_bipoc, 100) * input$wt_bipoc,
                   weighted_youth = ntile(pct_youth_15t21, 100) * input$wt_youth,
                   weighted_elder = ntile(pct_elder_65, 100) * input$wt_elder,
                   weighted_age_dep = ntile(pct_age_dependent, 100) * input$wt_age_dep,
                   weighted_disability = ntile(pct_disability, 100) * input$wt_disability,
                   weighted_poverty = ntile(pct_poverty, 100) * input$wt_poverty,
                   weighted_0veh = ntile(pct_vehicles_0, 100) * input$wt_0veh,
                   weighted_insuff_veh = ntile(pct_vehicles_shortage, 100) * input$wt_insuff_veh,
                   weighted_unemployed = ntile(pct_unemployed, 100) * input$wt_unemployed,
                   weighted_edu_hs = ntile(pct_less_hs, 100) * input$wt_edu_hs,
                  
                   composite_score = weighted_bipoc + weighted_youth + weighted_elder + weighted_age_dep + 
                       weighted_disability + weighted_poverty + weighted_0veh + 
                       weighted_insuff_veh + weighted_unemployed + weighted_edu_hs,
                
                   indexed_score = range01(composite_score, na.rm = T) * 100) %>%
            # select(GEOID, NAME, indexed_score, in_primary_place, in_transit_district) %>%
            arrange(desc(indexed_score))
    })
    
    output$download_geojson <- downloadHandler(
        filename = "custom_weight_export-geojson.geojson",
        content = function(file) {
            sf::st_write(obj = reweighted(), dsn = file)
        }
    )  
    
    # output$download_shp <- downloadHandler(
    #     filename = "custom_weight_export-shp.zip",
    #     content = function(file) {
    #         sf::st_write(obj = reweighted(), dsn = "custom_weight_export-shp.shp")
    #         zip(zipfile='custom_weight_export-shp.zip', files=Sys.glob("custom_weight_export-shp.*"))
    #         file.copy("custom_weight_export-shp.zip", file)
    #     }
    # )  
    
    
    
    output$vulnerabilitymap <- renderLeaflet({
        bins <- c(0, 20, 40, 60, 80, 100)
        pal <- leaflet::colorBin(viridis_pal(option = "C")(length(bins)), domain = reweighted()$indexed_score, bins = bins)
        popup <- paste0("<strong>Tract: </strong>", reweighted()$NAME, 
                        "<br/>",
                        "<strong>Transit Dependency Index: </strong>", 
                        as.character(round(reweighted()$indexed_score,0)),
                        "<br/>",
                        "<strong>% BIPOC: </strong>", 
                        as.character(round(reweighted()$pct_bipoc*100,0)),
                        "<br/>",
                        "<strong>% Youth: </strong>", 
                        as.character(round(reweighted()$pct_youth_15t21*100,0)),
                        "<br/>",
                        "<strong>% Elder: </strong>", 
                        as.character(round(reweighted()$pct_elder_65*100,0)),
                        "<br/>",
                        "<strong>% Youth & Elder: </strong>", 
                        as.character(round(reweighted()$pct_age_dependent*100,0)),
                        "<br/>",
                        "<strong>% Disabled: </strong>", 
                        as.character(round(reweighted()$pct_disability*100,0)),
                        "<br/>",
                        "<strong>% in Poverty: </strong>", 
                        as.character(round(reweighted()$pct_poverty*100,0)),
                        "<br/>",
                        "<strong>% 0 Vehicles: </strong>", 
                        as.character(round(reweighted()$pct_vehicles_0*100,0)),
                        "<br/>",
                        "<strong>% Insufficient Vehicles: </strong>", 
                        as.character(round(reweighted()$pct_vehicles_shortage*100,0)),
                        "<br/>",
                        "<strong>% Unemployed: </strong>", 
                        as.character(round(reweighted()$pct_unemployed*100,0)),
                        "<br/>",
                        "<strong>% HS diploma or less: </strong>", 
                        as.character(round(reweighted()$pct_less_hs*100,0))
                        )
        
        tdi_map <- leaflet(reweighted()) %>%
            addProviderTiles('CartoDB.Positron') %>%
            addPolygons(fillColor = ~pal(indexed_score),
                        weight = 1,
                        opacity = 1,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.5,
                        popup = popup) %>% 
            setView(lng = -122.183248, lat = 37.736487, zoom = 10) %>% ## Optionally set map center and zoom level
            addLegend("topright", pal = pal,
                      values = ~indexed_score,
                      title = "Transit Dependency Index",
                      opacity = 0.6)

        if(input$show_routes == "Yes"){
        tdi_map <- leaflet::addPolylines(tdi_map,
                                      data = routes,
                                      weight = 0.7,
                                      opacity = .35,
                                      color = "#000000") 
        }
        
        tdi_map
        
    })
    
    output$composite_vulnerability_table <- DT::renderDataTable(
        reweighted() %>% 
            select(GEOID, indexed_score, pct_bipoc:pct_less_hs) %>%
            st_set_geometry(NULL) %>%
            mutate(indexed_score = round(indexed_score, 0)) %>%
            rename(`Indexed Score` = indexed_score),
        filter = "top",
        options = list(
            pageLength = 10
        ))
}



# Run the application 
# options(browser = "/Applications/Google Chrome Canary.app/Contents/MacOS/Google Chrome Canary")
# runApp(list(ui = ui, server = server, display.mode = "showcase"), host = "127.0.0.1", port = 7002, launch.browser = T) # 192.168.1.170 # 127.0.0.1
shinyApp(ui = ui, server = server)

