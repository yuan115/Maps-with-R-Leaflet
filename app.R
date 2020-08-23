library(shiny)
library(shinydashboard)
library(rsconnect)
library(leaflet)
library(rgdal)
library(dplyr)
library(ggmap)
library(data.table)
library(naniar)
library(DT)
library(lubridate)

Taipei_region <- readOGR("D:\\Programming\\R\\(Udemy) Maps with R Leaflet\\Maps with R Leaflet\\Taipei_region\\Taipei_region.shp")
# Data preparation
housingData <- fread("D:\\Programming\\R\\(Udemy) Maps with R Leaflet\\Maps with R Leaflet\\NEW_data_housing_prices_0811_morning.csv")
housingData <- housingData[, 1:44]

housingData_random <- housingData[sample(nrow(housingData)), ]

housingData_randomPart <- housingData_random[c(1:round(0.1*nrow(housingData_random))), ][, 1:44]


housingData_randomPart_naOmit <- housingData_randomPart %>% na.omit()

housingData_randomPart_naOmit <- housingData_randomPart_naOmit %>% 
    mutate(year = year(housingData_randomPart_naOmit$"交易年月日")) %>% 
    filter(year %in% c(2016:2020)) %>% 
    select(area = `區域&里別`, totalPrice = `總價(元)`, unitPrice = `單價(元/平方公尺)`,
           housingAge = `屋齡`, floor = trade_floor_revision, buildingArea = `建物移轉總面積(平方公尺)`,
           landArea = `土地移轉總面積(平方公尺)`,dateOfTrade =  `交易年月日`,
           lat = `緯度 (WGS84)`,long =  `經度 (WGS84)`, year)

ui <- dashboardPage(skin = "red", dashboardHeader(title = "Housing Information Dashboard"),
                    dashboardSidebar(sliderInput("date_range", label = "Date Range",
                                                 min = 2016, max = 2020, value = c(2016:2020),
                                                 sep = "", step = 1)),
    dashboardBody(fluidRow(box(width = 12, leafletOutput(outputId = "myMap"))),
                  fluidRow(box(width = 12, dataTableOutput(outputId = "summaryTable"))))
)



server <- function(input, output) {
        
    data_input <- reactive({
        
        housingData_randomPart_naOmit %>%
            filter(year >= input$date_range[1]) %>%
            filter(year <= input$date_range[2])
    })

    labels <- reactive({
        paste("<p>", data_input()$area, "</p>",
              "<p>", "Total Price: ", data_input()$totalPrice, "</p>",
              "<p>", "Unit Price: ", data_input()$unitPrice, "</p>",
              "<p>", "Housing Age: ", data_input()$housingAge, "</p>",
              "<p>", "floor: ", data_input()$floor, "</p>",
              "<p>", "Building Area: ", data_input()$buildingArea , "</p>",
              "<p>", "Land Area: ", data_input()$landArea, "</p>",
              "<p>", "Date Of Trade: ", data_input()$dateOfTrade, "</p>")
        
    })
    
    
    output$myMap <- renderLeaflet({
        leaflet() %>% 
            setView(lat = 25.06776, lng = 121.53185, 11) %>% 
            addProviderTiles(providers$Wikimedia) %>% 
            addPolygons(data = Taipei_region, color = "#660000",
                        weight = 1, smoothFactor = 0.5) %>%
            addCircleMarkers(lat = data_input()$lat,
                             lng = data_input()$long,
                             color = "#484891",
                             weight = 1,
                             radius = 5,
                             label = lapply(labels(), HTML))
    })
    

    output$summaryTable <- renderDataTable(data_input())
    
}


shinyApp(ui = ui, server = server)

