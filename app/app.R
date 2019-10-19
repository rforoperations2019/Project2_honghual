#author: Nicole Li
#AndrewID： honghual

library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(shinyjs)
library(rgeos)


# Load data and remove miscelleanouse information
airbnb<-readOGR("https://raw.githubusercontent.com/cameronmaske/geo-dc/master/datasets/airbnb/airbnb.geojson")
airbnb <- airbnb@data[,-c(1,2)] 
airbnb<- subset(airbnb, bed_type != "Airbed")
pdf(NULL)

# Fun icons and colors for different bed types
icons <- awesomeIconList(
  `Real Bed`= makeAwesomeIcon(icon = "bed", library = "fa", markerColor = "purple"),
  `Futon` = makeAwesomeIcon(icon = "bed", library = "fa", markerColor = "blue"),
  `Sofa` = makeAwesomeIcon(icon = "bed", library = "fa", markerColor = "orange"),
  `Pull-out Sofa` = makeAwesomeIcon(icon = "table", library = "fa", markerColor = "green")
)

#Dashboard header------------------------------------------------------------
header <- dashboardHeader(title= "Airbnb Washington DC")

#Dashboard sidebar-----------------------------------------------------------------------------
sidebar <-dashboardSidebar(
  
  #Menu Items--------------------------------------------
  menuItem("Airbnb Locations",icon = icon("map"), tabName= "map"),
  menuItem("Prices by Room Type",icon = icon("bar-chart"), tabName= "plot1"),
  menuItem("Prices by Bed type",icon = icon("bar-chart"), tabName= "plot2"),
  menuItem("Table", icon = icon("table"), tabName= "table"),
  
  # Room Type Check Box
  checkboxGroupInput(inputId="roomSelect", label="Room Type",
                     choices= unique(airbnb$room_type) , 
                     selected= levels(airbnb$room_type)),
  
  # Bed Type Selection
  selectInput("BedSelect", label= "Bed Type",
              choices= unique(airbnb$bed_type), 
              multiple= TRUE,
              selected = levels(airbnb$bed_type)),
  
  # Price Range Filters
  sliderInput("priceSelect", label= "Price Range",
              min= min(airbnb$price),
              max= max(airbnb$price),
              value= c(min(airbnb$price),300), step=10),
  
  # Download button 
  downloadButton("downloadData","Download Airbnb Data")
  
)
#Dashboard body------------------------------------------------------------------------------
body <- dashboardBody(tabItems(
  
  # Leaflet Map page----------------------------------------------- 
  tabItem("map",
          title = "Map Airbnb Locations",
          leafletOutput("LocationMap")
  ),
  
  #Room Type plot page-------------------------------------------------
  tabItem("plot1",
          
          # Value Boxes--------------------------------------
          fluidRow(
            infoBoxOutput("avgprice"),
            infoBoxOutput("avgaccomodates")
          ),
          
          #plot of the prices and room type histogram-------------------
          fluidRow(
            tabBox(title = "Prices and Room Type",
                   width= 12,
                   tabPanel("Airbnb Washington D.C", 
                            plotlyOutput("PriceType")) 
            )
          )
  ),
  
  #Violin Plot of prices and bed type--------------------------------------
  tabItem("plot2",
          fluidRow(
            tabBox(title = "Prices and Bed Type",
                   width= 12,
                   tabPanel("Airbnb Washington DC"),
                   plotlyOutput("PriceBedType")
            )
          )
  ),
  
  #Data Table of filtered inputs-----------------------------------
  tabItem("table",
          fluidPage(
            box(title="Selected Room Information", DT::dataTableOutput("table"), width = 12)
          )
  )
))

ui <- dashboardPage(header, sidebar , body)


#Define server function to create plots and value boxes
server <- function(input, output){
    
    #reactive data---------------------------------------------------------------- 
    airbnbInput<- reactive ({
    airbnb <- airbnb %>%
      
    # price filter
    filter(price >= input$priceSelect[1] & price <= input$priceSelect[2])
    
    # room type and bed type filter
    if (length(input$BedSelect)>0) {
      airbnb <- subset(airbnb, bed_type %in% input$BedSelect &
                         room_type %in% input$roomSelect)
    }
    return (airbnb)
  })
    
  # Creating map base layer---------------------------------------------------------------------
  output$LocationMap <- renderLeaflet({
    leaflet() %>%
      # creating Basemaps
      addProviderTiles("Stamen.Toner", group = "Toner") %>%
      addProviderTiles("Stamen.Terrain", group = "Terrain") %>%
      setView(-77.03,38.91,11) %>%
      # Layers control
      addLayersControl(baseGroups = c("Toner", "Terrain"),
                       overlayGroups = c("Room_type","Bed_type"))
  })
  
  # When ser make room type selection update room type layer-------------------------------------------
  observe({
    
      if(length(input$roomSelect)>0) {
      airbnbmap <- airbnbInput()
      room_palette<- colorFactor(palette = "Set1", domain = airbnbmap$room_type)
      
      leafletProxy("LocationMap", data = airbnbmap) %>%
      clearGroup(group= "Room_type") %>%
      clearControls() %>%
      addCircles(data = airbnbmap,
                 color = ~room_palette(room_type),
                 radius=25,
                 fillOpacity = 10,
                 group = "Room_type")  %>%
      
      addLegend(position = "topright" , 
                pal = room_palette, 
                values = airbnbmap$room_type, 
                title = "Room_type",
                group ="Room_type") 
      }
  })
  
  # when user make bed type selection update bed type layer -------------------------------------------
  observe({
      if(length(input$BedSelect)>0) {
      airbnbmap <- airbnbInput()
      bed_palette<- colorFactor(palette = "Set1", domain = airbnbmap$bed_type)
      
      leafletProxy("LocationMap", data = airbnbmap) %>%
      clearGroup(group = "Bed_type") %>%
      clearControls() %>% 
      addAwesomeMarkers(data = airbnbmap,
                        icon = ~icons[bed_type],
                        popup = ~url,
                        group = "Bed_type") %>%
      addLegend(position = "topright" , 
                pal = bed_palette, 
                values = airbnbmap$bed_type, 
                title = "Bed_type",
                group = "Bed_type")
      }
  })
  
  #Two reactive information box--------------------------------------------------
  
  #average price info box
  output$avgprice <- renderInfoBox({
    num<- paste(round(mean(airbnbInput()$price),2),"$")
    infoBox(title= "Avg Price", value = num, icon = icon("dollar"), color = "purple")
  })
  
  #average accomodations info box
  output$avgaccomodates <- renderInfoBox({
    num<- paste(round(mean(airbnbInput()$accommodates),2),"accomodates")
    infoBox(title="Avg Number of Accomodates",value = num, icon = icon("moon"), color = "red" )
  })
  
  # two plotly graphs-------------------------------------------------------------
  
  #plot1: prices vs type histogram
  output$PriceType <- renderPlotly({
    ggplot(data=airbnbInput(), aes(x=price,color=room_type, fill=room_type))+geom_histogram()
  })
  
  #plot2 neighbourhood prices violin chart
  output$PriceBedType <- renderPlotly({
    ggplot(data = airbnbInput(), aes(x = bed_type, y= price, fill=bed_type)) + geom_violin()
  })

  
  
  #Entire Data table------------------------------------------------------------------
  output$table <- DT::renderDataTable(airbnbInput()[,c(2,3,4,6,8,9,11)],options = list(lengthMenu = c(5, 30, 50), pageLength = 20))
  
  #download button----------------------------------------------------
  output$downloadData<-downloadHandler(
                       filename="airbnb_dc.csv",
                       content=function(file){write.csv(airbnbInput(), file,row.names = FALSE)
  })

}

shinyApp(ui=ui, server=server)


