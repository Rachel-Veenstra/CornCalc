#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(dplyr)
library(sp)
library(geosphere)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("CornCalc"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            tags$h3("Producer Inputs:"),
            br(),
            numericInput("lat", "Field Latitude:  (+°N)", value=39.202),
            numericInput("long", "Field Longitude:  (-°E)", value=-96.594),
            textInput("date", "Planting Date: (mm-dd)", value = "04-20")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("map"),
            plotOutput("gdd")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$map <- renderLeaflet({
        
        input <- data.frame(Location = 'Your Location',  # Location
                            Start = paste0('2021', substr(as.character(input$date),1,2), substr(as.character(input$date),4,5), '000000'),  #Planting date ###################################
                            End = paste0(format(Sys.Date(), "%Y%m%d"), '000000'),  # Current date (harvest date)
                            Lat = as.character(input$lat),
                            Long = as.character(input$long))
        
        
        stations <- read.table('http://mesonet.k-state.edu/rest/stationnames/', sep=',', header=T) %>% # Pulling station coordinates
            dplyr::select(c(NAME, LATITUDE, LONGITUDE)) %>% 
            mutate(id = seq(1,102))
        
        loc <- data.frame(NAME = 'My Location', LATITUDE = as.numeric(input$Lat), LONGITUDE = as.numeric(input$Long)) # Creating selected station info
        
        sp.stations <- stations # Creating identical dataframes for spatial objects
        sp.loc <- loc
        
        coordinates(sp.stations) <- ~LONGITUDE+LATITUDE # Creating coodinate points
        coordinates(sp.loc) <- ~LONGITUDE+LATITUDE
        
        dist <- data.frame(distHaversine(sp.stations, sp.loc)) %>%  # Calcuating distances from stations to selected point
            mutate(id = seq(1, 102)) %>% 
            left_join(stations, by = 'id') %>% # Joining station names with distances
            rename(dist = 1)
        
        close <- min(dist$dist) # Lowest distance from point
        
        final <- dist %>% # Selecting weather station with lowest distance
            filter(., dist == close)
        
        # showing map
        leaflet() %>% 
            addProviderTiles("Esri.WorldImagery") %>% 
            addMarkers(lng = final$LONGITUDE[1], lat = final$LATITUDE[1]) %>% 
            addMarkers(lng = loc$LONGITUDE[1], lat=loc$LATITUDE[1])
    })
    
    output$gdd <- renderPlot({
        
        # wrangling data
        input <- data.frame(Location = 'Your Location',  # Location
                            Start = paste0('2021', substr(as.character(input$date),1,2), substr(as.character(input$date),4,5), '000000'),  #Planting date ###################################
                            End = paste0(format(Sys.Date(), "%Y%m%d"), '000000'),  # Current date (harvest date)
                            Lat = as.character(input$lat),
                            Long = as.character(input$long))
        
        stations <- read.table('http://mesonet.k-state.edu/rest/stationnames/', sep=',', header=T) %>% # Pulling station coordinates
            dplyr::select(c(NAME, LATITUDE, LONGITUDE)) %>% 
            mutate(id = seq(1,102))
        
        loc <- data.frame(NAME = 'My Location', LATITUDE = as.numeric(input$Lat), LONGITUDE = as.numeric(input$Long)) # Creating selected station info
        
        sp.stations <- stations # Creating identical dataframes for spatial objects
        sp.loc <- loc
        
        coordinates(sp.stations) <- ~LONGITUDE+LATITUDE # Creating coodinate points
        coordinates(sp.loc) <- ~LONGITUDE+LATITUDE
        
        dist <- data.frame(distHaversine(sp.stations, sp.loc)) %>%  # Calcuating distances from stations to selected point
            mutate(id = seq(1, 102)) %>% 
            left_join(stations, by = 'id') %>% # Joining station names with distances
            rename(dist = 1)
        
        close <- min(dist$dist) # Lowest distance from point
        
        final <- dist %>% # Selecting weather station with lowest distance
            filter(., dist == close)
        
        st.name <- final$NAME[1] # Pulling name of weather station
        
        st.edit <- st.name %>% 
            str_replace_all(' ', '%20') # Editing weather station name to be url-compatible
        
        get <- input %>% 
            mutate(url = paste0("http://mesonet.k-state.edu/rest/stationdata/?stn=", st.edit, "&int=day&t_start=", `Start`, "&t_end=", `End`, "&vars=TEMP2MAVG,TEMP2MMIN,TEMP2MMAX,PRECIP"))
        
        data <- read.table(get$url, sep=',', header=T) %>% 
            mutate(Location = 'Your Location')
        
        data <- data %>% 
            mutate(Date = substr(`TIMESTAMP`, 0,10)) %>% 
            
            mutate(min = as.numeric(TEMP2MMIN), # changing columns to numeric
                   max = as.numeric(TEMP2MMAX)) %>% 
            
            mutate(min.corn = case_when(min <= 10 ~ min,
                                        min >= 30 ~ 30, # maximum temperature for corn growth - 30C
                                        min > 10 & min < 30 ~ min), # growth window (minimum temperature)
                   
                   max.corn = case_when(max <= 10 ~ max,
                                        max >= 30 ~ 30, # maximum temperature for corn growth - 30C
                                        max > 10 & max < 30 ~ max), # growth window (maximum temperature)
                   
                   temp.corn = ((min.corn + max.corn)/2)-10) %>% # average of growth window minus base temperature (10C)
            
            mutate(temp.corn = case_when(temp.corn <= 0 ~ 0, # setting negative values to 0
                                         temp.corn > 0 ~ temp.corn)) %>% # maintaining positive values
            
            group_by(Location) %>% # separating locations (this case, only one)
            
            mutate(CumGDD = cumsum(temp.corn), # adding up accumulated GDD
                   CumPP = cumsum(PRECIP)) %>% # adding up accumulated precipitation
            
            ungroup() %>% 
            
            select(-c(TEMP2MMIN, TEMP2MMAX, TIMESTAMP, TEMP2MAVG, min.corn, max.corn, temp.corn)) # removing unnecessary columns 
        
        start <- paste0('2021-', substr(as.character(input$date),1,2),'-', substr(as.character(input$date),4,5))
        
        # plot results GDD
        ggplot(data=data)+
            geom_line(aes(x = as.Date(Date), y = (CumGDD*9/5)+32), color='darkgreen', size = 2, alpha = 0.75) +
            geom_point(aes(x = as.Date(Date), y = (CumGDD*9/5)+32), fill= 'darkgreen', size = 3, shape = 21, color='black')+
            
            scale_x_date(breaks = "3 days", date_labels= "%B %d", limits = c(as.Date(start, format = '%Y-%m-%d'), Sys.Date())) +
            
            ggtitle('2021 Cumulative Growing Degree Days')+
            xlab('Date')+
            ylab('GDD (°F day⁻¹)')+
            
            theme(panel.grid = element_line(color='#d9d9d9',size=0.2), 
                  strip.background = element_blank(),
                  panel.background = element_rect(colour = 'black', size = 0.3, fill = 'white'),
                  title = element_blank(),
                  axis.text.x = element_text(size = 15, margin = margin(t = 3, r = 0, b = 0, l = 0),angle=45,vjust=1,hjust=1,color = 'black'),
                  axis.text.y = element_text(size = 18, margin = margin(t = 0, r = 5, b = 0, l = 0), color = 'black'),
                  legend.text = element_blank(),
                  axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 10, b = 0, l = 0), color = 'black'),
                  axis.title.x = element_blank(),
                  legend.title = element_blank(),
                  legend.position = 'none')
        
        
    }, height=400)
}

# Run the application 
shinyApp(ui = ui, server = server)
