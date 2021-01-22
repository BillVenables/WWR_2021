
#################################################################################################################
################################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     Load req packages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##################################################################################################################
#################################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#    DATA RELATED PACKAGES
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#geospatial stuff
library(rgdal)
library(maptools)
library(rgeos)
library(sp)
library(sf)
library(leaflet)
library(leaflet.providers)

#data wrangling
library(data.table)
library(tidyverse) 
library(reshape2)
#library(packrat)
library(sqldf)
library(tidyr)
#text



#visualisation
library(plotly)
library(RColorBrewer)
library(scales)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     SHINY RELATED PACKAGES
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(shinyalert)
library(shinydashboardPlus)
library(shinyjqui)
library(styler)
library(shinyAce)
library(shinyEffects)
library(htmlwidgets)
library(htmltools)
library(htmlTable)
library(shinycssloaders)


#################################################################################################################
################################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     Read in the data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##################################################################################################################
#################################################################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     Spatial Data - 2016 ABS SA2 polygons (shp files) - subset to Logan LGA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sa2_log <- readOGR(dsn=path.expand("./data/shape_files"), layer="Logan_SA2_2016")
#plot(sa2_log["SA2_MAIN16"])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     DSS, Total Population, ATSI population Language at home data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
log_data1 <- read.csv("data/Logan_SA2_DSS2020_Indig_TotalPop_Lang_at_home.csv")
#glimpse(log_data1)
log_data1$X<-NULL

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     Ask Izzy Jan - Jul 2020 data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ask_iz <- read.csv("data/Ask_Izzy_2020_Jan_July_Logan_SA2.csv")
#glimpse(ask_iz)
ask_iz$X<-NULL

#sum across whole year for map
ask_iz_sum <- ask_iz %>% 
    group_by(Year, SA2_MAIN16, SA2_NAME16) %>% 
    summarise_if(is.numeric, sum) %>% 
glimpse()
    

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     Income data data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
income_log <- read.csv("data/Logan_Income_data_ABS_2016.csv")
#glimpse(income_log)
income_log$X<-NULL
income_log$X.1<-NULL

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     Population ages 0-8
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pop_08_log <- read.csv("data/Logan_map_pop_0_8.csv")
#glimpse(pop_08_log)
pop_08_log <- pop_08_log %>% 
    dplyr::select( -X) %>% 
    mutate_if(is.numeric, round, 1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     Population ATSI
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ATSI_log <- read.csv("data/Logan_map_ATSI_pop.csv")
#glimpse(ATSI_log)


ATSI_log <- ATSI_log %>% 
    dplyr::select( -X) %>% 
    mutate_if(is.numeric, round, 1)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     Food outlet data - these data were manually collected by a research assistance Nov 2020
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
food_dat <- read.csv("data/Geocoded_Addresses_Food_Outlet_Logan_Jan_21_2021.csv")

#unique(food_dat$Outlet_Type)

#recode foot outlet types to consolidate categories
food_dat_types <- food_dat %>% 
    dplyr::select(-X) %>% 
    mutate(Type_Food = case_when(
        Outlet_Type %in% c("Pickup", "Pickup ","Pickup_Delivery","Delivery", "Restaurant_Takeaway", "Takeaway" ) ~ "Pickup_Delivery",
        Outlet_Type %in% c("Bakery","Bakery " ) ~ "Bakery",
        Outlet_Type %in% c("Convenience_Petrol","Convenience Store", "Local Market ","Local Market" ) ~ "Local_Convienience",
        Outlet_Type %in% c( "Cafe", "Restaurant", "Restaurant ", "Dessert"  ) ~ "Restaurant_Cafe",
        Outlet_Type %in% c( "Distillery", "Brewery", "Restaurant_Pub" ) ~ "Pub_Brewery_Distillery",
        Outlet_Type %in% c( "Supermarket ", "Grocery Store","Butcher" ) ~ "Supermarket",
        Outlet_Type %in% c(  "Water Park ", "Sports Club"   ) ~ "Sports_Club_Park",
        Outlet_Type %in% c("Venue", "Catering" ) ~ "Venue_Catering",
        TRUE ~ as.character(Outlet_Type)
    )) %>% 
    #counts per type and SA2
    add_count(Type_Food, SA2_NAME16, name = "type_per_sa2") %>% 
glimpse()

food_dat_types_map <- food_dat_types %>% 
    select(SA2_NAME16, Type_Food,type_per_sa2) %>% 
    unique() %>% 
    pivot_wider(names_from = "Type_Food", values_from = "type_per_sa2") %>% 
    glimpse()

#add underscore to the column names in place of spaces
names(food_dat_types_map) <- gsub(x = names(food_dat_types_map), pattern = " ", replacement = "_")

#create dataframe for markers
food_dat_types_markers <-food_dat_types %>% 
    mutate(Type_Food = gsub(" ", "_", Type_Food),
           Type_Food = as.factor(Type_Food)) %>% 
    filter(Type_Food != "What_is_is") %>% 
    glimpse()

# #create small subset to test marker logic
# food_dat_types_markers_test <- food_dat_types_markers %>% 
#     filter(Type_Food %in% c("Fast_Food_restaurant", "Restaurant_Cafe","Supermarket"))
# #unique(food_dat_types_markers_test$Type_Food)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     merge all the data for the map
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sa2_merged <- merge(sa2_log, ATSI_log, by = "SA2_NAME16")
sa2_merged <- merge(sa2_merged, pop_08_log, by = "SA2_NAME16")
sa2_merged <- merge(sa2_merged, income_log, by = "SA2_NAME16")
sa2_merged <- merge(sa2_merged, ask_iz_sum, by = "SA2_NAME16")
sa2_merged <- merge(sa2_merged, food_dat_types_map, by = "SA2_NAME16")
sa2_merged <- merge(sa2_merged, log_data1, by = "SA2_NAME16")

#######################################################################################################################
######################################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DASHBOARD UI AND SERVER LOGIC 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#######################################################################################################################
######################################################################################################################
# Going with a 4 Map concept as proposed in word doc in data_processing/docs folder

mycol <- colorRampPalette(c("#822E81", "#20CCD5","#7DB800","#FFCC00","#F54E00","#CB1555", "#03045e")) #colour scheme being used


#*************************************************************************************************************
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   USER INTERFACE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#************************************************************************************************************
##THUMBNAIL SHARE IMAGE
#Social sharing

# info for sharing this app on facebook/twitter
share <- list(
    title = "Logan: Food resilience",
    url = "https://regionalinnovationdatalab.shinyapps.io/Logan_Food_Mapping/",
    image = "favicon.png",
    description = "Deep dive into data and map the existing food ecosystem in the Logan region. Developed to establish potential scenarios for growing resilience by exploring intersections between land use planning, entrepreneurship and social innovation. Dashboard built by the Regional Innovation Data lab at Griffith University."
    
)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("yeti"), #set theme for dashboard based on preset shiny themes
                
                #thumnail share image
                tags$head(
                    # Facebook OpenGraph tags - image to share when social sharing app
                    tags$meta(property = "og:title", content = share$title),
                    tags$meta(property = "og:type", content = "website"),
                    tags$meta(property = "og:url", content = share$url),
                    tags$meta(property = "og:image", content = share$image),
                    tags$meta(property = "og:image:width", content = "300"),
                    tags$meta(property = "og:image:height", content = "400"),
                    tags$meta(property = "og:description", content = share$description)),       
                
                #Header image
                titlePanel(div(img(src='header.png', height = "auto", width = "100%"))
                ),# end title panel for header image
                
                #title of web brower tab
                title = "Logan: Food resilience", 
                
                #for info boxes to work need to use shinyDashboard
                useShinydashboard(),
                
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                #sidebar displaying graphs per selected SA2
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                
                #sidebarPanel(width = 4, 
                             #tags$style(".well {background-color:#00264e;}"),
                             #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                             # stats to colour map drop down menu
                             #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                             fluidRow(column(width = 6,  
                                             pickerInput("stats_input", 
                                                         label="Select a data point or statistic to colour the map:",
                                                         multiple = FALSE,
                                                         #selected = TRUE,
                                                         choices = list(
                                                             "% of pop who identify as A & TSI" = "Percent_ATSI",
                                                             "% of pop ages 0 - 8" ="Age_0_8",
                                                             "Median age of working pop" = "Median.age.of.earner",
                                                             "Median income" = "Median",
                                                             "Income inequality (0 = perfect equality, 1 = max inequality)" = "Gini.coefficient",
                                                             "# searches for food related support (AskIzzy 2020)" = "FOOD",
                                                             "# searches for housing support (AskIzzy 2020)" = "HOUSING",
                                                             "# searches for A & TSI services (AskIzzy 2020)" = "ATSI",
                                                             "# of searches for Covid - 19 support (AskIzzy 2020)" = "COVID19",
                                                             "# of searches for emergency relief" = "E_RELIEF",
                                                             "# of Single Parenting Payments (DSS 2020)" = "Parenting_Payment_Single",
                                                             "# of JobSeeker Payments (DSS 2020)" = "JobSeeker_Payment",
                                                             "# of Disability Pensions (DSS 2020)" = "Disability_Support_.Pension",
                                                             "Statistical Area 2 (SA2) 2016" = "SA2_NAME16",
                                                             "# of supermarkets" = "Supermarket",
                                                             "# of hypermarket" = "Hypermarket",
                                                             "# of food relief services" = "Food_relief_service",
                                                             "# of restaurants & cafes"= "Restaurant_Cafe",
                                                             "# of fast food outlets" = "Fast_Food_restaurant"
                                                         ), # end of choices list for stats drop down
                                                         options = list(`style` = "btn-danger"))#end pickerinput
                                      )#end column
                               ),#end fluidRow
                #              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                #              # PLOTS under drop down
                #              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                #              fluidRow(
                #                  column(width = 12,
                #                         br(),
                #                         plotlyOutput("Ask_Izzy_Plot", width = "100%",height = 400),br())#end column
                #              )#end fluidRow
                # ), #end sidebar panel
                
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                # Main panel mAP OUT PUT
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                #mainPanel(
                    fluidRow(
                       column(12,
                           # leaflet map output
                           withSpinner(
                               leafletOutput("map",height = "700px", width="100%"))),#end column
                    
                )#end fluid row
            
                #) #end mainPanel  
                
                

)# end fluidPage

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
    #MAP OUTPUT 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
    #create the base map that will be displayed regardless of selected input
    output$map<-renderLeaflet({
        leaflet(sa2_merged) %>%
            #addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) %>%
            addProviderTiles(leaflet::providers$Esri.WorldImagery) %>%
            #marker groups need to be hidden until added by user
            hideGroup(type_group) %>%
            # Centre the map in the middle of our co-ordinates
            #fitBounds(min(153.99),max(-29.18),min(153.55),max(-24.12))
            fitBounds(152.8, -27.7, 153.3, -27.6)
        
    }) 
    
    type_group <-levels(food_dat_types_markers$Type_Food)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create custom Icons - different colour per Brand
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Food_Icons <- awesomeIconList(
        Hypermarket = makeAwesomeIcon(icon='shopping-cart', library='glyphicon', markerColor = 'black'),
        Supermarket = makeAwesomeIcon(icon='shopping-cart', library='glyphicon', markerColor = 'lightgray'),
        Pickup_Delivery = makeAwesomeIcon(icon='phone', library='glyphicon', markerColor = 'darkblue'),
        Restaurant_Cafe = makeAwesomeIcon(icon='cutlery', library='glyphicon', markerColor = 'cadetblue'),
        Fast_Food_restaurant = makeAwesomeIcon(icon='cutlery', library='glyphicon', markerColor = 'blue'),
        Local_Convienience = makeAwesomeIcon(icon='shopping-cart', library='glyphicon', markerColor = 'gray'),
        Food_relief_service  = makeAwesomeIcon(icon='alert', library='glyphicon', markerColor = 'red'),
        Pub_Brewery_Distillery = makeAwesomeIcon(icon='glass', library='glyphicon', markerColor = 'pink'),
        Bakery = makeAwesomeIcon(icon='grain', library='glyphicon', markerColor = 'beige'),
        Farm = makeAwesomeIcon(icon='apple', library='glyphicon', markerColor = 'green'),
        Food_Manufacturer = makeAwesomeIcon(icon='wrench', library='glyphicon', markerColor = 'orange'),
        Food_Truck = makeAwesomeIcon(icon='bullhorn', library='glyphicon', markerColor = 'purple'),
        Food_Supplier = makeAwesomeIcon(icon='star', library='glyphicon', markerColor = 'lightred'),
        Staff_Exclusive = makeAwesomeIcon(icon='briefcase', library='glyphicon', markerColor = 'lightblue'),
        Sports_Club_Park = makeAwesomeIcon(icon='ok-sign', library='glyphicon', markerColor = 'lightgreen'),
        Supplements_Store = makeAwesomeIcon(icon='map-marker', library='glyphicon', markerColor = 'darkgreen'),
        Venue_Catering = makeAwesomeIcon(icon='ok-sign', library='glyphicon', markerColor = 'darkpurple')
        )
    
    #colour palettes
    observe({ #beginning of map
        if (input$stats_input == "SA2_NAME16") {
            pal <- colorFactor(mycol(30), domain= sa2_merged[[input$stats_input]])
        } else {
            pal <- colorNumeric(c("#008B00","#C12525"), domain = sa2_merged[[input$stats_input]], reverse = FALSE)
        }
        
        
        
        #labels for polygons - hover or click
        labels <- sprintf(
                "<strong>%s</strong><br/>
                  Median income: %s <br/><br/>
                  Gini coefficient: %s<br/><br/>
                  Seaches for food support (AskIzzy 2020): %s<br/><br/>
                  Pct A and TSI: %s<br/><br/>
                  Pct ages 0-8: %s<br/><br/>", 
            
            sa2_merged$SA2_NAME16, sa2_merged$Median_LABELS, sa2_merged$Gini.coefficient, sa2_merged$FOOD,
            sa2_merged$Percent_ATSI, sa2_merged$Age_0_8) %>% lapply(htmltools::HTML)
        
        #creating a proxy map that displays the various stats from the stats drp down 
        leafletProxy("map", data = sa2_merged) %>%
            clearShapes() %>%
            addPolygons(
                layerId = sa2_merged$SA2_NAME16,
                fillColor = ~pal(sa2_merged[[input$stats_input]]), #colour map polygons based on user input
                fillOpacity = 0.9,
                weight = 0.6,
                opacity = 1,
                color = "#FFFFFF",
                dashArray = "2",
                label = labels
            ) %>% 
            addAwesomeMarkers(data = food_dat_types_markers,
                          lng = ~lon, lat = ~lat,
                          group = ~type_group[Type_Food],
                          # label = ~City,
                          # labelOptions = rep(labelOptions(noHide = T),nrow(cities)),
                          icon = ~Food_Icons[Type_Food] ) %>% 
            #add layer controls
            addLayersControl(overlayGroups = type_group,
                             options = layersControlOptions(collapsed = FALSE), position = "bottomright")
    
    

    varname<-switch(input$stats_input,
                    "Percent_ATSI" = "% of pop who identify as A & TSI",
                    "Age_0_8" = "% of pop ages 0 - 8",
                    "Median.age.of.earner" = "Median age of working pop",
                    "Median" = "Median income",
                    "Gini.coefficient" = "Income inequality",
                    "FOOD" = "# searches for food related support",
                    "HOUSING" = "# searches for housing support",
                    "ATSI" = "# searches for A & TSI services",
                    "COVID19" = "# of searches for Covid - 19 support",
                    "E_RELIEF" = "# of searches for emergency relief",
                    "SA2_NAME16" = "SA2 2016",
                    "Supermarket" = "# of supermarkets",
                    "Hypermarket" = "# of hypermarket",
                    "Food_relief_service" = "# of food relief services",
                    "Restaurant_Cafe" = "# of restaurants and cafes",
                    "Fast_Food_restaurant" = "# of fast food outlets",
                    "Parenting_Payment_Single" =  "# of Single Parenting Payments (DSS 2020)",
                    "JobSeeker_Payment" = "# of JobSeeker Payments (DSS 2020)",
                    "Disability_Support_.Pension" = "# of Disability Pensions (DSS 2020)")


    
    leafletProxy("map", data = sa2_merged) %>% clearControls() %>%
        addLegend(pal = pal, opacity = 0.9, title = varname,
                  values = ~sa2_merged[[input$stats_input]],labels = c(min(input$stats_input), max(input$stats_input)),
                  position = "topright")
    
    })# end of map shape colour and labeling  and legend observe event


}

# Run the application 
shinyApp(ui = ui, server = server)
#rsconnect::showLogs()
