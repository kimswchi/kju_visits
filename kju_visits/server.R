library(tidyverse)
library(ggmap)
library(shiny)

#load visit location data and rename headings for nicer display
loc <- read.csv("kju_visits_loc.csv", 
                col.names = c("X", "id", "title", "Date", "Visit_Site", "Location", "lat", "lon"))
#replace "?"s under the Location column, for nicer display
loc$Location <- gsub("\\?", "Unknown location, North Korea", loc$Location)
loc$Date <- as.character(loc$Date)

#load links to articles regarding visits
links <- read.csv("kju_visits_links.csv")
links$date <- as.character(links$date)

shinyServer(function(input, output, session) {
  
  #make selected single date reactive
  inputDate <- reactive({
    as.character(input$date)
  })
  
  #filter location data by selected date range and make it reactive
  filtered <- reactive({
    loc %>%
      filter(as.Date(Date) >= input$dateRange[1],
             as.Date(Date) <= input$dateRange[2])
  })
  
  #output where KJU was at on the selected date in sentence form
  output$VisitSingle  <- renderText({
    
    visit_site <- loc$Visit_Site[loc$Date == inputDate()]
    location <- loc$Location[loc$Date == inputDate()]
    start <- paste("On", inputDate(), "KJU was at")
    
    #if he made 2 visits, output: "On [date] KJU was at [site 1] in [location 1]
    #and [site 2] in [location 2].
    if (length(visit_site) > 1) {
      x <- paste(start, visit_site[1], "in", location[1],
                 "and", visit_site[2], "in", paste0(location[2],"."))
    } 
    #if he made 1 visit, output: "On [date] KJU was at [site] in [location]."
    else if (length(visit_site) == 1) {
      x <- paste(start, visit_site, "in", paste0(location, "."))
    }
    #if no known visits, output "No data for this date"
    else {
      x <- "No data for this date"
    }
    x
  })
  
  #output links to state news articles regarding visits made on selected single date, if any 
  output$Article  <- renderText({
    
    #list of links to relevant articles
    article <- as.character(links$article_link[links$date == inputDate()])
    
    if (length(article) > 1) {
      article_links <- paste(article[1],"\n", article[2])
    }
    else if (length(article) == 1) {
      article_links <- article
    }
    else {
      article_links <- "NA"
    }
    article_links
  })
  
  #output links to state news photos from visits made on selected single date, if any
  output$Pic  <- renderText({
    
    #list of links to relevant photos
    pic <- as.character(links$pic_link[links$date == inputDate()])
    
    if (length(pic) > 1) {
      pic_links <- paste(pic[1], "\n", pic[2])
    }
    else if (length(pic) == 1) {
      pic_links <- pic
    }
    else {
      pic_links <- "NA"
    }
    pic_links
  })
  
  #output map showing locations KJU went to in the selected date range and frequency of visits
  output$mapRange <- renderPlot({
    
    #make data frame of each location visited with number of times it was visited
    loc_count <- filtered() %>%
      filter(Location != "Unknown Location, North Korea") %>%
      group_by(Location, lat, lon) %>%
      summarize(count = n())
    #create weights based on count to determine size of each point
    loc_count$wgt = (loc_count$count/sum(loc_count$count) * 100 + 2)^(4/5)
    
    #get map of North Korea
    map <- get_map(location = 'North Korea', source = "google", maptype = "hybrid",
                   crop = FALSE, zoom = 7)
    
    #plot visit frequency
    ggmap(map) + 
      geom_point(data = loc_count, aes(x = lon, y = lat), 
                 color = "red", alpha = 0.5, size = loc_count$wgt) + 
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.position = "None")
  })
  
  #output map showing where KJU was on the selected date
  output$mapSingle <- renderPlot({
    
    #make dataframe of geographic coordinates
    longitude <- loc$lon[loc$Date == inputDate()]
    latitude <- loc$lat[loc$Date == inputDate()]
    longitude <- as.data.frame(longitude)
    latitude <- as.data.frame(latitude)
    coords <- cbind(longitude,latitude)
    colnames(coords) <- c("longitude", "latitude")
    
    #get map of North Korea
    map <- get_map(location = 'North Korea', source = "google", maptype = "hybrid",
                   crop = FALSE, zoom = 7)
    #mark location with a red and yellow star (4 triangles stacked on top of each other)
    ggmap(map) + 
      geom_point(data = coords, aes(x = longitude, y = latitude), 
                 color = "red", fill = "red", shape = 24, size = 4.5) + 
      geom_point(data = coords, aes(x = longitude, y = latitude), 
                 color = "red", fill = "red", shape = 25, size = 4.5) + 
      geom_point(data = coords, aes(x = longitude, y = latitude), 
                 color = "yellow", fill = "yellow", shape = 24, size = 3) + 
      geom_point(data = coords, aes(x = longitude, y = latitude), 
                 color = "yellow", fill = "yellow", shape = 25, size = 3) + 
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
  })
  
  #output number of visits KJU made in selected date range
  output$VisitRange  <- renderText({
    
    paste("Between", as.character(input$dateRange[1]), "and", 
          as.character(input$dateRange[2]), "KJU visited", 
          as.character(nrow(filtered())), "sites:")
  })
  
  #output location data filtered by date range, showing only the date, visit site, and location
  output$results <- renderTable({
    filtered() %>%
      select(Date, Visit_Site, Location)
  })
  
})