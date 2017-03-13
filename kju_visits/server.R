library(tidyverse)
library(ggmap)
library(shiny)

#load location data
loc <- read.csv("kju_visits_loc.csv", 
                col.names = c("X", "id", "title", "Date", "Visit_Site", "Location", "lat", "lon"))
#replace "?"s under the Location column, for display purposes
loc$Location <- gsub("\\?", "Unknown location, North Korea", loc$Location)
loc$Date <- as.character(loc$Date)

#load links data
links <- read.csv("kju_visits_links.csv")
links$date <- as.character(links$date)

shinyServer(function(input, output, session) {

  output$Visit  <- renderText({
    
    input_date <- as.character(input$date)
    visit_site <- loc$Visit_Site[loc$Date == input_date]
    location <- loc$Location[loc$Date == input_date]
    
    start <- paste("On", input_date, "\nKJU was at")
    
    if (length(visit_site) > 1) {
      x <- paste(start, visit_site[1], "\nin", location[1],
                 "\nand", visit_site[2], "\nin", location[2])
    } 
    else if (length(visit_site) == 1) {
      x <- paste(start, visit_site, "\nin", location)
    }
    else {
      x <- "No data for this date"
    }
    x
  })
  
  output$Article  <- renderText({
    
    input_date <- as.character(input$date)
    base_url <- "http://rodong.rep.kp/en/"
    
    article <- links$article_link[links$date == input_date]
    
    if (length(article) > 1) {
      article_links <- paste(paste0(base_url, article[1]),"\n", 
                       paste0(base_url, article[2]))
    }
    else if (length(article) == 1) {
      article_links <- paste(paste0(base_url, article))
    }
    else {
      article_links <- "NA"
    }
    
    article_links
    
  })
  
  output$Pic  <- renderText({
    
    input_date <- as.character(input$date)
    base_url <- "http://rodong.rep.kp/en/"
    
    pic <- links$pic_link[links$date == input_date]

    if (length(pic) > 1) {
      pic_links <- paste(paste0(base_url, pic[1]), "\n",
                         paste0(base_url, pic[2]))
      
    }
    else if (length(pic) == 1) {
      pic_links <- paste(paste0(base_url, pic))
    }
    else {
      pic_links <- "NA"
    }
    
    pic_links
    
  })
  
  output$nkmap <- renderPlot({
    
    input_date <- as.character(input$date)
    longitude <- loc$lon[loc$Date == input_date]
    latitude <- loc$lat[loc$Date == input_date]
    
    longitude <- as.data.frame(longitude)
    latitude <- as.data.frame(latitude)
    coords <- cbind(longitude,latitude)
    colnames(coords) <- c("longitude", "latitude")
    
    map <- get_map(location = 'North Korea', source = "google", maptype = "hybrid",
                   crop = FALSE, zoom = 7)
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
    
  }
    
  )
  
  output$results <- renderTable({
    filtered <- loc %>%
      select(Date, Visit_Site, Location) %>%
      filter(as.Date(Date) >= input$dateRange[1],
             as.Date(Date) <= input$dateRange[2]
      )
    filtered
  })
  
})