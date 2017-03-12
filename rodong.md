# Homework 10: Build a Shiny application
Soo Wan Kim  
March 12, 2017  



#Kim Jong Un Going to Places:
##Tracking Kim Jong Un's visits around North Korea

The purpose of this R Markdown file is to show how I got the dataset used in the *Kim Jong Un Going to Places* Shiny app.

![](http://www.euronews.com/media/download/articlepix/recadree/north-korea-tensions-kim-binoculars-08031.jpg)

## Data sources

Information on dates and locations of Kim Jong Un's visits were scraped from the [website of Rodong Sinmun](http://www.rodong.rep.kp/en/), North Korea's official state newspaper. Exact locations, when known, were gathered via web search. Geographic coordinates for the locations were retrieved using the Google Maps API, accessed using code by [Jose Gonzalez at R-bloggers](https://www.r-bloggers.com/using-google-maps-api-and-r/).

## Packages

I used the `rvest` package to scrape the Rodong Sinmun website and the `RJSONIO` and `RCurl` packages to access the Google Maps API.


```r
library(tidyverse)
library(rvest)
library(RJSONIO)
library(RCurl)
```

## Scraping Rodong Sinmun

As of March 11, 2017, the section titled "Supreme Leader's Activity" had a total of 173 articles posted since January 2, 2016. The code below retrieves the article IDs, titles, and post dates and puts them in a tidy dataset.


```r
menu_url_base <- "http://rodong.rep.kp/en/index.php?strPageID=SF01_01_02&iMenuID=1&iSubMenuID="

#function to gather article IDs, titles, and post dates from each page
article_info <- function(i) {
  menu_url <- paste0(menu_url_base, as.character(i)) #get page url
  activities <- read_html(menu_url) #connect to url
  
  news <- activities %>%
    html_nodes(".ListNewsLineContainer") %>% #get relevant information
    html_text() %>%
    as.data.frame() #store in data frame
  
  colnames(news) <- "news"
  #separate string into titles and dates
  news <- separate(news, news, into = c("title", "date"), sep = "\\[")
  #separate title string into IDs and titles
  news <- separate(news, title, into=c("id", "title"), sep = "\\.")
  #clean date strings
  news$date <- gsub("\\]", "", news$date)
  news$date <- gsub("\\.", "-", news$date)
  
  return(news)
}

#initialize data frame
news <- data.frame(matrix(vector(), 0, 3,
                               dimnames=list(c(), c("id", "title", "date"))),
                        stringsAsFactors=F)

#bind data from each page into one data frame
for (i in 1:7) {
  news <- rbind(news, article_info(i))
}

#fix observations where the separate function got rid of part of the title due to a "." being present
news$title <- gsub("Farm No", "Farm No. 1116", news$title)
news$title <- gsub("Paektusan Hero Youth Power Station No", 
                        "Paektusan Hero Youth Power Station No. 3", news$title)
```

The code below filters the dataset to remove articles where Kim Jong Un is not visiting places but is carrying out some other activity, e.g. getting his picture taken.


```r
news_filt <- news %>%
  na.omit() %>%
  filter(grepl("^Kim Jong Un ", title, perl=TRUE)) %>%
  filter(!grepl("Has Photo Session", title, perl=TRUE)) %>%
  filter(!grepl("Kim Jong Un Makes", title, perl=TRUE)) %>%
  filter(!grepl("Kim Jong Un Meets", title, perl=TRUE)) %>%
  filter(!grepl("Kim Jong Un Issues Order", title, perl=TRUE)) 
```

The code below creates a new column where I try to isolate the visit site from the article title as much as possible. This makes it easier to compile the exact locations. 


```r
news_filt$loc <- news_filt$title
news_filt$loc <- gsub("Kim Jong Un Visits", "", news_filt$loc)
news_filt$loc <- gsub("Kim Jong Un Inspects", "", news_filt$loc)
news_filt$loc <- gsub("Kim Jong Un Provides Field Guidance to", "", news_filt$loc)
news_filt$loc <- gsub("Kim Jong Un Gives Field Guidance to", "", news_filt$loc)
news_filt$loc <- gsub("Newly Built", "", news_filt$loc)
news_filt$loc <- gsub("Newly-Built", "", news_filt$loc)
news_filt$loc <- gsub("Newly-built", "", news_filt$loc)
news_filt$loc <- gsub("under Construction", "", news_filt$loc)
news_filt$loc <- gsub("to Express Condolences over Demise of Fidel Castro Ruz ", 
                      "", news_filt$loc)

#remove stray spaces at the start or end of location names
news_filt$loc <- gsub("^ ", "", news_filt$loc)
news_filt$loc <- gsub(" $", "", news_filt$loc)
```

## Getting exact locations

I used `write.csv(news_filt, "news_filt.csv")` to write the data frame into a `.csv` file, and entered information on the exact locations (name of each site, city and/or province) manually after googling the rough location names under the `loc` column.

## Getting geographic coordinates 

The code below imports the new file containing exact locations, joins it to the old data set, uses the Google Maps API to get the latitude and longtitude for each location, and finally merges everything into one dataframe.


```r
#read in location data
loc_names <- read.csv("news_filt.csv") 
#join with old dataset
loc_names$id <- as.character(loc_names$id)
news_loc <- left_join(news_filt, loc_names, by="id")

#Use Google Maps API to get geographic coordinates of each place Kim Jong Un visited
##functions borrowed from here: https://www.r-bloggers.com/using-google-maps-api-and-r/

url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

geoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type <- x$results[[1]]$geometry$location_type
    formatted_address <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
  } else {
    return(c(NA,NA,NA, NA))
  }
}

#store coordinates in new data frame
geo <- map(news_loc$loc_geo, geoCode)
geo_df <- as.data.frame(geo) %>%
  t()
colnames(geo_df) <- c("lat", "lon", "prec", "loc2")
geo_df <- as_tibble(geo_df) %>%
  select(-prec)

#bind everything into one data frame
news_loc <- cbind(news_loc, geo_df) %>%
  select(-loc2)
```

Last step: export the data into a `.csv` file.


```r
write.csv(news_loc, "kju_visits.csv")
```


