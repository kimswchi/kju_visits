library(rvest)
library(RJSONIO)
library(RCurl)

menu_url_base <- "http://rodong.rep.kp/en/index.php?strPageID=SF01_01_02&iMenuID=1&iSubMenuID="

article_info <- function(i) {
  menu_url <- paste0(menu_url_base,as.character(i))
  activities <- read_html(menu_url)
  
  news <- activities %>%
    html_nodes(".ListNewsLineContainer") %>%
    html_text() %>%
    as.data.frame()
  
  colnames(news) <- "news"
  news <- separate(news, news, into = c("title", "date"), sep = "\\[")
  news$date <- gsub("\\]", "", news$date)
  news <- separate(news, title, into=c("id", "title"), sep = "\\.")
  news$date <- gsub("\\.", "-", news$date)
  
  return(news)
}

#https://www.r-bloggers.com/using-google-maps-api-and-r/
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

news <- data.frame(matrix(vector(), 0, 3,
                               dimnames=list(c(), c("id", "title", "date"))),
                        stringsAsFactors=F)

for (i in 1:7) {
  news <- rbind(news, article_info(i))
}

news_filt <- news %>%
  na.omit() %>%
  filter(grepl("^Kim Jong Un ", title, perl=TRUE)) %>%
  filter(!grepl("Has Photo Session", title, perl=TRUE)) %>%
  filter(!grepl("Kim Jong Un Makes", title, perl=TRUE)) %>%
  filter(!grepl("Kim Jong Un Meets", title, perl=TRUE)) %>%
  filter(!grepl("Kim Jong Un Issues Order", title, perl=TRUE)) 
  

news_filt$title <- gsub("Farm No", "Farm No. 1116", news_filt$title)
news_filt$title <- gsub("Paektusan Hero Youth Power Station No", 
                        "Paektusan Hero Youth Power Station No. 3", news_filt$title)
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
news_filt$loc <- gsub("^ ", "", news_filt$loc)
news_filt$loc <- gsub(" $", "", news_filt$loc)

write.csv(news_filt, "news_filt.csv")
geo <- map(news_filt$loc, geoCode)
geo_df <- as.data.frame(geo) %>%
  t()

colnames(geo_df) <- c("lat", "lon", "prec", "loc2")
geo_df <- as_tibble(geo_df) %>%
  select(-prec) %>%
  mutate(nk = ifelse(!is.na(loc2) & 
                              grepl("North Korea", loc2, perl=TRUE), "nk", NA))
geo_df$lat[is.na(geo_df$nk)] <- NA
geo_df$lon[is.na(geo_df$nk)] <- NA
geo_df$loc2[is.na(geo_df$nk)] <- NA

geo_df <- geo_df %>%
  select(-nk)
  
geo_df$loc2 <- 
  
  filter(grepl("North Korea", loc2, perl=TRUE))

geoCode("Pyongyang")

article_links <- function(i) {
  menu_url <- paste0(menu_url_base,as.character(i))
  activities <- read_html(menu_url)
  #seq_start <- 25*(i-1)+1
  #seq_stop <- 25*(i-1)+25
  
  links <- html_attr(html_nodes(activities, "a"), "href") %>%
    as.data.frame()
  colnames(links) <- "link"
  links <- links %>%
    filter(grepl("javascript:article_open", link, perl=TRUE))
  links$link <- gsub("javascript:article_open\\('", "", links$link)
  links$link <- gsub("'\\)", "", links$link)
  #links$id <- sort(rep(seq(seq_start,seq_stop),2))
  links <- links %>%
    mutate(date_id = substr(link, 39,48)) %>%
    mutate(type = ifelse(grepl("_photo", link, perl=TRUE), "pic_link", "article_link"))
  #  spread(type, link)
  
  return(links)
}

url <- "http://rodong.rep.kp/en/index.php?strPageID=SF01_02_01&newsID=2016-12-26-0002"
test <- read_html(url)

test <- test %>%
  html_nodes(".ArticleContent") %>%
  html_text() %>%
  as.data.frame() %>%
test <- test[1,]


test <- "index.php?strPageID=SF01_02_02&newsID=2017-03-11"
index <- substr()
  as.Date(substr(test, 39, 16), format="%Y-%m-%d")


activities <- read_html("http://rodong.rep.kp/en/index.php?strPageID=SF01_01_02&iMenuID=1&iSubMenuID=1")

news <- activities %>%
  html_nodes(".ListNewsLineContainer") %>%
  html_text() %>%
  as.data.frame()

colnames(news) <- "news"
news <- separate(news, news, into = c("title", "date"), sep = "\\[")
news$date <- gsub("\\]", "", news$date)
news <- separate(news, title, into=c("id", "title"), sep = "\\.")
news$id <- as.numeric(news$id)

links <- html_attr(html_nodes(activities, "a"), "href") %>%
  as.data.frame()
colnames(links) <- "link"
links <- links %>%
  filter(grepl("javascript:article_open", link, perl=TRUE))
links$link <- gsub("javascript:article_open\\('", "", links$link)
links$link <- gsub("'\\)", "", links$link)
links$id <- sort(rep(seq(1,25),2))
links <- links %>%
  mutate(type = ifelse(grepl("_photo", link, perl=TRUE), "pic_link", "article_link")) %>%
  spread(type, link)

news <- left_join(news, links, by="id")


#sample article link
http://rodong.rep.kp/en/index.php?strPageID=SF01_02_01&newsID=2016-06-13-0001


  rebel$gname <- gsub("`", "", rebel$gname)
%>% 
  
  separate(news, into = c("title", "date"), sep = "/[")
  
  mutate(alphnum = ifelse(grepl("^[0-9A-Za-z]", term, perl=TRUE), term, NA)) %>%
  na.omit()
#activities <- read_html("http://rodong.rep.kp/en/index.php?strPageID=SF01_02_01&newsID=2017-03-11-0001")
