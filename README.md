#hw10

##Kim Jong Un Going to Places
###Tracking Kim Jong Un's Official Visits Around North Korea

####App Description

This Shiny app tracks Kim Jong Un's official visits around North Korea as reported by North Korea's official state news agency. Users can input single dates and/or a range of dates to see where Kim was on a specific date and/or a range of dates. 

User inputs:

	* Single date (options: any date between January 2, 2016 and March 11, 2017)
	* Date range (options: any date range between January 2, 2016 and March 11, 2017)
	* Map type (options: No map, Single day, Date range)

Output/features:

	* Summary of Kim's visits on selected date
	* Links to news articles and photos of visits on selected date
	* Interactive map of Kim's visits
		- Single date: marks Kim's location with a star
		- Date range: marks Kim's visits with circles, size determined by relative frequency of visit
	* List of Kim's visits during selected date range with date, visit site, and location

####Making the App

To obtain information on Kim's visits, I scraped the [Rodong Sinmun website](http://rodong.rep.kp/en/) for dates and titles of articles reporting his activities. I augmented the data with information on exact locations of each visit site, when available, found by searching on Google. I then added geographic coordinates to the data for making the interactive map. The coordinates were retrieved via Google Maps API using code by [Jose Gonzalez at R-bloggers](https://www.r-bloggers.com/using-google-maps-api-and-r/). This approach requires the `RJSONIO` and `RCurl` paackages. 

Other packages used include `ggmap` for making the map and `shinythemes` for setting the theme of the app ("United").

####Links

	* [Shiny app](https://kimswchi.shinyapps.io/kju_visits/)
	* [Replication files](replication files)
		- [R Markdown file](replication files/Rodong.Rmd)
		- [Markdown file](replication files/Rodong.md)
	* [App files](kju_visits) 
