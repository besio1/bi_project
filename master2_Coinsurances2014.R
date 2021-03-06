library(ggplot2) 
library(readr) 
library(dplyr)
library(RColorBrewer)
library(DT)
library(choroplethrMaps)
library(choroplethr)


##Setwd for omar besic 
##setwd("C:/Users/omarb/Desktop/Studium/6. Semester/
##      06_Business Intelligence im Spital/Projekt/bi_project")

##Setwd for musab elkour 
setwd("C:/dev/bi_project")

#import data with help of the readr package and reads comma delimited files
pa <- read.csv("PlanAttributes.csv", stringsAsFactors = FALSE)


#prints all the data in the columns which are in the business year 2014
#Just for checking the access to the data 
#subset and filter the data just for the buisness year 2014
pa <- subset(pa, BusinessYear == "2014")
print(subset(pa, BusinessYear == "2014"))


#A quick glimpse and then some data cleaning of SBCHavingDiabetesCoinsurance (The dollar amount of the coinsurance for the sample SBC scenario of having diabetes)
#Coinsurance = Eine Mitversicherung ist die Beteiligung mehrerer Versicherungsunternehmen an der Versicherung desselben Risikos
#SBC= Summary of Benefits and Coverage Provides Clear and Consistent Information == allowing employers and employees to make apples-to-apples comparisons among plans, understand what is covered and what it costs
head(pa$SBCHavingDiabetesCoinsurance, 100)

# replace all "," with " " AND all "\\$" with " " in SBCHavingDiabetesCoinsurance
pa$SBCHavingDiabetesCoinsurance<- gsub('\\$', '', pa$SBCHavingDiabetesCoinsurance)
pa$SBCHavingDiabetesCoinsurance<- gsub(',', '', pa$SBCHavingDiabetesCoinsurance)
# assign (and convert to numeric) the column of SBCHavingDiabetesCoinsurance
pa$coinsurance <- as.numeric(pa$SBCHavingDiabetesCoinsurance)
# fill the blank cells with 0
pa$coinsurance[is.na(pa$coinsurance)] <- 0
head(pa$coinsurance, 100)

#subset and filter all coinsurances which equals 0 (remove all 0 values)
pa <- subset(pa, coinsurance != 0)


##set data of coinsurance in a table for the barplot 
counts <- table(pa$coinsurance)
#barplot of coinsurance
barplot(counts, main="Deckungskosten der Mitversicherung f�r Diabeteserkrankte in 2014",
        xlab="Dollar Amount", ylab="Counts", col="green")

# plot histogram of coinsurance dollar amount (x axis) and count in (y axis)
ggplot(pa, aes(x = pa$coinsurance)) + geom_histogram(col="black", fill="green")


#list aggregation of the mean of the coinsurance with the statecode and print it 
df <- aggregate(pa$coinsurance, list(pa$StateCode), mean)
df

#Set the title of the printed aggregated list above with new labels and print it 
names(df) <- c("state", "coinsurance")
df


# converts states abbreviations
source("function_stateFromLower.R")
#calling the states from the function stateFromLower 
df$region<-stateFromLower(df$Group.1)
df$value <- df$overall

if(stateFromLower(df$Group.1) != pa$StateCode) {
  stateFromLower(df$Group.1) == "NA"
}

#get summary of the states, coinsurance and region (Length, Median, mean...)
summary(df)

#Subset of all the coinsurances above 0 
coinsurance <- subset(pa, coinsurance > 0)


#------------------------choro Map creation -------------------------------------
#Aggregate the coinsurance with statecodes 
df <- aggregate(pa$coinsurance, list(pa$StateCode), mean)
#calling the states from the function stateFromLower
# Group.i for by[[i]]. -> in this case first group "Group.1" is group of all states
df$region<-stateFromLower(df$Group.1)
df$value <- df$x

#generate new choro object
choro = StateChoropleth$new(df)

# set title to the new choro map
choro$title = "    Deckungskosten der Mitversicherung f�r Diabeteserkrankte in 2014"

# The number of colors to use on the map. A value of 0 uses a divergent scale
choro$set_num_colors(1)

# These functions are useful for converting hand-designed `sequential' or `diverging' color 
# here green levels set 
myPalette <- colorRampPalette(brewer.pal(9, "Blues"))

#fill value and border color 
choro$ggplot_polygon = geom_polygon(aes(fill = value), color = "black")
#legend 
choro$ggplot_scale = scale_fill_gradientn(name = "Dollar amount", colours = myPalette(9))

#show and visualizie the choro map 
choro$render()
#------------------------- END choro Map Functions ------------------------------------------------

#subset the data for reasons that will become clear. Here use the 2014 and 2015 data.
#to check if the coinsurance has gotten higher or not 
coinsurance <- subset(coinsurance, BusinessYear == "2014")
dim(coinsurance)

coinsurance <- subset(coinsurance, BusinessYear == "2015")
dim(coinsurance)


#----------------- geograph --------
library(tigris)
library(leaflet)
library(geojson)

#In this case, we will use the geojsonio package to load the data into sp objects, which will let us easily manipulate the geographic features, and their properties, in R.
# transfrom .json file into a spatial polygons data frame
states <- geojsonio::geojson_read(x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json"
                                  , what = "sp")
class(states)
names(states)

#a basic map with just the outline of the states
map <- leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))

#We've saved the basic basemap as a separate variable m so we can easily iterate on the addPolygons call 
#call addPolygons with no additional arguments -- To add uniform polygons with default styling
map %>% addPolygons()

#First, we'll define the bins. This is a numeric vector that defines the boundaries between intervals 
bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)

#Then, we'll call colorBin to generate a palette function that maps the RColorBrewer "YlOrRd" colors to our bins.
pal <- colorBin("YlGnBu", domain = pa$coinsurance, bins = bins)


#Adding Color to the map 
#Finally, we'll modify addPolygons to use the palette function to generate a vector of colors for fillColor, and also add some other static style properties.
map %>% addPolygons(
  fillColor = ~pal(pa$coinsurance),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7)

#Adding interaction 
#make the polygons highlight as the mouse passes over them. The addPolygon function has a highlight argument that makes this simple
map %>% addPolygons(
  fillColor = ~pal(pa$coinsurance),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE))


#Custom infos 
#We'll generate the labels by handcrafting some HTML, and passing it to lapply(htmltools::HTML) so that Leaflet knows to treat each label as HTML instead of as plain text. We'll also set some label options to improve the style of the label element itself.
labels <- sprintf(
 
         "<br /> Businessyear: ", pa$BusinessYear
         
  )  %>% lapply(htmltools::HTML)

map <- map %>% addPolygons(
  fillColor = ~pal(pa$coinsurance),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto"))

#illustrate the map with this line 
map


#add a legend. Because we chose to color our map using colorBin, the addLegend function makes it particularly easy to add a legend with the correct colors and intervals.
map %>% addLegend(pal = pal, values = ~pal(pa$coinsurance), opacity = 0.7, title = "amount of dollar",
               position = "bottomright")
