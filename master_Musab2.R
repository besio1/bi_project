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
hc <- read.csv("PlanAttributes.csv", stringsAsFactors = FALSE)

#prints all the data in the columns which are in the business year 2014
#Just for checking the access to the data 
hc <- subset(hc, BusinessYear == "2014")
print(subset(hc, BusinessYear == "2014"))

#Just for checking the access to the data 
#prints all the data in the columns which the DentalOnlyPlan is equal "No" 
hc <- subset(hc, DentalOnlyPlan == "No")
print(subset(hc, DentalOnlyPlan == "No"))

#A quick glimpse and then some data cleaning of SBCHavingDiabetesCoinsurance (The dollar amount of the coinsurance for the sample SBC scenario of having diabetes)
#Coinsurance = Eine Mitversicherung ist die Beteiligung mehrerer Versicherungsunternehmen an der Versicherung desselben Risikos
#SBC= Summary of Benefits and Coverage Provides Clear and Consistent Information == allowing employers and employees to make apples-to-apples comparisons among plans, understand what is covered and what it costs
head(hc$SBCHavingDiabetesCoinsurance, 100)

# replace all "," with " " AND all "\\$" with " " in SBCHavingDiabetesCoinsurance
hc$SBCHavingDiabetesCoinsurance<- gsub('\\$', '', hc$SBCHavingDiabetesCoinsurance)
hc$SBCHavingDiabetesCoinsurance<- gsub(',', '', hc$SBCHavingDiabetesCoinsurance)
# assign (and convert to numeric) the column of SBCHavingDiabetesCoinsurance
hc$coinsurance <- as.numeric(hc$SBCHavingDiabetesCoinsurance)
# fill the blank cells with 0
hc$coinsurance[is.na(hc$coinsurance)] <- 0
head(hc$coinsurance, 100)



counts <- table(hc$coinsurance)
#barplot of coinsurance
barplot(counts, main="The coinsurance for the sample SBC scenario of having diabetes",
        xlab="Dollar Amount", ylab="Counts")

# plot histogram of coinsurance dollar amount (x axis) and count in (y axis)
ggplot(hc, aes(x = hc$coinsurance) + geom_histogram(color="green"))


#list aggregation of the mean of the coinsurance with the statecode and print it 
df <- aggregate(hc$coinsurance, list(hc$StateCode), mean)
df

#Set the title of the printed aggregated list above with new labels and print it 
names(df) <- c("state", "coinsurance")
df


#calling the states from the function stateFromLower 
df$region<-stateFromLower(df$state)
df$value <- df$overall

#get summary of the states, coinsurance and region (Length, Median, mean...)
summary(df)

#Subset of all the coinsurances above 0 
coinsurance <- subset(hc, coinsurance > 0)


#------------------------choro Map creation -------------------------------------
#Aggregate the coinsurance with statecodes 
df <- aggregate(hc$coinsurance, list(hc$StateCode), mean)
#calling the states from the function stateFromLower
# Group.i for by[[i]]. -> in this case first group "Group.1" is group of all states
df$region<-stateFromLower(df$Group.1)
df$value <- df$x

#generate new choro object
choro = StateChoropleth$new(df)

# set title to the new choro map
choro$title = "    The coinsurance for the sample SBC scenario of having diabetes"

# The number of colors to use on the map. A value of 0 uses a divergent scale
choro$set_num_colors(1)

# These functions are useful for converting hand-designed `sequential' or `diverging' color 
# here green levels set 
myPalette <- colorRampPalette(brewer.pal(9, "Greens"))

#fill value and border color 
choro$ggplot_polygon = geom_polygon(aes(fill = value), color = "black")
#legend 
choro$ggplot_scale = scale_fill_gradientn(name = "Dollar amount", colours = myPalette(9))

#show and visualizie the choro map 
choro$render()
#------------------------- END choro Map Functions ------------------------------------------------


coinsurance <- subset(coinsurance, BusinessYear == "2014")
dim(coinsurance)

coinsurance <- subset(coinsurance, BusinessYear == "2015")
dim(coinsurance)

library(tigris)
library(leaflet)
library(geojson)

states <- geojsonio::geojson_read("json/us-states.geojson", what = "sp")
class(states)

names(states)

m <- leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))

m %>% addPolygons()

bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("YlOrRd", domain = states$density, bins = bins)

myPalette <- colorRampPalette(brewer.pal(9, "Reds"))

m %>% addPolygons(
  fillColor = myPalette(9),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7)

m %>% addPolygons(
  fillColor = myPalette(9),
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



labels <- sprintf(
  paste0("State: ", state.name)
) %>% lapply(htmltools::HTML)

m <- m %>% addPolygons(
  fillColor = myPalette(9),
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
m

m %>% addLegend(pal = pal, values = myPalette(9), opacity = 0.7, title = "amount of dollar",
               position = "bottomright")
