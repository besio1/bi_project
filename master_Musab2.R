library(ggplot2) 
library(readr) 
library(dplyr)
library(RColorBrewer)
library(DT)
library(choroplethrMaps)
library(choroplethr)


setwd("C:/dev/bi_project")

hc <- read.csv("PlanAttributes.csv", stringsAsFactors = FALSE)

hc <- subset(hc, BusinessYear == "2014")

hc <- subset(hc, DentalOnlyPlan == "No")

head(hc$SBCHavingDiabetesCoinsurance, 100)

hc$SBCHavingDiabetesCoinsurance<- gsub('\\$', '', hc$SBCHavingDiabetesCoinsurance)
hc$SBCHavingDiabetesCoinsurance<- gsub(',', '', hc$SBCHavingDiabetesCoinsurance)
hc$coinsurance <- as.numeric(hc$SBCHavingDiabetesCoinsurance)
hc$coinsurance[is.na(hc$coinsurance)] <- 0
head(hc$coinsurance, 100)

counts <- table(hc$coinsurance)
barplot(counts, main="the coinsurance for the sample SBC scenario of having diabetes",
        xlab="Dollar Amount")

df <- aggregate(hc$coinsurance, list(hc$StateCode), mean)
df

names(df) <- c("state", "coinsurance")
df

df$region<-stateFromLower(df$state)
df$value <- df$overall

summary(df)

coinsurance <- subset(hc, coinsurance > 0)


df <- aggregate(hc$coinsurance, list(hc$StateCode), mean)
df$region<-stateFromLower(df$Group.1)
df$value <- df$x
choro = StateChoropleth$new(df)
choro$title = "the coinsurance for the sample SBC scenario of having diabetes"
choro$set_num_colors(1)
myPalette <- colorRampPalette(brewer.pal(9, "Reds"))
choro$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
choro$ggplot_scale = scale_fill_gradientn(name = "Dollar amount", colours = myPalette(9))
choro$render()

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
  "<strong>%s</strong><br/>%s people / mi<sup>2</sup>",
  state.name, state.name
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
