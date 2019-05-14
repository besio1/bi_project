library(ggplot2)
library(readr)
library(dplyr) 
library(choroplethr)
library(extrafont)
library(extrafontdb)
library(RColorBrewer)
library(scales)
library(gridExtra)
library(choroplethrMaps)

# import data
planAttributes <- read.csv("PlanAttributes.csv", stringsAsFactors = FALSE)

# overview of the 50 first rows
head(planAttributes$TEHBInnTier1FamilyMOOP, 50)

planAttributes$TEHBInnTier1FamilyMOOP<- gsub(',', '', planAttributes$TEHBInnTier1FamilyMOOP)
planAttributes$TEHBInnTier1FamilyMOOP<- gsub('\\$', '', planAttributes$TEHBInnTier1FamilyMOOP)
planAttributes$moop<- as.numeric(planAttributes$TEHBInnTier1FamilyMOOP)
planAttributes$moop[is.na(planAttributes$moop)] <- 0
ggplot(planAttributes, aes(x = planAttributes$moop)) + geom_histogram()


# There’s a lot of plans in there that have a zero family MOOP. That’s not accurate. 
# I will only stick to plans that actually have a dollar amount.
moop <- subset(planAttributes, moop > 0)

# I’m going to map this to see which states have the worst MOOP on average for a family. 
# I used a function that turns state abbreviations to a format that choropleth can actually use.
df <- aggregate(planAttributes$moop, list(planAttributes$StateCode), mean)

source("function_stateFromLower.R")
df$region<-stateFromLower(df$Group.1)
df$value <- df$x
choro = StateChoropleth$new(df)
choro$title = "Average Max Out of Pocket"
choro$set_num_colors(1)
myPalette <- colorRampPalette(brewer.pal(9, "Reds"))
choro$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
choro$ggplot_scale = scale_fill_gradientn(name = "MOOP", colours = myPalette(9))
choro$render()



