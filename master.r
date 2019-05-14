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

# Maximum Out of Pocket for Medical and Drug EHB Benefits (Total),
# In Network (Tier 1), Family
# The max out of pocket is the amount of money that the family would 
# have to pay before the insurance covers everything 100%
# Give me the TEHBInnTier1FamilyMOOP column
planAttributes %>% select(TEHBInnTier1FamilyMOOP)

# replace all "," with " " in TEHBInnTier1FamilyMOOP
# as example: given = $12,600 BUT wanted = $12600
planAttributes$TEHBInnTier1FamilyMOOP<- gsub(',', '', planAttributes$TEHBInnTier1FamilyMOOP)

# replace all "\\$" with " " in TEHBInnTier1FamilyMOOP
# as example: given = $12600 BUT wanted = 12600
planAttributes$TEHBInnTier1FamilyMOOP<- gsub('\\$', '', planAttributes$TEHBInnTier1FamilyMOOP)

# assign (and convert to numeric) the maximum out of pocket column to planAttributes$moop
planAttributes$moop<- as.numeric(planAttributes$TEHBInnTier1FamilyMOOP)

# fill the blank cells with 0
planAttributes$moop[is.na(planAttributes$moop)] <- 0

# plot histogram of moop
ggplot(planAttributes, aes(x = planAttributes$moop)) + geom_histogram()

# There’s a lot of plans in there that have a zero family MOOP. That’s not accurate. 
# I will only stick to plans that actually have a dollar amount.
moop <- subset(planAttributes, moop > 0)
ggplot(planAttributes, aes(x = planAttributes$moop)) + geom_histogram()

# I’m going to map this to see which states have the worst MOOP on average for a family. 
# I used a function that turns state abbreviations to a format 
# that choropleth can actually use.
?aggregate()
planAttributes <- aggregate(planAttributes$moop, list(planAttributes$StateCode), mean)

# converts states abbreviations
source("function_stateFromLower.R")

planAttributes$region <- stateFromLower(planAttributes$Group.1)
planAttributes$value <- planAttributes$x
choro = StateChoropleth$new(planAttributes)
choro$title = "Average Max Out of Pocket"
choro$set_num_colors(1)
myPalette <- colorRampPalette(brewer.pal(9, "Reds"))
choro$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
choro$ggplot_scale = scale_fill_gradientn(name = "MOOP", colours = myPalette(9))
choro$render()

moop14 <- subset(moop, BusinessYear == "2014")
dim(moop14)

moop15 <- subset(moop, BusinessYear == "2015")
dim(moop15)
