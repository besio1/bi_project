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

##setwd("C:/Users/omarb/Desktop/Studium/6. Semester/
##      06_Business Intelligence im Spital/Projekt/bi_project")

# import data with help of the readr package and reads comma delimited files
planAttributes <- read.csv("PlanAttributes.csv", stringsAsFactors = FALSE)

# planAttributes <- planAttributes %>% filter(planAttributes$BusinessYear == 2014)
# planAttributes <- planAttributes %>% filter(planAttributes$BusinessYear == 2016)

# Maximum Out of Pocket for Medical and Drug EHB Benefits (Total),
# In Network (Tier 1), Family
# The max out of pocket is the amount of money that the family would 
# have to pay before the insurance covers everything 100%
# Give me the TEHBInnTier1FamilyMOOP column just for a short glimpse
planAttributes <- planAttributes %>% select(TEHBInnTier1FamilyMOOP)

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

# choroplethr is used for plotting maps
# I’m going to map this to see which states have the worst MOOP on average for a family. 
# I used a function that turns state abbreviations to a format 
# that choropleth can actually use.
# aggregate from planAttributes$moop and group by list(planAttributes$StateCode), take mean
planAttributes <- aggregate(planAttributes$moop, list(planAttributes$StateCode), mean)

# converts states abbreviations
source("function_stateFromLower.R")

# For the data frame method, a data frame with columns corresponding to the grouping variables 
# in by followed by aggregated columns from x. If the by has names, the non-empty times 
# are used to label the columns in the results, with unnamed grouping variables being named 
# Group.i for by[[i]]. -> in this case first group "Group.1" is group of all states
planAttributes$region <- stateFromLower(planAttributes$Group.1)

# assign planAttributes$x (means of moop by state) to planAttributes$value
planAttributes$value <- planAttributes$x

# generate new choro object
?StateChoropleth
choro = StateChoropleth$new(planAttributes)

# set title to the new choro chart
choro$title = "Average Max Out of Pocket"

# The number of colors to use on the map. A value of 0 uses a divergent scale
# (useful for visualizing negative and positive numbers), A value of 1 uses a continuous
# scale (useful for visualizing outliers), and a value in [2, 9] will use that many quantiles.
choro$set_num_colors(1)

# These functions are useful for converting hand-designed `sequential' or `diverging' color 
# schemes into continous color ramps eg for image and filled contour plots. 
myPalette <- colorRampPalette(brewer.pal(9, "Reds"))

choro$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
choro$ggplot_scale = scale_fill_gradientn(name = "MOOP", colours = myPalette(9))
choro$render()
