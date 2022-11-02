# WFS 597 - Programming for Ecologists
# R Session - Group Project
# Visualizing Animal Movement
# Veronica W., Morgan S., Karen B.
#updated 10/27/2022 - Morgan

library(tidyverse)
library(lubridate)
library(move)
library(moveVis)
library(RColorBrewer)
library(leaflet)

#load cat data from Movebank as Movestack
cats <- getMovebankData("Feral cat (Felis catus) - Scotia, NSW", includeExtraSensors = TRUE)
# https://cran.r-project.org/web/packages/move/index.html
#username: mbstum
#password: Cq6j9m4KrQRQGJ@

#subsetting cats dataset

#load in csv from raw data
#x <- read.csv("Data/Feral cat (Felis catus) - Scotia, NSW-reference-data.csv") #reference data
#cat <- read.csv("Data/Feral cat (Felis catus) - Scotia, NSW.csv") #cat movement data

#unique(cat$individual.local.identifier)

#filter cats for only year 2017 with more than 730 gps locations (2x daily)
#cat_choice <- cat %>% 
  #filter(year(timestamp) %in% 2017) %>% 
  #group_by(individual.local.identifier) %>% 
  #summarise(n = length(location.long)) %>% 
  #filter(n >= 730) 


#subset cats for only aug - nov
#list of cats to use
cat_list <- c("Ben_MC236", "Ivy_FC489", "Ken_MC536", "Ray_MC729", "Roy_MC769" )

#subset movestack by cat names
subcats <- cats[[cat_list]]

#subset movestack by year
subcats <- subcats[year(timestamps(subcats))==2017] 

#subset movestack by months aug - nov
subcats <- subcats[month(timestamps(subcats))%in% c(8,9,10,11)]

#check timestamps
min(timestamps(subcats))
max(timestamps(subcats))

# align data to a uniform time scale, resample 4x daily
mC <- align_move(subcats, res = 6, unit = "hours")

#colors for movement tracks
col = brewer.pal(n = 5, name = "Set2")

# create spatial frames with a OpenStreetMap watercolour map #change basemap? #reproject map?
frames <- frames_spatial(mC, path_colours = col,
                         tail_colour = "white", tail_size = 0.8, #how to include tail
                         trace_show = TRUE, trace_colour = "darkgray", #how to include path trace
                         map_service = 'osm', map_type = "terrain",
                         alpha = 0.5) %>% 
  add_labels(x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  add_northarrow() %>% 
  add_scalebar() %>% 
  add_timestamps(type = "label") %>% 
  add_progress()

frames[[100]] # preview one of the frames, e.g. the 100th frame

#view locations on static interactive map
view_spatial(mC, path_colours = col, render_as = "leaflet")
# animate frames
animate_frames(frames, out_file = "catVis2.gif", overwrite = TRUE)

