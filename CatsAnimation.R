# WFS 597 - Programming for Ecologists
# R Session - Group Project
# Visualizing Animal Movement
# Veronica W., Morgan S., Karen B.
#updated 10/27/2022 - Morgan

library(tidyverse)
library(lubridate)
library(move)
library(moveVis)

#load cat data from Movebank as Movestack
cats <- getMovebankData("Feral cat (Felis catus) - Scotia, NSW", includeExtraSensors = TRUE)
# https://cran.r-project.org/web/packages/move/index.html

#subsetting cats dataset

#load in csv from raw data
x <- read.csv("Data/Feral cat (Felis catus) - Scotia, NSW-reference-data.csv") #reference data
cat <- read.csv("Data/Feral cat (Felis catus) - Scotia, NSW.csv") #cat movement data

#filter cats for only year 2017 with more than 730 gps locations (2x daily)
cat_choice <- cat %>% 
  filter(year(timestamp) %in% 2017) %>% 
  group_by(individual.local.identifier) %>% 
  summarise(n = length(location.long)) %>% 
  filter(n >= 730) 

#list of cats to use
cat_list <- cat_choice$individual.local.identifier

#subset movestack by cat names
subcats <- cats[[cat_list]]

#subset movestack by year
sub2017cats <- subcats[year(timestamps(subcats))==2017] 

#check timestamps
min(timestamps(sub2017cats))
max(timestamps(sub2017cats))

# align data to a uniform time scale, resample 2x daily
mC <- align_move(sub2017cats, res = 12, unit = "hours")

#colors for movement tracks
n_colors <- 8
palette1 <- rainbow(n_colors)

# create spatial frames with a OpenStreetMap watercolour map #change basemap? #reproject map?
frames <- frames_spatial(mC, path_colours = palette1,
                         #tail_colour = "white", tail_size = 0.8 #how to include tail
                         #trace_show = TRUE, trace_colour = "white", #how to include path trace
                         map_service = "osm", map_type = "watercolor", alpha = 0.5) %>% 
  add_labels(x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  add_northarrow() %>% 
  add_scalebar() %>% 
  add_timestamps(type = "label") %>% 
  add_progress()

frames[[100]] # preview one of the frames, e.g. the 100th frame

# animate frames
animate_frames(frames, out_file = "catVis.gif")

