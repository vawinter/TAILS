# WFS 597 - Programming for Ecologists
# R Session - Group Project
# Visualizing Animal Movement
# Veronica W., Morgan S., Karen B.
#updated 10/28/2022 - Karen

library(move)
library(tidyverse)
library(lubridate)
library(moveVis)

# Load fox data & reference file directly from MoveBank
mblogin <- movebankLogin(username = Sys.getenv("mbus"), password = Sys.getenv("mbpw"))
fox <- getMovebankData("Red Fox (Vulpes vulpes) - Scotia, NSW, Australia", includeExtraSensors = TRUE, login = mblogin)
foxRef <- getMovebankReferenceTable("Red Fox (Vulpes vulpes) - Scotia, NSW, Australia", login = mblogin)

# Filter data set to only individuals with observations in 2017
foxChoice <- fox %>%
  as.data.frame() %>%
  filter(timestamp >= "2017-08-01", timestamp < "2018-12-01") %>% 
  group_by(local_identifier) %>% 
  summarise(n = length(location_long)) %>% 
  print() # aiming for inds w/ > 730 locs; all inds fit this qualification
foxList <- foxChoice$local_identifier

# Subset main data set to only the individuals and locations in 2017 Aug-Nov
subFox <- fox[[foxList]]
subFox <- subFox[year(timestamps(subFox)) == 2017]
subFox <- subFox[month(timestamps(subFox)) %in% c(8, 9, 10, 11)]

# Confirm timestamps and ids in specified range
range(timestamps(subFox)) #    PROBLEM :: time range 7/31-11/21
unique(subFox@idData$local_identifier)

# Resample data to 6-hr intervals and align times for matching sequenced animation
head(timeLag(subFox, unit = "mins"), n=2) #check timestamp intervals; currently 20 mins
moveFox <- align_move(subFox, res = 6, unit = "hours")

# Set colors for individual locations/movement path
library(RColorBrewer)
colChoice <- brewer.pal(n = 9, name= 'Set1')

# Display locations on interactive map
view_spatial(moveFox, path_colours = colChoice)

# Create animated movement path
framesFox <- frames_spatial(moveFox, path_colours = colChoice, # Can't figure out how to match tail colour to path colour by individual
                            trace_show = TRUE, trace_colour = 'white',
                            map_service = 'osm', map_type = 'watercolor',
                            alpha = 0.5) %>% 
  add_labels(x = "Longitude", y = "Latitude") %>%
  add_northarrow() %>%
  add_scalebar() %>%
  add_timestamps(type = "label") %>%
  add_progress()
  # %>% add_gg(gg = expr(list(xlim(c(140.0, 142.0)), ylim(c(-30.0, -34.0))))) # Can't figure out how to manually change the map extent
framesFox[[11]]
animate_frames(framesFox, out_file = "foxVis.gif")
