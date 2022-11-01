# WFS 597 - Programming for Ecologists
# R Session - Group Project
# Visualizing Animal Movement
# Veronica W., Morgan S., Karen B.
#updated 10/28/2022 - Karen

library(move)
library(tidyverse)
library(lubridate)
library(moveVis)


# Load fox & cat data directly from MoveBank
mblogin <- movebankLogin(username = Sys.getenv("mbus"), password = Sys.getenv("mbpw"))
fox <- getMovebankData("Red Fox (Vulpes vulpes) - Scotia, NSW, Australia", includeExtraSensors = TRUE, login = mblogin)
cats <- getMovebankData("Feral cat (Felis catus) - Scotia, NSW", includeExtraSensors = TRUE, login = mblogin)

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

# Resample data to 6-hr intervals and align times for matching sequenced animation
moveFox <- align_move(subFox, res = 6, unit = "hours")

# #filter cats for only year 2017 with more than 730 gps locations (2x daily)
# cat_choice <- cat %>% 
#   filter(year(timestamp) %in% 2017) %>% 
#   group_by(individual.local.identifier) %>% 
#   summarise(n = length(location.long)) %>% 
#   filter(n >= 730) 
# 
# # Display locations on interactive map
# view_spatial(moveFox, path_colours = colChoice)
# 
# # Create animated movement path
# framesFox <- frames_spatial(moveFox, path_colours = colChoice, # Can't figure out how to match tail colour to path colour by individual
#                             trace_show = TRUE, trace_colour = 'white',
#                             map_service = 'osm', map_type = 'watercolor',
#                             alpha = 0.5) %>% 
#   add_labels(x = "Longitude", y = "Latitude") %>%
#   add_northarrow() %>%
#   add_scalebar() %>%
#   add_timestamps(type = "label") %>%
#   add_progress()
# # %>% add_gg(gg = expr(list(xlim(c(140.0, 142.0)), ylim(c(-30.0, -34.0))))) # Can't figure out how to manually change the map extent
# animate_frames(framesFox, out_file = "foxVis.gif")
