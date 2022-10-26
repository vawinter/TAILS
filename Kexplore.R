# Karen exploring the data and learning the visualization tools

# Pull data using 'australiaScript.R'


# Exploring MoveVis package -----------------------------------------------

# working through vignettes on MoveVis.org

library(moveVis)
library(leaflet)
library(mapview)

data("move_data", package = "moveVis")
head(unique(timestamps(move_data))) #check timestamps
head(timeLag(move_data, unit = "mins")) #check timestamp intervals
m <- align_move(move_data, res= 4, unit = "mins") #align timestamps/intervals across individuals
head(unique(timestamps(m))) #check single timestamp for whole data set
head(timeLag(m, unit = "mins")) #check equal time intervals
view_spatial(m, path_colours = c("indianred", "palegreen3", "cyan3"), ) #displays tracks on interactive mapview or leaflet map
get_maptypes() #list of map type options
frames <- frames_spatial(m, path_colours = c("red", "green", "blue"), #instead of path_colours, could add a colour column to move/moveStack object
                         #tail_colour = "white", tail_size = 0.8 #how to include tail
                         #trace_show = TRUE, trace_colour = "white", #how to include path trace
                         map_service = "osm", map_type = "watercolor", #map distorted? can reproject mvmt data and disable default map modifications; see post-script
                         alpha = 0.5) %>% 
  add_labels(x = "Longitude", y = "Latitude") %>%
  add_northarrow() %>%
  add_scalebar() %>%
  add_timestamps(type = "label") %>%
  add_progress()
frames[[11]] #frames can be individually plotted using ggplot2
animate_frames(frames, out_file = "moveVisExample.gif") # can specify width, height, res
frames_graph() #frames displaying movement-environment interaction graphs
join_frames() #could allow side-by-side of cats vs foxes
rev() #reverse order of frames

# SEE CODE SNIPPETS SECTION FOR COOL MAPPING IDEAS
# can color movement tracks by individual, behavior, sex, etc.

move_data <- sp::spTransform(move_data, crs=3857) #to reproject mvmt data to map crs
frames_spatial(..., equidistant = F)


# Exploring Leaflet package -----------------------------------------------

# working through vignettes on https://rstudio.github.io/leaflet/

library(leaflet)

map <- leaflet() %>%
  addTiles() #default OpenStreetMap
# accepts sp class objects
# can include 'attribution = "xyz"'
# set customized point markers (see 'Markers' tab)


# Exploring DynamoVis package ---------------------------------------------

# working through vignettes on https://dynamovis.geog.ucsb.edu/index
