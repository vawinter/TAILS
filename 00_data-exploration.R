library(moveVis)
library(move)
library(ggplot2)

data("move_data", package = "moveVis") # move class object
# if your tracks are present as data.frames, see df2move() for conversion

# align move_data to a uniform time scale
m <- align_move(move_data, res = 4, unit = "mins")

# create spatial frames with a OpenStreetMap watercolour map
frames <- frames_spatial(m, path_colours = c("red", "green", "blue"),
                         map_service = "osm", map_type = "watercolor", alpha = 0.5) %>% 
  add_labels(x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  add_northarrow() %>% 
  add_scalebar() %>% 
  add_timestamps(type = "label") %>% 
  add_progress()

frames[[100]] # preview one of the frames, e.g. the 100th frame

# animate frames
animate_frames(frames, out_file = "moveVis.gif")

x <- read.csv("Data/Feral cat (Felis catus) - Scotia, NSW-reference-data.csv")
x
cat <- read.csv("Data/Feral cat (Felis catus) - Scotia, NSW.csv")
fox <- read.csv("Data/Red Fox (Vulpes vulpes) - Scotia, NSW, Australia.csv")


cat_choice <- cat %>% 
  filter(year(timestamp) %in% 2017) %>% 
  group_by(individual.local.identifier) %>% 
  summarise(n = length(location.long)) %>% 
  filter(n >= 600) 
  
cat_check <- x %>% 
  filter(animal.id %in% cat_choice$individual.local.identifier) 

table(x$animal.id, x$animal.sex, year(x$deploy.on.date))  
  
  