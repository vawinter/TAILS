rm(list = ls())
gc()

# Creating home ranges for foxs
library(dplyr)
library(lubridate)
library(amt)

# Read in data ----
# fox data and reference
x <- read.csv("Data/Red Fox (Vulpes vulpes) - Scotia, NSW, Australia-reference-data.csv")
x
fox <- read.csv("Data/Red Fox (Vulpes vulpes) - Scotia, NSW, Australia.csv")

# Filter individuals in 2017 with greater than 600 data pints
fox_choice <- fox %>% 
  filter(year(timestamp) %in% 2016) %>% 
  group_by(individual.local.identifier) %>% 
  summarise(n = length(location.long)) %>% 
  filter(n >= 600) 

# Filter individuals in reference file to see metadata
fox_check <- x %>% 
  filter(animal.id %in% fox_choice$individual.local.identifier) 

# visualize in table
table(x$animal.id, x$animal.sex, year(x$deploy.on.date))  

# Grab sex from reference data
sex <- x %>% 
  select(animal.id,
         animal.sex)

# Okay, not lets do some renaming
fox_choice <- fox %>% 
  filter(year(timestamp) %in% 2016) %>% 
  # select cols and rename
  select(animal.id = individual.local.identifier,
         y = location.lat,
         x = location.long,
         utm.y = utm.easting,
         utm.x = utm.northing,
         utm.zone,
         dt = timestamp) %>% 
  # join with ref data
  left_join(sex, by = "animal.id") %>% 
  # rename again
  rename(sex = animal.sex,
         id = animal.id) %>% 
  # filter time
  filter(hour(dt) %in% c(0, 6, 12, 18),
         month(dt) %in% 2:6) %>% 
  # change class of col
  mutate(x = as.numeric(x),
         y = as.numeric(y),
         dt = as.Date(dt)) %>% 
  # arrange asc date and time
  arrange(dt)

# make track for hr analysis
fox_track <- mk_track(tbl = fox_choice, .x = x, .y = y, .t = dt, 
                      id = id, crs = 23884)


# create hrs per fox
fox <- unique(fox_choice$id)
fox_mcp <- list()
fox_kde <- list()

# loop
for(i in 1:length(fox)){
  x <- fox_track %>% 
    filter(id == fox[i]) 
  
  fox_mcp[[i]] <- hr_mcp(x = x, 
                         levels = c(0.95, 0.5), 
                         keep.data = TRUE)
  
  # Fit KDE
  fox_kde[[i]] <- hr_kde(x = x,
                         levels = c(0.95, 0.5),
                         keep.data = TRUE,
                         h = hr_kde_ref(x), #default
                         trast = make_trast(x))
  
  
}

# name the list elements
names(fox_kde) <- names(fox_mcp) <- fox

# plot
plot(fox_kde[[1]])

# To extract the areas from all MCPs
mcp_areas <- lapply(fox_mcp, hr_area)
kde_areas <- lapply(fox_kde, hr_area)

kde_df <- bind_rows(kde_areas, .id = "id")
mcp_df <- bind_rows(mcp_areas, .id = "id")

#saveRDS(fox_choice, "Data/20221028_fox-data.rds")
