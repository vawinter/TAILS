rm(list = ls())
gc()

# Creating home ranges for cats
library(dplyr)
library(lubridate)
library(amt)

# Read in data ----
# Cat data and reference
x <- read.csv("Data/Feral cat (Felis catus) - Scotia, NSW-reference-data.csv")
x
cat <- read.csv("Data/Feral cat (Felis catus) - Scotia, NSW.csv")

# Filter individuals in 2017 with greater than 600 data pints
cat_choice <- cat %>% 
  filter(year(timestamp) %in% 2017) %>% 
  group_by(individual.local.identifier) %>% 
  summarise(n = length(location.long)) %>% 
  filter(n >= 600) 
  
# Filter individuals in reference file to see metadata
cat_check <- x %>% 
  filter(animal.id %in% cat_choice$individual.local.identifier) 

# visualize in table
table(x$animal.id, x$animal.sex, year(x$deploy.on.date))  

# Grab sex from reference data
sex <- x %>% 
  select(animal.id,
         animal.sex)

# Okay, not lets do some renaming
cat_choice <- cat %>% 
  filter(year(timestamp) %in% 2017) %>% 
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
         month(dt) %in% 8:11) %>% 
  # change class of col
  mutate(x = as.numeric(x),
         y = as.numeric(y),
         dt = as.Date(dt)) %>% 
  # arrange asc date and time
  arrange(dt)

# make track for hr analysis
cat_track <- mk_track(tbl = cat_choice, .x = x, .y = y, .t = dt, 
                      id = id, crs = 23884)


# create hrs per cat
cat <- unique(cat_choice$id)
cat_mcp <- list()
cat_kde <- list()

# loop
for(i in 1:length(cat)){
  x <- cat_track %>% 
    filter(id == cat[i]) 
  
    cat_mcp[[i]] <- hr_mcp(x = x, 
                    levels = c(0.95, 0.5), 
                    keep.data = TRUE)
    
    # Fit KDE
    cat_kde[[i]] <- hr_kde(x = x,
                       levels = c(0.95, 0.5),
                       keep.data = TRUE,
                       h = hr_kde_ref(x), #default
                       trast = make_trast(x))
    

}

# name the list elements
names(cat_kde) <- names(cat_mcp) <- cat

# plot
plot(cat_kde[[3]], main = "Ken")

# To extract the areas from all MCPs
mcp_areas <- lapply(cat_mcp, hr_area)
kde_areas <- lapply(cat_kde, hr_area)

kde_df <- bind_rows(kde_areas, .id = "id")
mcp_df <- bind_rows(mcp_areas, .id = "id")

saveRDS(cat_kde, "DataVis/20221031_cat-kde.rds")
saveRDS(kde_df, "DataVis/20221031_cat_hr-df.rds")

  
  