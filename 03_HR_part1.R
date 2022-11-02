#########################################X
#-----WILD 6900 -- Space-use Ecology-----X
#-------Lab 3: Home Ranges Part 1--------X
#########################################X

# In this lab, we'll get to work with our unicorn
# GPS collar data. Since this is our first time
# diving into our data, we're also going to look
# at some code for efficiently loading in a lot
# of individual CSV files, which are how most
# GPS collars will store their data.

# Once we load the data, we'll look at a couple of
# simple home range estimators. Here's an outline
# for this lab:
#   1. Load GPS collar files
#   2. Prep spatial data with 'amt'
#   3. Fit home ranges for 1 individual
#     a. Fit MCP home ranges
#     b. Fit KDE home ranges
#     c. Fit LoCoH home ranges ******************* Will prob need this
#   4. Fit home ranges for multiple individuals
#   5. Compare estimators with a table

#Load packages----
library(tidyverse)
library(lubridate)
library(amt)
library(raster)

#1. Load collar data ----
# ... GPS Telemetry data ----
# Most people have their GPS data as separate CSV files
# that come from each collar. Here, we will see how to
# write a loop to load in all of our data files and combine
# them in a single data.frame.

#Here's the approach:
#   i. Identify the path where your collar data are stored
#   ii. Create a vector of all the CSV files in that directory
#   iii. Declare a blank data.frame to hold your results
#   iv. Loop through all files in the directory:
#     a. Load each one
#     b. Subset and/or reformat columns
#     c. Combine with results data.frame

# Step i. Path to collar data
# We can specify our path either relative to our working
# directory, or using an absolute path. You might want to
# use an absolute path if your files are stored on a server
# somewhere (like if you work for an agency) or if they are
# in a folder very far away from where you do your analysis.
# Here, we will use a relative path.
path_to_collar <- "../data/collars/"

# Step ii. Vector of CSV files
# R  has base functions that can list the files or folders
# in any directory. For more details, see the help file here:
?dir
# We just need to list all the files in our collars folder,
# but be aware that you can also supply a text pattern to the
# function if you only want some of the files.
files <- list.files(path = path_to_collar)

# Step iii. Declare blank data.frame
unicorn <- data.frame()

# Step iv. Loop to load files
# We will use a "for()" loop here. For loops are common in
# many different programming languages for iterating code.
# By convention, we'll use the variable name "i" to keep
# track of which iteration we're on. i will take on any
# values specified after "in", which in our case, is a
# sequence from 1 to as many files as we have.
# All the code within the curly brackets {} will be run
# with i = 1, and when it is complete, it will start over
# with i = 2. This will repeat until we have iterated over
# all the values in our sequence.

for (i in 1:length(files)) {  # PUT THIS IN YOUR SCRIPT
  # Print the status
  # Note the special character "\n" is a line break
  cat(paste0("Reading file ", i, " of ", length(files), "\n"))
  # Paste together path and file
  full_path <- paste0(path_to_collar, files[i])
  # Read the CSV to a temporary object (overwritten each iteration)
  temp <- read.csv(full_path)
  # Format "dt" as POSIXct
  temp$dt <- ymd_hms(temp$dt) 
  # Create ID column
  temp$uid <- rep(paste0("U", i), nrow(temp))
  # Combine in final data.frame
  unicorn <- rbind(unicorn, temp)
}

# Check the results
unique(unicorn$uid) # make sure we got everyone
table(unicorn$uid) # locations per individual
str(unicorn) # make sure all the columns are correct

unicorn <- read.csv("Data/Feral cat (Felis catus) - Scotia, NSW.csv")

# Looks good! We'd normally be ready to work with it.
# However, one of the algorithms we'll see for the
# home ranges can take a long time to fit. So
# for the sake of computational speed (not ecology),
# we're going to subset our data first.

# Instead of keeping all 24 hourly locations per day,
# we'll just keep 4. Let's keep 6am, noon, 6pm, and midnight.
# We'll also just keep the months August -- October
# Again, this not question-based, but just an arbitrary
# choice for the sake of speeding up this lab.
unicorn <- unicorn %>%  
  dplyr::rename(dt = timestamp,
         uid = individual.local.identifier) %>% 
# USE THIS TOO - can see most common at diff hours to grab all
  filter(hour(dt) %in% c(0, 6, 12, 18))

table(unicorn$uid)
# Now we have 368 data points per unicorn instead of >4000

# 2. Prep data with 'amt' ----
# We will use the 'amt' package to fit each of these home ranges.
# As we saw in the first lab, the basic building blocks in amt
# are tracks. So we will again create an object of class
# track_xyt' that we can pass to the home range functions.

uni_track <- make_track(tbl = unicorn, .x = x, .y = y, .t = dt, uid = uid, #uid=uid is where you can rename for ex. id
                        crs = sp::CRS("+init=epsg:32612")) #code for UTM zone 12 WGS84

# 'amt' has functions to fit a variety of home range estimators to
# a 'track_xy' or 'track_xyt' object. These functions were designed
# with a consistent syntax regardless of estimator. For an in-depth
# look at the philosophy behind the structure of home range objects
# in 'amt', see this publication (currently a pre-print) by 'amt'
# developer Johannes Signer and John Fieberg:

#   Signer, J., and J. Fieberg. 2020. A fresh look at an old concept: 
#   Home-range estimation in a tidy world. bioRxiv:2020.08.19.256859.
#   https://www.biorxiv.org/content/10.1101/2020.08.19.256859v2

# One of the consistent features of these functions is that they are
# designed to take one set of locations and return one home range 
# object. I.e., if you have multiple individuals, you need to handle
# splitting the data and repeating the operation. The reason is that
# this allows great flexibility in how home ranges should be 
# aggregated. E.g., maybe you have multiple individuals, and maybe 
# each one is tracked for multiple years. Do you want one home range 
# per individual, or one home range per individual per year?

# Signer and Fieberg advocate for a tidy approach to this problem, 
# and in the following sections, we will see their approach for 
# fitting home ranges following a tidy workflow.

# We'll start by fitting just a single home range for a single 
# individual. That way we can focus on the details of each estimator,
# rather than getting bogged down by understanding the code to 
# group and iterate. Toward the end of the lab, we will see the
# approach to fit the home ranges for all individuals.

# Let's subset to just Unicorn # 11
uni_sub11 <- uni_track %>% 
  filter(uid == "U11")

# 3. Fit home ranges for 1 individual ----

# ... a. Fit MCP home ranges ----
# We'll start with the simplest geometric home range estimator, the
# minimum convex polygon (MCP).

# We will use the function 'hr_mcp()' to fit the home range. You can
# see all the documentation for the hr_*() functions here:
?hr

# You could also use the name of a specific function; it will take 
# you to the same place:
?hr_mcp

# The function 'hr_mcp()' has 3 arguments:
#   x         --  the track_xy* that contains the data
#   levels    --  the isopleth level(s) you want (100%, 95%, etc.)
#   keep.data --  whether the hr object should have the original data (default is true)

# Let's calculate the 95% and 50% MCP home ranges for Unicorn 11
mcp11 <- hr_mcp(x = uni_sub11, 
                levels = c(0.95, 0.5), 
                keep.data = TRUE)

# What class is the object we got back?
class(mcp11)
# You can see it is most specifically of class "mcp", but it is also 
# of class "hr_geom" and "hr". 

# All these classes give the object default methods, like a default 
# 'plot()' method. Let's plot it:
plot(mcp11)
# By default, it plots the isopleths we asked for (50% and 95%) and
# because we used the argument 'keep.data = TRUE', it can also plot
# the original data, too.

# Let's take a closer look at the structure of this object:
str(mcp11)
# It is fundamentally a list, with 5 elements
#   $mcp is an 'sf' data.frame. That means it is a data.frame
#     with a geometry column from the 'sf' package that makes
#     it a geospatial object
#   $levels stores the isopleths we asked for
#   $estimator stores the name of the estimator, "mcp"
#   $crs stores the coordinate reference system
#   $data contains the data used to fit the HR

# At some point, you may want to take a closer look at this object, 
# or even manipulate it for yourself. For now, just know that this 
# is what it looks like under the hood.

# 'amt' also provides us with other functions besides 'plot()' that
# we can use to interact with an 'hr' object.

# To get the area of your home range:
hr_area(mcp11, units = TRUE)

# To get the polygons themselves:
hr_isopleths(mcp11)

# Since the polygons are 'sf' spatial objects, they offer a lot of
# flexibility. They can be used in a complex custom plot using either
# base R graphics or ggplot2. They could also be easily exported as, 
# for example, a shapefile or a KML. 

# ... b. Fit KDE home ranges ----
# Fitting a kernel density estimator (KDE) requires more 
# considerations than fitting an MCP. The primary difference is that
# while MCP is a purely geometric operation, the KDE is based on
# estimating a utilization distribution (UD), a probabilistic 
# surface. Thus, fitting a KDE is a two-step process:
#   1. Estimate the kernel UD from the relocations
#   2. Vectorize the UD by finding the polygon that encloses the 
#       desired volume of the UD to draw the isopleths

# One of the important decisions in fitting a KDE is choosing a
# smoothing parameter to estimate the UD. This concept confuses
# a lot of people, so let's spend a minute on it.

# Let's first think about kernel smoothing in just one dimension.
# Plot a histogram of just the x-coordinates of our unicorn:
hist(uni_sub11$x_)

# We can see that is made of a series of bars, and it looks 
# somewhat choppy. We can change how it looks by using different
# numbers of bars:
hist(uni_sub11$x_, breaks = 5)
hist(uni_sub11$x_, breaks = 20)

# Those two histograms look really different, and the maximum
# values are even in different places. This is analogous to the
# smoothing parameter in kernel density estimation. The difference
# is that the 'kernel' in KDE is a function that smooths those 
# transitions.

# We can actually do KDE smoothing with the base R function
# 'density()'.
plot(density(uni_sub11$x_))
# The function prints the bandwidth (smoothing parameter) at the 
# bottom of the plot. We could also control this ourselves:
plot(density(uni_sub11$x_, bw = 30))
plot(density(uni_sub11$x_, bw = 2))

# Again, we get really different results depending on the smoothing.

# Now let's actually fit a KDE home range. The 'amt' function
# 'hr_kde()' has 5 arguments:
#     * Three of them are the same as hr_mcp():
#   x         --  the track_xy* that contains the data
#   levels    --  the isopleth level(s) you want
#   keep.data --  whether the hr object should have the original data
#     * And 2 more that hr_mcp() doesn't have:
#   h         --  the smoothing parameter (or a function to calculate it)
#                 in both x and y direction (vector of length 2)
#   trast     --  the template raster on which to estimate the UD

kde11 <- hr_kde(x = uni_sub11,
                levels = c(0.95, 0.5),
                keep.data = TRUE,
                h = hr_kde_ref(uni_sub11), #default
                trast = make_trast(uni_sub11)) #default

# We can plot the KDE HR just like the MCP:
plot(kde11)

# We should examine it's structure, too:
str(kde11)

# Notice that this has some additional elements, including one called
# "ud", which is a RasterLayer. We can plot a raster:
plot(kde11$ud)

# And there is our smooth UD! We can play with the bandwidth just like
# we did for the histograms and density plots above
kde_h02 <- hr_kde(x = uni_sub11,
       levels = c(0.5, 0.95),
       keep.data = TRUE,
       h = c(2, 2),
       trast = make_trast(uni_sub11))
kde_h30 <- hr_kde(x = uni_sub11,
                  levels = c(0.5, 0.95),
                  keep.data = TRUE,
                  h = c(30, 30),
                  trast = make_trast(uni_sub11))

plot(kde_h02$ud, main = "h = 2")
plot(kde_h30$ud, main = "h = 30")

# Again, we get really different results depending on the bandwidth.

hr_area(kde_h02, units = TRUE)
hr_area(kde_h30, units = TRUE)

# *Again*, REALLY different results depending on the bandwidth.

# So how do you choose a bandwidth?
# This is a tough question, and people are still working on this
# problem today. Here are some options:
#   1. The reference bandwidth (Worton 1989)
#     Assumes the home range is a bivariate normal distribution.
#     If distribution is multimodal, it will tend to over-smooth.
#   2. Least squares cross validation (Seaman and Powell 1996)
#     Often results in under-smoothing and disjunct polygons
#   3. Ad hoc methods (many, see Kie 2013).
#     There are many possible trial-and-error methods for picking
#     an h that matches your preconception of what the home range
#     should look like. These can be hard to defend, depending on
#     the goals of your analysis.
#   4. Use a movement model to estimate the bandwidth, e.g.,
#     using the AKDE approach. We will see this method in the
#     next lab.
#   5. Others! There is a large literature on this, both specific
#     to home ranges and as a general statistical approach.

# ... c. Fit LoCoH home ranges ----
# The last home range type we'll look at today is the Local Convex 
# Hull (LoCoH) home range. As we saw in the lecture this week, there 
# are multiple algorithms that we can use to fit these hulls:
#   k --  include k points in each hull
#   r --  include all points within distance r in each hull
#   a --  include the maximum number of points with cumulative
#         distance <= a

# For more on these three algorithms, see the following publication:
#   Getz, W. M., S. Fortmann-Roe, P. C. Cross, A. J. Lyons, S. J. 
#   Ryan, and C. C. Wilmers. 2007. LoCoH: Nonparameteric Kernel 
#   methods for constructing home ranges and utilization 
#   distributions. PLoS ONE 2(2):e207.
#   https://doi.org/10.1371/journal.pone.0000207

# a-LoCoH is recommended by Getz et al. because the size of the
# kernels (the local hulls) is adaptive (like k, unlike r) and
# it is insensitive to suboptimal choice of parameter a.

# Getz et al. also recommend starting with a value of a equal to
# the maximum distance between any two points in the dataset.

# Let's fit an a-LoCoH home range, using that suggested a. The
# 'amt' function hr_locoh() has 6 arguments:
#     * Three of them are the same as hr_mcp():
#   x           --  the track_xy* that contains the data
#   levels      --  the isopleth level(s) you want
#   keep.data   --  whether the hr object should have the original data
#     * And 3 more that hr_mcp() doesn't have:
#   n           --  the value of the local parameter
#   type        --  which LoCoH rule to use, "k", "r", or "a"
#   rand_buffer --  a small random amount of noise to make sure that
#                   no points are exactly the same (would make a
#                   polygon of area 0)

# First, calculate the distance between all points in our dataset.
# Note, this is very time consuming for large datasets since the
# number of calculations is proportional to the square of the number
# of locations.
dmat11 <- dist(uni_track[, c("x_", "y_")])
a11 <- max(dmat11)

# Now fit the HR
locoh11 <- hr_locoh(x = uni_sub11,
                    levels = seq(0.1, 1, by = 0.1), # asking for a bunch of isopleth values - gives idea of concentrated SU
                    keep.data = TRUE,
                    n = a11,
                    type = "a",
                    rand_buffer = 1e-05)

# Examine the resulting object
str(locoh11)
# We can see it is a list, just like the MCP and KDE objects

# It has an hr_area() method just like the others
hr_area(locoh11)

# And an hr_isopleths() method
hr_isopleths(locoh11)

# It also has default plotting methods just like the others
plot(locoh11)

# We see that this plot has a lot of lines, and it's hard to tell
# exactly what's going on. So let's make our own plot of these 
# polygons with some color added.

# Note that we can pass the fill color to the default plotting method
plot(locoh11, col = "skyblue")
# but this plots the largest isopleth last, so covers everything else.
# This does demonstrate that there is a hole in our home range (which
# wasn't clear from the uncolored plot), but we want to see all the
# isopleths.

# First, we'll store the isopleths object with a name
locoh11_iso <- hr_isopleths(locoh11)

# Now we will plot the geometry column, but reversed, and with
# a gray color ramp
plot(rev(locoh11_iso$geometry), col = gray((1:10)/10))
# Now the largest isopleth is in black, and as the isopleths
# get smaller (i.e., the intensity of use increases), the
# color gets brighter (i.e., closer to white).

# This plot allows us to see that there are separate areas of
# intense use within the larger home range. The most intense
# use is in the eastern edge, but there's another center of
# use near the center of the home range.

# Let's go back to the default plot for one more point:
plot(locoh11)

# Notice that, like MCP but unlike KDE, the points themselves 
# determine the edge of the home range. That means that LoCoH
# can delimit hard boundaries on the edge of a home range, like 
# open water or a cliff edge.

# 4. Multiple Individuals ----
# The functions we've used from `amt` are meant to be applied
# to a single individual. So how can we efficiently repeat this
# for multiple individuals?

# There are many possible ways! The one you choose is up to you,
# based on your needs (for computational speed) and programming
# abilities. Here are some options:
#   1. Copy-and-paste: Most of us would probably be comfortable
#       with this one. We could copy all the code above that we
#       used for unicorn 11, paste it 21 more times, and change
#       the numbers to repeat for unicorns 1 - 22.
#   2. Use a for() loop: While for() loops are intimidating for
#       new users, they are an efficient way to iterate your code.
#   3. Use lapply(): This approach requires you to write your own
#       function, either within the lapply() or as a standalone.
#       Even more intimidating than for() loops for beginners, but
#       this is a great option for making your code readable and
#       efficient.
#   4. Use nested data.frames: The tidyverse allows you to create
#       what are called "nested data.frames". The basic idea is to
#       take a large data frame with lots of repetition and collapse
#       the data while only keeping one copy of the repetitive fields.
#       For example, we could have a data.frame with just 22 rows, one
#       for each unicorn. That data.frame could also have a column called
#       "track" (or anything else) where we store all the tracking data
#       in a collapsed form. Once we nest our data, we can either use
#       lapply() or map() [from tidyverse] to run a custom function
#       over each track.

# Computers are really great at iterating boring tasks. By choosing
# one of these options (#s 2 - 4), we make our code easier to read,
# quicker to write (with practice), and easier to maintain.

# But iteration is an intermediate programming skill that takes some
# time to learn. Today, we're going to start with the simplest option,
# writing a for() loop. In future labs, we'll work our way up to writing
# custom functions and more efficient iteration tools.

# So how can we write a for() loop to iterate home range fitting
# for all of our individuals?

# A good place to start is with the code we already have for unicorn
# 11. Let's get it all in once place without so many comments:

# Subset to individual
uni_sub11 <- uni_track %>% 
  filter(uid == "U11")

# Fit MCP
mcp11 <- hr_mcp(x = uni_sub11, 
                levels = c(0.95, 0.5), 
                keep.data = TRUE)

# Fit KDE
kde11 <- hr_kde(x = uni_sub11,
                levels = c(0.95, 0.5),
                keep.data = TRUE,
                h = hr_kde_ref(uni_sub11), #default
                trast = make_trast(uni_sub11))

# Calculate LoCoH a*
dmat11 <- dist(uni_track[, c("x_", "y_")])
a11 <- max(dmat11)

# Fit LoCoH
locoh11 <- hr_locoh(x = uni_sub11,
                    levels = seq(0.1, 1, by = 0.1),
                    keep.data = TRUE,
                    n = a11,
                    type = "a",
                    rand_buffer = 1e-05)

# Okay, now that we have all the code for one individual,
# let's see if we can generalize it for all individuals.

# One good option is to use lists to store each object we save. Let's
# declare them, or in other words, let R know that we will need these
# lists later.

uni_sub <- list()
mcp <- list()
kde <- list()
locoh <- list()

# In the for() loop, we'll use an index variable, i, to keep track of
# which unicorn we are working on. In our case, our unicorns are 
# numbered 1 - 22. But even if your animals have complex IDs, you 
# might still consider using an ordered index.

# Unique unicorn IDs
uni_ids <- sort(unique(unicorn$uid))

# Now the loop. I'm using the same code as for unicorn 11, but I'm
# going to tweak it so that we subset the individual based on our
# index variable.

for (i in 1:length(uni_ids)) {
  # Print status
  print(i)
  
  # Subset to individual
  uni_sub[[i]] <- uni_track %>% 
    filter(uid == uni_ids[i])
  
  # Fit MCP
  mcp[[i]] <- hr_mcp(x = uni_sub[[i]], 
                  levels = c(0.95, 0.5), 
                  keep.data = TRUE)
  
  # Fit KDE
  kde[[i]] <- hr_kde(x = uni_sub[[i]],
                  levels = c(0.95, 0.5),
                  keep.data = TRUE,
                  h = hr_kde_ref(uni_sub[[i]]), #default
                  trast = make_trast(uni_sub[[i]]))
  
  # Calculate LoCoH a*
  dmat <- dist(uni_sub[[i]][, c("x_", "y_")])
  a <- max(dmat)
  
  # Fit LoCoH
  locoh[[i]] <- hr_locoh(x = uni_sub[[i]],
                      levels = seq(0.1, 1, by = 0.1),
                      keep.data = TRUE,
                      n = a,
                      type = "a",
                      rand_buffer = 1e-05)
  
}

# What do our results look like now?
str(mcp)

# It is a list of lists (sometimes of lists)!

# How can I plot the first unicorn's MCP?
plot(mcp[[1]], main = "1st Unicorn")

# How can I plot the 15th unicorn's KDE?
plot(kde[[15]], main = "15th Unicorn")

# How can I get the area of the 19th unicorn's LoCoH?
hr_area(locoh[[19]])
hr_area(mcp[[5]])
hr_area(kde[[19]])

# Finally, we might want our list elements to have names, like if
# they were the columns of a data.frame. Then we can call them with
# the $ operator.
names(mcp) <- names(kde) <- names(locoh) <- uni_ids

# Now plot the MCP for U6
plot(mcp$U6)

# How big is the KDE for U9?
hr_area(kde$U9)

# Get the LoCoH polygons for U17
hr_isopleths(locoh$U17)

# ... compiling home range areas ----
# Q: "Can I extract areas for all the unicorns?"
# A: "Yes!"

# You can easily iterate a function over a list of objects by
# using the base R function "lapply()". We will visit this in
# detail in a later lab. For today, let's just see how we can
# extract home range areas.

# lapply() needs at least 2 arguments:
#   X = the list across which to iterate
#   FUN = the function to repeat

# You can pass it additional arguments, which will be passed along
# to FUN. We don't need any for today, though.

# To extract the areas from all MCPs
mcp_areas <- lapply(mcp, hr_area)
kde_areas <- lapply(kde, hr_area)
locoh_areas <- lapply(locoh, hr_area)

# The result of lapply() is also a list. If we want to combine these
# in a data.frame, the easiest way is to use dplyr::bind_rows()
mcp_df <- bind_rows(mcp_areas, .id = "UID")
locoh_df <- bind_rows(locoh_areas, .id = "UID")
kde_df <- bind_rows(kde_areas, .id = "UID")
# Try to compile a data.frame of KDE and LoCoH sizes for the unicorns
# as well!
