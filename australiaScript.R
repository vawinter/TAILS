# WFS 597 - Programming for Ecologists
# R Session - Group Project
# Visualizing Animal Movement
# Veronica W., Morgan S., Karen B.

library(move)

fox <- getMovebankData("Red Fox (Vulpes vulpes) - Scotia, NSW, Australia", includeExtraSensors = TRUE)
cat <- getMovebankData("Feral cat (Felis catus) - Scotia, NSW", includeExtraSensors = TRUE)

foxRef <- getMovebankReferenceTable("Red Fox (Vulpes vulpes) - Scotia, NSW, Australia")
catRef <- getMovebankReferenceTable("Feral cat (Felis catus) - Scotia, NSW")


# # visualization package options:
# https://movevis.org
# https://terpconnect.umd.edu/~egurarie/teaching/MovementAtICCB2017/
# https://dynamovis.geog.ucsb.edu/index
# https://rstudio.github.io/leaflet/
# https://towardsdatascience.com/animating-your-data-visualizations-like-a-boss-using-r-f94ae20843e3
# https://hansenjohnson.org/post/animate-movement-in-r/
# https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/
# https://plotly.com/r/

# Webpage to walkthrough animal vis:
# https://hansenjohnson.org/post/animate-movement-in-r/