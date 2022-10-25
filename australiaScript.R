# WFS 597 - Programming for Ecologists
# R Session - Group Project
# Visualizing Animal Movement
# Veronica W., Morgan S., Karen B.

cat <- getMovebankData("Red Fox (Vulpes vulpes) - Scotia, NSW, Australia", includeExtraSensors = TRUE)
fox <- getMovebankData("Feral cat (Felis catus) - Scotia, NSW", includeExtraSensors = TRUE)
# https://cran.r-project.org/web/packages/move/index.html

# visualization package options:
https://movevis.org
https://rstudio.github.io/leaflet/
https://towardsdatascience.com/animating-your-data-visualizations-like-a-boss-using-r-f94ae20843e3
https://hansenjohnson.org/post/animate-movement-in-r/
https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/
https://plotly.com/r/
  