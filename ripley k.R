#### Spatial Point Pattern Analysis ####

# Step 1: Install packages
install.packages("maptools")
install.packages("spatstat")
install.packages("sf")

# Step 2: Load packages
library(maptools)
library(spatstat)
library(sf)
library(ggplot2)
library(tidyverse)



# Step 3: Set working directory 
setwd("/Users/briannacarnagie/Programming/P8371/Midterm materials")


# Read in pa shp
pa <- st_read("PA_county.shp")
plot(pa["area"])


# Set as owin - observation window 
pa_window <- as.owin(pa)

# Read in cholera deaths 
frack <- st_read("fracking_wells.shp")

frack %>% 
  sample_n(500) -> frack_500



# Set death as a point pattern process
frack_500_ppp <- as.ppp(frack_500)


# Combine Spatial Points with Observation Window 

wells_pa <- ppp(frack_500_ppp$x,
                frack_500_ppp$y,
                  window = pa_window)
plot(wells_pa)

# Create heatmap of TRI sites
plot(density(wells_pa),
     main= "Kernel density map of freaking wells in PA")

# Change the buffer to 50 meters
plot(density(wells_pa, sigma = 50),
     main= "Kernel density map of freaking wells in PA")


# Quadrat Test
qt <- quadrat.test(wells_pa, nx=6, ny=10)
qt
plot(qt, 
     cex=.7,
     main = "Quadrat Test")


###Run L function with 10 simulations and 95% confidence interval (rank=2) on all points (global=T)###
L <- envelope(wells_pa, 
              Lest, 
              nsim = 20,
              rank=2, 
              global=T)


###From the results we can see that at very small distances between points (x-axis) there is an 
###immediate spike in density (y-axis)

L_plot = plot(L, 
              main = "Global Clustering Results", 
              ylab = "Density Function",
              xlab = "Distance") |> ggsave("ripleysK.png")



library(ggplot2)

# Create a base R plot (replace this with your actual plot code)
L <- ...  # Your data and plot code here

# Convert the base R plot to a ggplot2 object
L_plot_gg <- as_ggplot(L)

# Customize the ggplot2 plot (add titles, labels, etc.)
L_plot_gg <- L_plot_gg +
  labs(
    title = "Global Clustering Results",
    y = "Density Function",
    x = "Distance"
  )

# Save the ggplot2 plot as an image file using ggsave
ggsave("ripleysK.png", plot = L_plot_gg)




