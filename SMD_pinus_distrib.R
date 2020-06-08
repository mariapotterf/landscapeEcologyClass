####################################################

## Project: Landscape Ecology MOOC

## Script purpose: Find and map suitable sites for wind energy in Switzerland

## Date: 2020/06/07

## Author: Maria Potterf

##################################################

#getwd()
setwd("C:/MyTemp/myGitLab/landscapeEcology/inputData/tree_distribution_MOOC_07_19/tree_distribution_MOOC_07_19/data")

# Read libraries
library(raster)
library (sp)
library (RColorBrewer)
library (pander)
library (rgdal)


# Load species distribution data:
Occurrences <- read.csv("./occurrences.csv", h = T)

# Load raster data:
DDEG <- raster("./ddeg_1km")  # Number of degrees days
MIND <- raster("./mind_1km")  # MOisture index 
SRAD <- raster("./srad_1km")  # solar radiation

# 6. Visualize the climatic maps------------------------------------------------------------

par(mar = c(3, 0.1, 1, 0.1), mfrow = c(2, 2))

# Plot raster and tehn add legend
plot(DDEG, 
     col = rev(heat.colors(20)), 
     box = F, 
     axes = F, 
     legend = F)

plot(DDEG, 
     legend.only = T, 
     horizontal = T, 
     add = T, 
     col = rev(heat.colors(20)), 
     smallplot = c(0.25, 0.75, 0.08, 0.1))

# Soil moisrture
plot(MIND, 
     col = brewer.pal(9, "PuBu"), 
     box = F, axes = F, legend = F)

plot(MIND, 
     legend.only = T, 
     horizontal = T, add = T, 
     col = brewer.pal(9, "PuBu"), 
     smallplot = c(0.25, 0.75, 0.08, 0.1))


# Solar radiation
plot(SRAD, 
     col = brewer.pal(9, "YlOrRd"), 
     box = F, axes = F, legend = F)

plot(SRAD, 
     legend.only = T, 
     horizontal = T, 
     add = T, 
     col = brewer.pal(9, "YlOrRd"), 
     smallplot = c(0.25, 0.75, 0.14, 0.16))



# 7. save the dataframe "climate"-----------------------------------------------------------

Climate <- cbind(DDEG = extract(DDEG, Occurrences[, c("x", "y")]),
                 MIND = extract(MIND, Occurrences[, c("x", "y")]),
                 SRAD = extract(SRAD, Occurrences [, c("x", "y")]))



# 8. Append climatic data to occurences, discard incomplete observations--------------------
Occurrences <- cbind(Occurrences, 
                     Climate)

# remove na values
Occurrences <- na.omit(Occurrences)


# 9. Plot climate raster and abscence/presence data (random sample, otherwise we can't see the map well)

set.seed(1)

table(Occurrences$presence)/nrow(Occurrences) * 100


# Subset the samples: just for visualisation purposes
# ----------------------
# Index occurence:
indx.oc <- sample(which(Occurrences$presence == 1), 
                  round(1000 * sum(Occurrences$presence == 1)/nrow(Occurrences)))

# Index absence
indx.ab <- sample(which(Occurrences$presence == 0), 
                  round(1000 * sum(Occurrences$presence == 0)/nrow(Occurrences)))


# Plot sample data
par(mfrow = c(2, 1), 
    mar = c(0.1, 0.1, 0.1, 0.1))

# PLot occurence
plot(DDEG, 
     col = rev(heat.colors(20)), 
     box = F, axes = F)

points(Occurrences$x[indx.oc], 
       Occurrences$y[indx.oc], 
       col = "green4", pch = 3, cex = 0.5)

# Plot absences
plot(DDEG, 
     col = rev(heat.colors(20)), 
     box = F, axes = F)

points(Occurrences$x[indx.ab], 
       Occurrences$y[indx.ab], 
       col = "blue", pch = 3, cex = 0.5)

legend("bottomright", 
       legend = c("present", "absent"), pch = c(3, 3), col = c("green4", "blue"))



# ------------------------------------
# Species distribution modelling:
# ------------------------------------

# First run model on each predictor:

# 11. Fit GLM based on degree days------------------------------------------------

glm.uni <- glm(presence ~ poly(DDEG, 2), 
               data = Occurrences,
               family = binomial, 
               maxit = 100)

# Print table
pander(summary(glm.uni)$coefficients, 
       caption = "Summary of GLM model based on degree-days.")


