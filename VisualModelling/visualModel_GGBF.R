## Packages
library(alphashape3d)
library(dplyr)
library(lmodel2)
library(MuMIn)
library(pavo)
library(stringr)
library(tidyverse)


## Clear workspace
rm(list=ls())

allspec <- getspec('data/GGBF/spectra', ext = 'txt', subdir = FALSE,lim = c(300, 700))

# Proc - Applies normalization and/or smoothing to spectra for further analysis or plotting.
allspec <- procspec(allspec, opt = 'smooth', fixneg = 'zero')

#Select just groin
ggbfGroin <- subset(allspec, "groin") 

# Aggregate spectra - Average spectra by name taking the first 6 chars of name. This is the Frog Identifier
group <- substr(names(ggbfGroin), 1, 6)
group[1] <- 'wl'
ggbfGroin <- aggspec(ggbfGroin, by = group)

# Tidy  
rm(group)


colours <- spec2rgb(ggbfGroin)
# Remove the names from the colours DF to ensure that all points/lines are plotted. With out this one line/point is missed
names(colours) <- NULL

par(mfrow=c(1,2))
par(mar=c(8,5,2,1))

plot(ggbfGroin, col = colours,ylab="Reflectance (%)", xlab="Wavelength (nm)", cex.lab=2)
mtext("Frog flash colours", side = 1, line = 6, cex = 2.0)


bgspec <- getspec("data/GGBF/Plants", ext = "jaz", subdir = FALSE)
bgspec <- procspec(bgspec, opt = 'smooth', fixneg = 'zero')

colours <- spec2rgb(bgspec)
# Remove the names from the colours DF to ensure that all points/lines are plotted. With out this one line/point is missed
names(colours) <- NULL
plot(bgspec, col = colours,ylab="Reflectance (%)", xlab="Wavelength (nm)", cex.lab=2)
mtext("Background colours", side = 1, line = 6, cex = 2.0)


m_ggbfGroin <- colspace(vismodel(ggbfGroin, visual = "avg.v", achromatic = "ch.dc"))
combspec <- merge(ggbfGroin,bgspec)

# Run a visual model using an average 'v' sensitive bird
vis.comb <- vismodel(combspec, visual = "avg.v", achromatic = "ch.dc", relative = FALSE)


# Plot in a tetrahedral colourspace for fun/visualisation.
colours <- spec2rgb(combspec)
# Remove the names from the colours DF to ensure that all points/lines are plotted. With out this one line/point is missed
names(colours) <- NULL

par(mfrow=c(1,1))
par(mar=c(8,0,2,0))

plot(colspace(vis.comb), col = colours, labels = TRUE,cex.lab=2,cex=2,cex.axis=2)

# Create a grouping variable 
pop_group <- c(rep("GGBF", 19), rep("Background", 10))

# Estimate the noise-weighted disttance (Just Noticable Distances) between the colours of group 1 and group 2
# Distances (JND's) of < 1-3 'difficult to discriminate'. > 3 pretty confident they should be discriminable.
clDist <- bootcoldist(vis.comb, by = pop_group, n = c(1, 2, 2, 4), weber = 0.1, weber.achro = 0.1)

clDist
