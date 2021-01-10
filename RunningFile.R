# Computational instantiation of the Behavior System approach (predatory subsystem)
# This is an R script for mac. It may need sintaxis modifications to run on microsoft
# Cristina Santos - University of Minho - cristina.dst@gmail.com - Decemeber, 2018.


################ DESCRIPTION #################
# This scrip will run the simulation and save a file called "TimeStepData" 
# in two different formats (.txt and .csv) with the outcome of each time step.
# Two additional data files will be generated in two different formats each (.txt and .csv),
# "IRTSurvival" with the p(IRT > time step), and "BoutFrequency" with the relative frequency of bout lengths.
# This scrip also generates two figures: One with the data of the "IRTSurvival" file,
# and another one with the data in the "BoutFrequency" file.
# Both figures are saved as .pdf files with the same name as the data.
#All files will be saved in the directory where the script is running from.

################ INSRUCTIONS #################
# Make sure this scrip is saved un the same place as the FunctionsFile.R sript
# Points 1, 2, and 3, only need to be run the first time.
# Run the simulation by running the 'xBehavSys(d, b, w, q)' function.
# Plot the data by running the Figures section of the code.


# 1. Load packages needed to run the code
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)

# 2. Identify the working directory to source the funtions
thedir <- getwd()

# 3. Source file with the functions
source(paste(thedir, "/FunctionsFile.R", sep = ""))


#### RUN SIMULATION ####
xBehavSys(d = .1, b = .3, w = .5, q = .1)
# d: probability of changing to Global Search or Disengage mode
# b: probability of changing to Focal Search or Visit mode  
# w: probability of lever pressing when in Focal Search or Visit mode
# q: probability of lever pressing when in Global Search or Disengage mode


###### FIGURES ######

# IRT's survival plot 
FigureA <- ggplot(Irts, aes(x = Pauses, y = pSurvival)) +
  geom_line() +
  geom_point(size = 1) +
  scale_y_log10(name = "p(IRT > x)", expand = c(.03, 0), breaks = c(.001, .01, .1, 1),
                labels = scales::comma) +
  scale_x_continuous(name = "IRT (time steps)", expand = c(0, 1), limits = c(0, 25)) +
  labs(title = "IRT Survival plot") +
  theme_classic() +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

# Visualize plot
FigureA

# Save plot
ggsave("IRTSurvival.pdf", FigureA, device = "pdf", width = 5, height = 3.5, units = "in")


# Figure of the relative frequency of bout length 
FigureB <- ggplot(BoutFrequency, aes(x = ABout, y = RelBF)) +
  geom_smooth(method = lm, se = F, colour = "grey70", size = .7) +
  geom_point(size = 1) +
  scale_y_log10(name = "Relative frequency", expand = c(.01, 0), breaks = c(.001, .01, .1, 1),
                labels = scales::comma) +
  scale_x_continuous(name = "Bout length (Responses)") +
  labs(title = "Relative frequency of bout lengths") +
  theme_classic() +
  theme(axis.line = element_line(colour = "black"))

# Visualize plot
FigureB

#Save Plot
ggsave("BoutFreq.pdf", FigureB, device = "pdf", width = 5 , height = 3.5, units = "in")

