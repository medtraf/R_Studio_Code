

library(plyr)     # to rename columns
library(ggplot2)  # for making plots
library(reshape2) # reshape df for plotting 

# Set working directory
##setwd("E:/Documents/Predictive Analytics/MSDS 455 Data Visualization/Final Project")

this_dir <- function(directory)
  setwd( file.path(getwd(), directory) )

# Import data
Flu_vac <- read.csv(file = "flu vac coverage sheet.csv", header = TRUE)

# Fix column names
Flu_vac <- rename(Flu_vac, c('X6.months...17.years' = '6mo-17yrs', 'X18.64.years' = '18-64yrs', 
                             'Over.65.years'= '>=65yrs'))

# Prep data
flu_df <- melt(Flu_vac, id.vars="Season")                   # reshape data frame

# Create plot
plotting_object <- ggplot(flu_df, aes(Season,value, col=variable,group = variable)) + 
  geom_line(size=2) +
  ylim(0,100) +                                                          # set y axis 0-100
  ylab("Percent Coverage") +                                             # Lable Y axis
  xlab("Flu Season") +                                                   # Lable X axis
  scale_colour_hue(name = "Age Group",                                   # Legend title 
                    breaks=c(">=65yrs","6mo-17yrs","18-64yrs"),          # Change order of ledgend
                   labels=c("65yrs", "6mo-17yrs","18-64yrs"))+    # Change text of legend (>= symbol)  \u2265 
  ggtitle("Vaccine Coverage by Age per Season") +                        # Plot title 
  theme(plot.title = element_text(hjust = 0.5)) +                        # Center plot title
  theme(axis.line = element_line(colour = "gray"),                       # Remove background colors
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 16, family = 'Calibri'))             # Set font

print (plotting_object)
ggsave("Figure5.svg", width=12, height=10)


this_dir <- function(directory)
  setwd( file.path(getwd(), directory) )



