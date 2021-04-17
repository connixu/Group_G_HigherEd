library(ggplot2)
ourtheme <- theme(panel.grid = element_line(color = "white"),
      text = element_text(family = "sansserif"),
      panel.background = element_blank(),
      legend.position = "bottom",
      axis.ticks = element_blank(),
      plot.title = element_text(hjust=0.5))