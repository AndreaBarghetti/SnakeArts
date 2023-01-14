library(tidyverse)
source("R/snake_player.R")

set.seed(1)
space <- Space$new(80,150)
space$fill_space()
space$show() + snake_palette("A")

get_space_stats(space)

space$show(linewidth = 1, dotsize=2) + snake_palette("D")

p <- space$show(background_color="black",linewidth = 5, dotsize=4)
p + scale_color_viridis_c(direction = 1, begin = 0.1, end = .9, option = "F")
p + scale_color_viridis_c(direction = 1, begin = 0, end = 1, option = "D")
# scale_color_manual(values = rep(c("white","red"),600))

p1 <- p + scale_color_viridis_c(direction = 1, begin = 0.1, end = .9, option = "F")
ggsave(p1, 
       filename="image.svg", 
       device = "svg", 
       units = "cm", 
       width = 150,
       height = 80,
       dpi = 320,
       limitsize=F)

