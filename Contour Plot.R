library(ggplot2)
library(dplyr)
library(ggthemes)
library(gridExtra)

Dt <- read.csv('Dataset.csv')
head(Dt)

# Version 1
## bins = 15
p1 <- 
  ggplot(Dt, aes(x, y, z = z)) +
  geom_contour(bins = 15) +                 # Create contour plot with 15 bins
  ggtitle('bins = 15') + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +                                                      # dark-on-light theme
  theme(panel.background = element_blank(),                    
        axis.line.x = element_line(),                               # these two are for the axis line
        axis.line.y = element_line(),
        axis.text.x = element_text(colour = "black"),               # there two are for texts in axes
        axis.text.y = element_text(colour = "black"),
        axis.ticks.x = element_line(),                              # these two are for ticks in axes
        axis.ticks.y = element_line(),
        axis.title.x = element_text(colour = "black", face = 'bold', vjust = -1),                              
        axis.title.y = element_text(colour = "black", face = 'bold'),
        plot.title = element_text(face = 'bold'),
        legend.title = element_text(colour = "black", face = 'bold'),
        legend.text = element_text(colour = "black"))

## bins = 30
p2 <- 
  ggplot(Dt, aes(x, y, z = z)) +
  geom_contour(bins = 30) +                # Create contour plot with 30 bins
  ggtitle('bins = 30') + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +                                                      # dark-on-light theme
  theme(panel.background = element_blank(),                    
        axis.line.x = element_line(),                               # these two are for the axis line
        axis.line.y = element_line(),
        axis.text.x = element_text(colour = "black"),               # there two are for texts in axes
        axis.text.y = element_blank(),
        axis.ticks.x = element_line(),                              # these two are for ticks in axes
        axis.ticks.y = element_line(),
        axis.title.x = element_text(colour = "black", face = 'bold', vjust = -1),                              
        axis.title.y = element_blank(),
        plot.title = element_text(face = 'bold'),
        legend.title = element_text(colour = "black", face = 'bold'),
        legend.text = element_text(colour = "black"))

# Arrange 2 figures
Layout.Mat <- matrix(c(rep(1, 9), rep(2, 8)), nrow = 1)
grid.arrange(p1, p2, layout_matrix = Layout.Mat)

# Version 2
p3 <- 
  ggplot(Dt, aes(x, y, z = z)) +
  geom_contour(aes(colour = after_stat(level)), bins = 30) +       # Create contour plot with 30 bins and set color of level
  scale_color_continuous(type = 'viridis') +
  ggtitle('bins = 30') + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +                                                      # dark-on-light theme
  theme(panel.background = element_blank(),                    
        axis.line.x = element_line(),                               # these two are for the axis line
        axis.line.y = element_line(),
        axis.text.x = element_text(colour = "black"),               # there two are for texts in axes
        axis.text.y = element_text(colour = "black"),
        axis.ticks.x = element_line(),                              # these two are for ticks in axes
        axis.ticks.y = element_line(),
        axis.title.x = element_text(colour = "black", face = 'bold', vjust = -1),                              
        axis.title.y = element_text(colour = "black", face = 'bold'),
        plot.title = element_text(face = 'bold'),
        legend.title = element_text(colour = "black", face = 'bold'),
        legend.text = element_text(colour = "black"))

# Modify legend title
p3$labels$colour <- 'Level of z'
p3

# Create selected palette
require(RColorBrewer)
Breaks <- c(-Inf, seq(150, 400, length.out = 9), Inf)               # set up the number of breaks
Spectral.colors <- brewer.pal(n = length(Breaks), name = "Spectral") # generate color palette

# Version 3
p4 <- 
  ggplot(Dt, aes(x, y, z = z)) +
  geom_contour_filled(breaks = Breaks) +            # pass Breaks (approximately bins = 11)
  scale_fill_manual(values = rev(Spectral.colors),  # set up color palette manually and using decrasing order for values
                    drop = F) +                     # avoid dropping unused factors 
  guides(fill = guide_colorsteps(direction = "vertical")) +         # vertically display legend
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +                                                      # dark-on-light theme
  theme(panel.background = element_blank(),                    
        axis.line.x = element_line(),                               # these two are for the axis line
        axis.line.y = element_line(),
        axis.text.x = element_text(colour = "black"),               # there two are for texts in axes
        axis.text.y = element_text(colour = "black"),
        axis.ticks.x = element_line(),                              # these two are for ticks in axes
        axis.ticks.y = element_line(),
        axis.title.x = element_text(colour = "black", face = 'bold', vjust = -1),                              
        axis.title.y = element_text(colour = "black", face = 'bold'),
        plot.title = element_text(face = 'bold'),
        legend.title = element_text(colour = "black", face = 'bold'),
        legend.text = element_text(colour = "black"))

# Modify legend title
p4$labels$fill <- 'Level of z'
p4

# Version 4
p5 <- 
  ggplot(Dt, aes(x, y, z = z)) +
  geom_contour(bins = 11) +            # create contour plot with 30 bins and set color of level
  geom_raster(aes(fill = z)) +         # fill grid 
  scale_fill_continuous(type = 'viridis') +
  ggtitle('bins = 11') + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +                                                      # dark-on-light theme
  theme(panel.background = element_blank(),                    
        axis.line.x = element_line(),                               # these two are for the axis line
        axis.line.y = element_line(),
        axis.text.x = element_text(colour = "black"),               # there two are for texts in axes
        axis.text.y = element_text(colour = "black"),
        axis.ticks.x = element_line(),                              # these two are for ticks in axes
        axis.ticks.y = element_line(),
        axis.title.x = element_text(colour = "black", face = 'bold', vjust = -1),                              
        axis.title.y = element_text(colour = "black", face = 'bold'),
        plot.title = element_text(face = 'bold'),
        legend.title = element_text(colour = "black", face = 'bold'),
        legend.text = element_text(colour = "black"))
# Modify legend title
p5$labels$fill <- 'Level of z'
p5


