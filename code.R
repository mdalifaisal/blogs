# Setup ------------------------------------------------------------------------

library(tidytuesdayR)
library(janitor)
library(dplyr)
library(ggplot2)
library(scales)
library(countrycode)
library(ggimage)
library(tidyr)
library(glue)
library(ggtext)
library(here)
library(htmltools)

# Plot -------------------------------------------------------------------------
flights <- read.csv('dumbell_data.csv')
# You might need to install Roboto

font <- "Roboto"

# Colours for the dumbbells

chart_colours <- c("latest" = "#5B9374", "previous" = "#C34A4A")

# With the ggtext package we can use html to colour the text in the title

subtitle_html <- glue(
        "The z-score of major European economies given for postcovid
   <br>
   <span style='color:{chart_colours[1]};'>(2021)</span>
   and precovid
   <span style='color:{chart_colours[2]};'>(2020)</span>."
)

p <- flights |>
        ggplot(aes(x = caps, y = countryname)) +
        
        # Add the line
        
        geom_line(aes(group = pair), colour = "white") +
        
        # Add the points on the dumbbells
        
        geom_point(aes(colour = timeframe), size = 2) +
        scale_colour_manual(values = chart_colours) +
        
        # Flags using the isocodes we found earliers
        
        geom_flag(x = 0, aes(image = code)) +
        
        # Percent change text
        
        geom_text(aes(label=percent),
                  x = 2,
                  size = 3.4,
                  colour = "white") +
        labs(
                x = "Score",
                y = "",
                title = "Commercial banking system's Z-score",
                subtitle = subtitle_html,
                caption = "") +
        theme_bw() +
        theme(text = element_text(family = font),
              panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "none",
              panel.grid.major.x = element_line(colour = "#253139"),
              axis.text.x = element_text(size = 12, colour = "white"),
              axis.text.y = element_text(size = 11, colour = "white"),
              axis.title.x = element_text(size = 12,
                                          margin = margin(15, 0, 0, 0),
                                          colour = "white"),
              axis.ticks = element_blank(),
              plot.title = element_markdown(size = 20,
                                            margin = margin(15, 0, 10, 9),
                                            face = "bold",
                                            colour = "white"),
              plot.title.position = "plot",
              
              # Allows us to use html to colour the subtitle text
              
              plot.subtitle = element_markdown(size = 12,
                                               margin = margin(0, 0, 20, 9),
                                               colour = "white",
                                               lineheight = 1.1),
              plot.caption = element_text(margin = margin(15, 0, 0, 5),
                                          size = 9,
                                          colour= "white"),
              plot.background = element_rect(fill = "#2E3D47"),
              panel.background = element_rect(fill = "#2E3D47"))+
        
        
        scale_x_continuous(labels = label_comma(scale = 1,  suffix = ""),
                        breaks = c(5, 10, 15),
                          limits = c(0, 20))

ggsave(glue("z-score.png"),
       plot = p,
       width = 7.65,
       height = 6.375,
       dpi = 550)
