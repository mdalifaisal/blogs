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

# Data prep --------------------------------------------------------------------

flights_raw <- tt_load('2022-07-12')$flights

# Data only goes up to end of May 2022 so to do a fair year on year comparison
# will need to select the same time period (latest v previous)

flights_all <- flights_raw |>
        clean_names() |>
        mutate(time_frame = case_when(flt_date > as.Date("2021-04-30") ~ "latest",
                                      flt_date > as.Date("2020-04-30") ~ "previous",
                                      TRUE ~ "other")) |>
        filter(time_frame %in% c("latest", "previous")) |>
        group_by(time_frame, state_name) |>
        summarise(arrivals = sum(flt_arr_1, na.rm = TRUE), .groups = "drop") |>
        arrange(time_frame, desc(arrivals))

# Take the top 10 countries in the latest period only

state_names <- flights_all |>
        slice(1:10) |>
        pull(state_name)

flights_top_10 <- flights_all |>
        filter(state_name %in% state_names) |>
        
        # Get country code for using with geom_flag later - Turkey doesnt match
        # because it has changed its name so manually input it's code
        
        mutate(state_name = factor(state_name, levels = rev(state_names)),
               code = countrycode(state_name, "country.name", "iso2c"),
               code = if_else(is.na(code), "TR", code)) |>
        # Assign a value to each pair of entries corresponding to countries. This is
        # how we will draw the line for the dumbbell
        
        arrange(state_name) |>
        mutate(pair = rep(1:(n() / 2), each = 2))

# Calculate percentage change between periods for each country and create
# text for the plot. Set the previous time period to blank "" because only need
# one

flights <- flights_top_10 %>%
  pivot_wider(names_from = time_frame, values_from = arrivals) %>%
  mutate(percent_change=100*(latest / previous - 1)) %>%
  select(state_name, percent_change) %>%
  left_join(flights_top_10, y = _) %>%
  mutate(percent_text = paste0("+", round(percent_change), "%"),
         percent_text = if_else(time_frame == "previous", "", percent_text))

flights <- cbind(flights_top_10, percent_text=(1:10))

# Plot -------------------------------------------------------------------------
flights <- read.csv('dumbell_data.csv')
#flights <- flights[5:22,]
# You might need to install Roboto

font <- "Roboto"

# Colours for the dumbbells

chart_colours <- c("latest" = "#5B9374", "previous" = "#C34A4A")

# With the ggtext package we can use html to colour the text in the title

subtitle_html <- glue(
        "The bank z-score of major European economies given for postcovid
   <br>
   <span style='color:{chart_colours[1]};'>(2021)</span>
   and precovid
   <span style='color:{chart_colours[2]};'>(2020)</span>.
   <br>
   It captures the probability of the default of a countries commercial banking system."
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
                title = "Bank Z-score",
                subtitle = subtitle_html,
                caption = "Source: https://github.com/mdalifaisal/blogs/tree/main/z-score, data from WorldBank Database") +
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
        
        # Scales package for writing the scale in thousands
        
        scale_x_continuous(labels = label_comma(scale = 1,  suffix = ""),
                        breaks = c(5, 10, 15),
                          limits = c(0, 20))
p
ggsave(glue("z-score.png"),
       plot = p,
       width = 7.65,
       height = 6.375,
       dpi = 550)
