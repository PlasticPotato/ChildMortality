# Installs the necessary packages
install.packages(c('ggplot2', 'gifski', 'plotly', 'scales', 'htmlwidgets'))

# Gets data from the .csv file
childMortality <- read.csv('Cleaned and joined table - export from MySQL Workbench.csv')

# Gets the necessary ibraries:
library(ggplot2)
library(plotly)
library(gifski)
library(scales)
library(htmlwidgets)

# Creates an alternative text for the data labels which are then shown when hovering over the graph, rounds the numbers
childMortality <- childMortality %>%
  mutate(text = paste0("Country: ", COUNTRY,
                       "\nPopulation: ", round(POPULATION/1e6,1), " million",
                        "\nGDP: ", scales::dollar(childMortality$GDP, accuracy = 4),
                        "\nChild mortality: ", round(CHILD_MORTALITY_RATE, 1)))

# Creates the ggplot basis for the later animation in plotly
plot1 <- ggplot(data = childMortality, aes(x = GDP, y = CHILD_MORTALITY_RATE, size = POPULATION, color = CONTINENT,
                                           label = COUNTRY, text = text)) +
  geom_point(aes(size = POPULATION, frame = YEAR, ids = COUNTRY), alpha = 0.7) +
# Places values on a logarithmic scale and changes labels to match the values e.g. 1e+02 = 100, 1e+03 = 1000 etc.
  scale_x_continuous(trans= "log10", breaks = c(1e+02, 1e+03, 1e+04, 1e+05), labels = c("100", "1k", "10k", "100k")) +
# Adjusts the scale of the bubbles
  scale_size(range = c(1,10)) +
# Chops the data into separate graphs by continent
  facet_wrap(~CONTINENT) +
# Adjusts the visuals of the graphs and the color scale
  theme_bw() +
  scale_color_brewer(palette = "Dark2") +
# Adds titles to the graps and the axis
  labs(title = "Child mortality vs. GDP",
       x = "GDP in US$ (current)",
       y = "Child mortality rate (per 1000 live births)") +
# Adjusts the legends, titles and spacing of the graphs
  theme(plot.title = element_text(size = 24, face = "bold", color = "black"),
        axis.title.x = element_text(size = 12, face = "bold", color = "black"),
        axis.title.y = element_text(size = 12, face = "bold", color = "black"),
        axis.text.x = element_text(size = 8, face = "bold", color = "darkgrey"),
        axis.text.y = element_text(size = 8, face = "bold", color = "darkgrey"),
        axis.line = element_line(colour = "black", linewidth = 1, linetype = "solid"),
        legend.position = "none",
        panel.spacing = unit(2, "lines"))

# Generates a plotly animated plot from the ggplot above
plot2 <- ggplotly(plot1, height = 800, width = 1000, tooltip = "text") %>%
  animation_opts(500, easing = "linear", redraw = FALSE) %>%
  animation_button(x = 0, y = 0) %>%
  animation_slider(currentvalue = list(prefix = "Year: ", font = list(color="darkred")))

# Exports the animated plot to html format
htmlwidgets::saveWidget(widget = plot2, file = "plot.html", selfcontained = FALSE)