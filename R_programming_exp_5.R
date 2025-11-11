# Install if required:
# install.packages(c("ggplot2", "reshape2", "gapminder", "gganimate", "gifski"))

library(ggplot2)
library(reshape2)
library(gapminder)
library(gganimate)

# 1. Correlation Heatmap

data(mtcars)
corr_matrix <- round(cor(mtcars), 2)
melted_corr <- melt(corr_matrix)

heatmap_plot <- ggplot(melted_corr, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  labs(title = "Correlation Heatmap of mtcars dataset", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

print(heatmap_plot)


# 2. Faceted Plots

data(iris)
facet_plot <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point(size = 2) +
  facet_wrap(~ Species) +
  theme_minimal() +
  labs(title = "Faceted Scatter Plots of Iris Dataset")

print(facet_plot)


# 3. Animated Time-Series Visualization

p <- ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, 
                           size = pop, color = continent)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_x_log10() +
  labs(title = 'Year: {frame_time}', 
       x = 'GDP per Capita (log scale)', 
       y = 'Life Expectancy') +
  theme_minimal() +
  transition_time(year) +
  ease_aes('linear')

anim <- animate(p, duration = 15, fps = 20, width = 800, height = 600, renderer = gifski_renderer())
anim_save("animated_plot.gif", animation = anim, path = "C:/Users/Abhijit/Downloads")
