# Define a function named 'add_numbers' that takes two arguments, 'a' and 'b'
add_numbers <- function(a, b) a+b
my_sum <- add_numbers(5,14)
print(my_sum)

numbers <- c(1, 3, 3, 2, 1, 3, 3, 4)
my_mode <- getmode(numbers)
print(my_mode)


max(numbers)
which.max()

num <- c(345, 528, 297, 301, 229, 950, 635, 836, 498, 335, 596, 524, 938, 411, 961, 805, 929, 700, 373, 975, 791, 583, 697, 599, 377, 824, 162, 476, 100, 894, 482, 104, 220, 893, 255, 120, 742, 201, 451, 170, 304, 433, 371, 459, 281, 126, 314, 937, 403, 623, 586, 161, 634, 300, 249, 535, 632, 332, 795, 385, 889, 676, 226, 539, 778, 307, 909, 237, 325, 561, 748, 641, 934, 316, 430, 873, 204, 431, 381, 210, 215, 328, 171, 166, 693, 838, 522, 930, 207, 437, 257, 568, 285, 242, 419, 662, 901, 891, 744, 321)
print(my_getmode(numbers))

# --------------------
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

my_getmode <- function(v){
 as.numeric(names(sort(table(v), decreasing = TRUE)[1])) 
}

my_getmode <- function(v) as.numeric(names(sort(table(v), decreasing = TRUE)[1])) 

data("iris")
apply(iris[,1:4],2,my_getmode)
apply(iris[,1:4],2,getmode)
#-------------------

iris %>% group_by(Species) %>% summaries(across(where(is.numeric)))
# Same result, harder to read:
summarise(group_by(iris, Species), across(where(is.numeric), list(
  mean = mean,
  sd = sd,
  min = min,
  max = max
)))

summarise(iris, list(mean = mean, sd = sd, min = min, max = max))

# Create a comprehensive summary
stats_summary <- list(
  central_tendency = list(mean = mean(iris$Sepal.Length), 
                          median = median(iris$Sepal.Length)),
  spread = list(sd = sd(iris$Sepal.Length),
                range = range(iris$Sepal.Length)),
  sample_size = nrow(iris)
)

stats_summary# Access the mean

ggpairs(iris, aes(color = Species),
        upper = list(continuous = wrap("cor", size = 4)),  # Bigger correlation text
        lower = list(continuous = wrap("points", alpha = 0.5)),  # Transparent points
        diag = list(continuous = wrap("densityDiag", alpha = 0.5)))+   # Transparent densities
  theme_minimal()

ggplot(iris, aes(y = Sepal.Length, fill = Species)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Density Plot of Sepal Length by Species")

par(mfrow = c(1,1))
data_vector <- c(12, 15, 18, 22, 25, 28, 30, 33, 35, 38, 40, 42, 45, 48, 50)
hist(data_vector)
hist(data_vector,
     main = "Distribution of Data", # Main title
     xlab = "Value",               # X-axis label
     ylab = "Frequency",           # Y-axis label
     col = "lightblue",            # Bar color
     border = "black",             # Bar border color
     breaks = 6)                   # Number of bins

df <- data.frame(values = c(12, 15, 18, 22, 25, 28, 30, 33, 35, 38, 40, 42, 45, 48, 50))

ggplot(df, aes(x = values)) +
  geom_histogram()
ggplot(df, aes(x = values)) +
  geom_histogram(bins = 7, fill = "lightgreen", color = "darkgreen") +
  labs(title = "Distribution of Data",
       x = "Value",
       y = "Count") +
  theme_minimal() # Apply a theme for a cleaner look


ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ cyl)
