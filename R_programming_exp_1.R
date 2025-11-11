# Load libraries
library(dplyr)
library(ggplot2)
library(psych)     # for describe()
library(GGally)    # for ggpairs()

# 1. Load Iris dataset
data(iris)
head(iris)  # first 6 rows

summary(iris)
describe(iris[, 1:4])

# 3. Calculate mode function (R doesn’t have built-in mode for numeric)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Alternatively we can use to get mode same but written in beginner friendly manner
# getmode <- function(v) {
#   uniqv <- unique(v)        # Step 1: Get unique values
#   positions <- match(v, uniqv)  # Step 2: Map to positions
#   counts <- tabulate(positions) # Step 3: Count frequencies
#   max_position <- which.max(counts) # Step 4: Find most frequent position
#   uniqv[max_position]        # Step 5: Get the actual value
# }

#My method to define mode and using shortcut to define function
# my_getmode <- function(v) as.numeric(names(sort(table(v), decreasing = TRUE)[1]))

# Median and Mode
# apply(data, margin(1-rows & 2-columns), function)
apply(iris[, 1:4], 2, median)
apply(iris[, 1:4], 2, getmode)

# 4. Variance, Standard Deviation, Range
apply(iris[, 1:4], 2, var)
apply(iris[, 1:4], 2, sd)
apply(iris[, 1:4], 2, function(x)
  max(x) - min(x))


# 5. Group by Species for mean and sd
# %>% (pipe operator) passes the iris dataset to the next function
# Think of it as "take iris and then..."
iris %>%
  group_by(Species) %>%
  summarise(across(where(is.numeric), list(
    mean = mean,
    sd = sd,
    min = min,
    max = max
  )))

# # STEP 1: Start with iris data
# iris
# 
# # STEP 2: Pipe to group_by
# iris %>% group_by(Species)
# 
# # STEP 3: Now grouped, pipe to summarise
# iris %>% group_by(Species) %>% summarise(...)
# 
# # STEP 4: Inside summarise, use across to select columns
# summarise(across(where(is.numeric), ...))
# 
# # STEP 5: For selected columns, apply multiple functions
# across(where(is.numeric), list(mean = mean, sd = sd, min = min, max = max))
# 
# # FINAL RESULT: A summary table with 3 rows (species) and multiple statistics


# 6. Visualization
# Histograms for all numeric variables
par(mfrow = c(2, 2))
for (i in 1:4) {
  hist(
    iris[, i],
    main = names(iris)[i],
    xlab = names(iris)[i],
    col = "lightblue",
    border = "black"
  )
}

# Boxplot by species for each variable
par(mfrow = c(2, 2))
for (i in 1:4) {
  boxplot(
    iris[, i] ~ iris$Species,
    main = names(iris)[i],
    col = c("lightgreen", "lightblue", "pink")
  )
}

# iris[, i] - The measurement values (y-axis)
# 
# iris$Species - The grouping variable (x-axis)
# 
# ~ - Formula notation: "plot measurement BY species"

# Scatterplot matrix (pairwise relationships)
ggpairs(iris, aes(color = Species))

# Density plot for one variable (e.g., Sepal.Length)
ggplot(iris, aes(x = Sepal.Length, fill = Species)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Sepal Length by Species")


