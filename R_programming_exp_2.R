# Load libraries
library(ggplot2)

# Parameters
set.seed(123)        # For reproducibility
n_trials <- 1000     # Number of repeated samples
sample_size <- 30    # Size of each sample

# --- BINOMIAL DISTRIBUTION ---
binom_means <- replicate(n_trials, mean(rbinom(sample_size, size = 10, prob = 0.5)))

# --- POISSON DISTRIBUTION ---
poisson_means <- replicate(n_trials, mean(rpois(sample_size, lambda = 4)))

# --- Histogram: Binomial ---
hist(
  binom_means,
  probability = TRUE,
  col = "skyblue",
  main = "CLT -Binomial Status Means",
  xlab = "Sample Mean"
)
curve(
  dnorm(x, mean = mean(binom_means), sd = sd(binom_means)),
  add = TRUE,
  col = "red",
  lwd = 2
)

# --- Histogram: Poisson ---
hist(
  poisson_means,
  probability = TRUE,
  col = "lightgreen",
  main = "CLT - Poisson Sample Means",
  xlab = "Sample Mean"
)
curve(
  dnorm(
    x,
    mean = mean(poisson_means),
    sd = sd(poisson_means)
  ),
  add = TRUE,
  col = "blue",
  lwd = 2
)

# --- Q-Q Plot: Binomial ---
qqnorm(binom_means, main = "Q-Q Plot: Binomial Sample Means")
qqline(binom_means, col = "red")

# --- Q-Q Plot: Poisson ---
qqnorm(poisson_means, main = "Q-Q Plot: Poisson Sample Means")
qqline(poisson_means, col = "blue")