# 1. Importing the Data
covid_data <- read.csv("G:/Other computers/My Laptop/Parth/Coding/R_programming/covid_data.txt", stringsAsFactors = FALSE)

# Display first few rows
head(covid_data)

# Check structure
str(covid_data)

# 2. Handling Missing Values

# Count missing values
colSums(is.na(covid_data))

# Option 1: Remove missing values
covid_clean <- na.omit(covid_data)

# Option 2: Impute missing numeric values with median
library(dplyr)
covid_clean <- covid_data %>%
  mutate(
    confirmed = ifelse(is.na(confirmed), median(confirmed, na.rm = TRUE), confirmed),
    deaths = ifelse(is.na(deaths), median(deaths, na.rm = TRUE), deaths),
    recovered = ifelse(is.na(recovered), median(recovered, na.rm = TRUE), recovered)
  )

# 3. Detecting Outliers
# Boxplots before cleaning
par(mfrow=c(1,2))
boxplot(covid_clean$confirmed, main="Confirmed Cases - Before Cleaning", col="skyblue")
boxplot(covid_clean$deaths, main="Deaths - Before Cleaning", col="lightpink")

# Outlier detection function
find_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5*IQR
  upper <- Q3 + 1.5*IQR
  which(x < lower | x > upper)
}

# Detect outliers in confirmed cases
find_outliers(covid_clean$confirmed)

# 4. Handling Outliers
# Function to cap outliers
cap_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5*IQR
  upper <- Q3 + 1.5*IQR
  x[x < lower] <- lower
  x[x > upper] <- upper
  return(x)
}

covid_clean$confirmed <- cap_outliers(covid_clean$confirmed)
covid_clean$deaths <- cap_outliers(covid_clean$deaths)

# Boxplots after cleaning
par(mfrow=c(1,2))
boxplot(covid_clean$confirmed, main="Confirmed Cases - After Cleaning", col="skyblue")
boxplot(covid_clean$deaths, main="Deaths - After Cleaning", col="lightpink")

# 5. Summary & Save Clean Data
summary(covid_clean)
write.csv(covid_clean, "covid19_cleaned.csv", row.names = FALSE)
