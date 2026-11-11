# TASK 1: DATA LOADING AND INITIAL EXPLORATION
1.1
# Install ggplot2 if it is not already installed
# Run this installation command only once
install.packages("ggplot2")

# Load ggplot2 library for data visualisation
library(ggplot2)

# Set random seed for reproducibility
set.seed(2026)


# Import the supplied CSV dataset into R
data <- read.csv("patient_survival_prediction.csv")

# Display a message confirming successful loading
print("Dataset loaded successfully")
getwd()


# Display the number of rows and columns
dim(data)

# Display the total number of observations
nrow(data)

# Display the total number of variables
ncol(data)

# Display all column names in the dataset
names(data)


# Examine the structure and data types of all variables
str(data)


# Generate descriptive statistics for the dataset
summary(data)


# Display the first six patient records
head(data)

# Display the first ten patient records
head(data, 10)

# Display the last six patient records
tail(data)


#1.9
# Identify numerical variables
numerical_variables <- names(data)[
  sapply(data, is.numeric)
]

# Display numerical variables
print("Numerical variables:")
print(numerical_variables)

# Identify categorical variables
categorical_variables <- names(data)[
  sapply(data, function(x) is.character(x) ||
           is.factor(x))
]

# Display categorical variables
print("Categorical variables:")
print(categorical_variables)

# Count numerical variables
length(numerical_variables)

# Count categorical variables
length(categorical_variables)

#Step 1.10: Here we Calculated the frequency and percentage of hospital_death

# Calculate the frequency of each outcome
death_frequency <- table(data$hospital_death)

# Display the frequency
print("Hospital death frequency:")
print(death_frequency)

# Calculate the percentage of each outcome
death_percentage <- prop.table(death_frequency) * 100

# Display percentages
print("Hospital death percentage:")
print(round(death_percentage, 2))

# Combine frequency and percentage into one table
death_distribution <- data.frame(
  Outcome = names(death_frequency),
  Frequency = as.vector(death_frequency),
  Percentage = round(as.vector(death_percentage), 2)
)

# Display the final distribution table
print(death_distribution)

#Step 1.11: here we Visualized 1 — Target class distribution

# Convert hospital_death into a factor for plotting
data$hospital_death <- factor(
  data$hospital_death,
  levels = c(0, 1),
  labels = c("Survived", "Died")
)

# Create a bar chart of hospital death distribution
ggplot(data, aes(x = hospital_death, fill = hospital_death)) +

  # Draw bars representing the number of records
  geom_bar() +

  # Add chart title and axis labels
  labs(
    title = "Distribution of Hospital Death",
    x = "Patient Outcome",
    y = "Number of Patients",
    fill = "Outcome"
  ) +

  # Apply a clean visual theme
  theme_minimal()

#Step 1.12: Visualisation 2 — Distribution of patient age

# Create a histogram showing patient age distribution
ggplot(data, aes(x = age)) +

  # Draw histogram bars using 30 bins
  geom_histogram(
    bins = 30,
    fill = "steelblue",
    color = "black"
  ) +

  # Add chart title and axis labels
  labs(
    title = "Distribution of Patient Age",
    x = "Patient Age (Years)",
    y = "Number of Patients"
  ) +

  # Apply a clean theme
  theme_minimal()
#Step 1.13: we Saved your visualisations

# Create a folder for storing plots if it does not exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Save the hospital death distribution plot
ggsave(
  "plots/hospital_death_distribution.png",
  width = 8,
  height = 5,
  dpi = 300
)

# Save the patient age distribution plot
ggsave(
  "plots/patient_age_distribution.png",
  width = 8,
  height = 5,
  dpi = 300
)


# TASK 2: DATA QUALITY, MISSING VALUES AND OUTLIERS

#Step1: Identify missing values in the dataset

# Calculate the total number of missing values in each column
missing_values <- colSums(is.na(data))

# Display missing values for every variable
print("Missing values by variable:")
print(missing_values)

# Calculate the total number of missing values in the dataset
total_missing <- sum(is.na(data))

# Display the total number of missing values
print("Total missing values in the dataset:")
print(total_missing)

# Calculate the percentage of missing values in each column
missing_percentage <- colMeans(is.na(data)) * 100

# Display missing-value percentages
print("Missing-value percentages:")
print(round(missing_percentage, 2))

#Step 2: Identify the five variables with the highest missing values

# Create a table containing variable names and missing-value counts
missing_summary <- data.frame(
  Variable = names(missing_values),
  Missing_Count = as.numeric(missing_values),
  Missing_Percentage = as.numeric(missing_percentage)
)

# Sort variables from highest to lowest missing-value count
missing_summary <- missing_summary[
  order(-missing_summary$Missing_Count),
]

# Display the five variables with the highest missing values
top_five_missing <- head(missing_summary, 5)

print("Top five variables with the highest missing values:")
print(top_five_missing)

#Step3: Visualise missing values

# Select the top five variables for visualisation
ggplot(top_five_missing,
       aes(x = reorder(Variable, Missing_Count),
           y = Missing_Count)) +

  # Draw bars representing missing-value counts
  geom_col(fill = "steelblue") +

  # Display variable names horizontally
  coord_flip() +

  # Add chart title and axis labels
  labs(
    title = "Top Five Variables with Missing Values",
    x = "Variables",
    y = "Number of Missing Values"
  ) +

  # Apply a clean theme
  theme_minimal()

# Save the missing-value plot
ggsave(
  "plots/top_five_missing_values.png",
  width = 9,
  height = 6,
  dpi = 300
)
#Step4: Select suitable imputation methods

# Load caret for stratified data splitting
library(caret)

# Set seed to make the split reproducible
set.seed(2026)

# Create a temporary stratified 80% training split
train_index <- createDataPartition(
  data$hospital_death,
  p = 0.80,
  list = FALSE
)

# Create training dataset
train_data <- data[train_index, ]

# Create testing dataset
test_data <- data[-train_index, ]

# Display the dimensions of both datasets
dim(train_data)
dim(test_data)
