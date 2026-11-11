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

#Step5: Median imputation for numerical variables

# Calculate the median age using training data only
age_median <- median(
  train_data$age,
  na.rm = TRUE
)

# Replace missing age values in training data
train_data$age[
  is.na(train_data$age)
] <- age_median

# Replace missing age values in testing data
# using the training median
test_data$age[
  is.na(test_data$age)
] <- age_median

# Display the median used for imputation
print("Training median age:")
print(age_median)

#Step6: Mode imputation for categorical variables

# Define a function to calculate the mode
calculate_mode <- function(x) {

  # Remove missing values before calculating the mode
  x <- x[!is.na(x)]

  # Find the unique values
  unique_values <- unique(x)

  # Count the frequency of each value
  frequencies <- tabulate(
    match(x, unique_values)
  )

  # Return the most frequently occurring value
  unique_values[which.max(frequencies)]
}

# Calculate the mode of ethnicity using training data
ethnicity_mode <- calculate_mode(
  train_data$ethnicity
)

# Replace missing ethnicity values in training data
train_data$ethnicity[
  is.na(train_data$ethnicity)
] <- ethnicity_mode

# Replace missing ethnicity values in testing data
# using the training mode
test_data$ethnicity[
  is.na(test_data$ethnicity)
] <- ethnicity_mode

# Display the selected mode
print("Training ethnicity mode:")
print(ethnicity_mode)

#Step7: Investigate outliers
#Step7.1: Create an outlier detection function

# Define a function to detect outliers using IQR
detect_outliers <- function(x) {

  # Remove missing values
  x <- x[!is.na(x)]

  # Calculate first quartile
  Q1 <- quantile(x, 0.25)

  # Calculate third quartile
  Q3 <- quantile(x, 0.75)

  # Calculate interquartile range
  IQR_value <- IQR(x)

  # Calculate lower boundary
  lower_bound <- Q1 - 1.5 * IQR_value

  # Calculate upper boundary
  upper_bound <- Q3 + 1.5 * IQR_value

  # Identify outliers
  outliers <- x[
    x < lower_bound | x > upper_bound
  ]

  # Return outlier information
  return(list(
    Q1 = Q1,
    Q3 = Q3,
    IQR = IQR_value,
    Lower_Bound = lower_bound,
    Upper_Bound = upper_bound,
    Outlier_Count = length(outliers)
  ))
}
#step7.2: Investigate age outliers

# Investigate outliers in patient age
age_outliers <- detect_outliers(data$age)

# Display age outlier results
print("Age outlier analysis:")
print(age_outliers)
#step7.3: Investigate BMI outliers

# Investigate outliers in patient BMI
bmi_outliers <- detect_outliers(data$bmi)

# Display BMI outlier results
print("BMI outlier analysis:")
print(bmi_outliers)

#step7.4: Create boxplots

# Create boxplot for patient age
ggplot(data, aes(y = age)) +

  geom_boxplot(fill = "lightblue") +

  labs(
    title = "Boxplot of Patient Age",
    y = "Age (Years)"
  ) +

  theme_minimal()

# Save age boxplot
ggsave(
  "plots/age_boxplot.png",
  width = 7,
  height = 5,
  dpi = 300
)

# Create boxplot for patient BMI
ggplot(data, aes(y = bmi)) +

  geom_boxplot(fill = "lightgreen") +

  labs(
    title = "Boxplot of Patient BMI",
    y = "Body Mass Index (BMI)"
  ) +

  theme_minimal()

# Save BMI boxplot
ggsave(
  "plots/bmi_boxplot.png",
  width = 7,
  height = 5,
  dpi = 300
)

#TASK 3: FEATURE PREPARATION AND ENGINEERING
#Step 3.1: Remove unnecessary columns

# Specify columns that must not be used as predictors
excluded_columns <- c(
  "encounter_id",
  "patient_id",
  "hospital_id",
  "icu_id",
  "Unnamed: 83",
  "apache_4a_hospital_death_prob",
  "apache_4a_icu_death_prob"
)

# Also identify any APACHE predicted death-probability
# columns with slightly different names
apache_probability_columns <- grep(
  "apache.*(death.*prob|prob.*death)",
  names(train_data),
  ignore.case = TRUE,
  value = TRUE
)

# Identify unnamed columns
unnamed_columns <- grep(
  "^Unnamed",
  names(train_data),
  ignore.case = TRUE,
  value = TRUE
)

# Combine all columns to exclude
columns_to_remove <- unique(c(
  excluded_columns,
  apache_probability_columns,
  unnamed_columns
))

# Keep only excluded columns that actually exist
columns_to_remove <- intersect(
  columns_to_remove,
  names(train_data)
)

# Remove excluded columns from training data
train_data <- train_data[
  , !(names(train_data) %in% columns_to_remove),
  drop = FALSE
]

# Remove the same columns from testing data
test_data <- test_data[
  , !(names(test_data) %in% columns_to_remove),
  drop = FALSE
]

# Display removed columns
print("Columns removed:")
print(columns_to_remove)

# Display the new dimensions
print("Training data dimensions:")
print(dim(train_data))

print("Testing data dimensions:")
print(dim(test_data))

#Step 3.2: Convert variables into appropriate data types

# 3.2 Convert the target variable into a factor

# Define the outcome categories
train_data$hospital_death <- factor(
  train_data$hospital_death,
  levels = c("Survived", "Died")
)

test_data$hospital_death <- factor(
  test_data$hospital_death,
  levels = c("Survived", "Died")
)

# Convert character predictors into factors
# These are categorical variables
character_columns <- names(train_data)[
  sapply(train_data, is.character)
]

for (column in character_columns) {

  train_data[[column]] <- as.factor(
    train_data[[column]]
  )

  test_data[[column]] <- factor(
    test_data[[column]],
    levels = levels(train_data[[column]])
  )
}

# Display the updated data structure
str(train_data)
#Step 3.3: Complete missing-value preparation

# 3.3 Handle remaining missing values

# Identify all predictor columns except the target
predictor_columns <- setdiff(
  names(train_data),
  "hospital_death"
)

# Identify columns that contain no observed training values
all_missing_columns <- predictor_columns[
  sapply(
    train_data[predictor_columns],
    function(x) all(is.na(x))
  )
]

# Remove completely missing predictors
train_data <- train_data[
  , !(names(train_data) %in% all_missing_columns),
  drop = FALSE
]

test_data <- test_data[
  , !(names(test_data) %in% all_missing_columns),
  drop = FALSE
]

# Update predictor list
predictor_columns <- setdiff(
  names(train_data),
  "hospital_death"
)

# Store training imputation values
imputation_values <- list()

# Process each predictor
for (column in predictor_columns) {

  # Numerical variables use median imputation
  if (is.numeric(train_data[[column]])) {

    # Calculate median using training data only
    fill_value <- median(
      train_data[[column]],
      na.rm = TRUE
    )

    # Store the calculated median
    imputation_values[[column]] <- fill_value

    # Replace missing values in training data
    train_data[[column]][
      is.na(train_data[[column]])
    ] <- fill_value

    # Apply the same median to testing data
    test_data[[column]][
      is.na(test_data[[column]])
    ] <- fill_value

  } else {

    # Categorical variables use mode imputation
    fill_value <- calculate_mode(
      as.character(train_data[[column]])
    )

    # Store the training mode
    imputation_values[[column]] <- fill_value

    # Replace missing values in training data
    train_data[[column]][
      is.na(train_data[[column]])
    ] <- fill_value

    # Apply the same mode to testing data
    test_data[[column]][
      is.na(test_data[[column]])
    ] <- fill_value
  }
}

# Display remaining missing values
print("Remaining missing values in training data:")
print(sum(is.na(train_data)))

print("Remaining missing values in testing data:")
print(sum(is.na(test_data)))

#Step 3.4: Create at least two meaningful features

# 3.4 Feature engineering

# Create age groups in the training dataset
train_data$age_group <- cut(
  train_data$age,
  breaks = c(-Inf, 40, 60, Inf),
  labels = c("Young", "Middle_Aged", "Older"),
  right = FALSE
)

# Create the same age groups in the testing dataset
test_data$age_group <- cut(
  test_data$age,
  breaks = c(-Inf, 40, 60, Inf),
  labels = c("Young", "Middle_Aged", "Older"),
  right = FALSE
)

# Display age group frequencies
print("Training age group distribution:")
print(table(train_data$age_group))


# Create BMI categories in training data
train_data$bmi_category <- cut(
  train_data$bmi,
  breaks = c(-Inf, 18.5, 25, 30, Inf),
  labels = c(
    "Underweight",
    "Normal",
    "Overweight",
    "Obese"
  ),
  right = FALSE
)

# Create the same BMI categories in testing data
test_data$bmi_category <- cut(
  test_data$bmi,
  breaks = c(-Inf, 18.5, 25, 30, Inf),
  labels = c(
    "Underweight",
    "Normal",
    "Overweight",
    "Obese"
  ),
  right = FALSE
)

# Display BMI category frequencies
print("Training BMI category distribution:")
print(table(train_data$bmi_category))


# Display the first ten records of the new features
head(
  train_data[
    c("age", "age_group", "bmi", "bmi_category")
  ],
  10
)

# Display the updated training data structure
str(train_data)

#Step 3.5: Scale numerical predictors
#Step 3.5.1: Identify numerical predictors

# Identify numerical predictors in training data
numeric_predictors <- names(train_data)[
  sapply(train_data, is.numeric)
]

# Remove the target variable if present
numeric_predictors <- setdiff(
  numeric_predictors,
  "hospital_death"
)

# Display numerical predictors
print("Numerical predictors:")
print(numeric_predictors)

#Step 3.5.2: Calculate scaling parameters from training data

# Calculate the mean of each numerical predictor
# using training data only
scaling_means <- sapply(
  train_data[numeric_predictors],
  mean,
  na.rm = TRUE
)

# Calculate standard deviation using training data only
scaling_sds <- sapply(
  train_data[numeric_predictors],
  sd,
  na.rm = TRUE
)

# Replace zero or invalid standard deviations with 1
# This prevents division by zero
scaling_sds[
  is.na(scaling_sds) | scaling_sds == 0
] <- 1

# Display scaling parameters
print("Training means:")
print(scaling_means)

print("Training standard deviations:")
print(scaling_sds)
#Step 3.5.3: Apply scaling to training and testing data

# Create copies of the datasets for scaled modelling
train_scaled <- train_data
test_scaled <- test_data

# Standardise numerical training predictors
train_scaled[numeric_predictors] <- as.data.frame(
  scale(
    train_data[numeric_predictors],
    center = scaling_means,
    scale = scaling_sds
  )
)

# Apply the SAME training parameters to testing predictors
test_scaled[numeric_predictors] <- as.data.frame(
  scale(
    test_data[numeric_predictors],
    center = scaling_means,
    scale = scaling_sds
  )
)

# Display the first six scaled records
head(train_scaled)
head(test_scaled)
#Step 3.6: Verify the prepared datasets

# 3.6 Final verification of prepared datasets

# Check training dataset dimensions
print("Prepared training dataset:")
print(dim(train_data))

# Check testing dataset dimensions
print("Prepared testing dataset:")
print(dim(test_data))

# Check whether both datasets have identical columns
print("Do training and testing columns match?")
print(identical(names(train_data), names(test_data)))

# Check missing values after imputation
print("Missing values in training data:")
print(sum(is.na(train_data)))

print("Missing values in testing data:")
print(sum(is.na(test_data)))

# Check the target variable distribution
print("Training outcome distribution:")
print(table(train_data$hospital_death))

print("Testing outcome distribution:")
print(table(test_data$hospital_death))

# Confirm that the two engineered features exist
print("Newly engineered features:")
print(c("age_group", "bmi_category"))

# Display the final prepared dataset structure
str(train_data)

identical(names(train_data), names(test_data))
sum(is.na(train_data))
sum(is.na(test_data))


# ==========================================================
# TASK 4: CLASSIFICATION MODEL DEVELOPMENT

# Install required packages if not already installed
# Run installation commands only when necessary

# install.packages("caret")
# install.packages("randomForest")
# install.packages("pROC")

# Load required packages
library(caret)
library(randomForest)
library(pROC)

# Set random seed for reproducibility
set.seed(2026)
#Step 4.2: Verify the stratified training and testing split

# 4.2 Verify the existing stratified split

# Display training dataset dimensions
print("Training dataset dimensions:")
print(dim(train_data))

# Display testing dataset dimensions
print("Testing dataset dimensions:")
print(dim(test_data))

# Calculate training class frequencies
training_frequency <- table(train_data$hospital_death)

# Calculate testing class frequencies
testing_frequency <- table(test_data$hospital_death)

# Display training class proportions
print("Training class proportions:")
print(round(prop.table(training_frequency) * 100, 2))

# Display testing class proportions
print("Testing class proportions:")
print(round(prop.table(testing_frequency) * 100, 2))

# Display training and testing percentages
print("Training percentage:")
print(round(nrow(train_data) / nrow(data) * 100, 2))

print("Testing percentage:")
print(round(nrow(test_data) / nrow(data) * 100, 2))


#Step 4.3: Prepare the target variable for classification

# 4.3 Set the target factor levels

# Put Died first because it is our positive class
train_data$hospital_death <- factor(
  train_data$hospital_death,
  levels = c("Died", "Survived")
)

test_data$hospital_death <- factor(
  test_data$hospital_death,
  levels = c("Died", "Survived")
)

# Update the scaled datasets as well
train_scaled$hospital_death <- factor(
  train_scaled$hospital_death,
  levels = c("Died", "Survived")
)

test_scaled$hospital_death <- factor(
  test_scaled$hospital_death,
  levels = c("Died", "Survived")
)

# Verify target classes
levels(train_data$hospital_death)

# Display class frequencies
table(train_data$hospital_death)
#Step 4.4: Develop Model 1 — Logistic Regression
#Step 4.4.1: Prepare predictors and outcome

# 4.4 Logistic Regression

# Remove hospital_death from the predictor variables
# The target must not be used as an input predictor
logistic_train_x <- train_scaled[
  , setdiff(names(train_scaled), "hospital_death"),
  drop = FALSE
]

logistic_test_x <- test_scaled[
  , setdiff(names(test_scaled), "hospital_death"),
  drop = FALSE
]

# Extract target variable
logistic_train_y <- train_scaled$hospital_death

logistic_test_y <- test_scaled$hospital_death

# Display the number of predictors
ncol(logistic_train_x)
#Step 4.4.2: Configure cross-validation

# Configure three-fold cross-validation
logistic_control <- trainControl(

  # Use three-fold cross-validation
  method = "cv",

  # Divide training data into three folds
  number = 3,

  # Calculate class probabilities
  classProbs = TRUE,

  # Use ROC-AUC for model evaluation
  summaryFunction = twoClassSummary,

  # Save cross-validation predictions
  savePredictions = "final"
)
#Step 4.4.3: Train Logistic Regression

# Train Logistic Regression using cross-validation
set.seed(2026)

logistic_model <- train(

  # Predictor variables
  x = logistic_train_x,

  # Target variable
  y = logistic_train_y,

  # Select Logistic Regression algorithm
  method = "glm",

  # Specify binomial classification
  family = binomial(),

  # Cross-validation configuration
  trControl = logistic_control,

  # Select model using ROC-AUC
  metric = "ROC"
)

# Display model results
print(logistic_model)

# Display model details
summary(logistic_model$finalModel)

#Step 4.5: Develop Model 2 — Random Forest
#Step 4.5.1: Prepare Random Forest predictors

# 4.5 Random Forest classification

# Remove the target variable from training predictors
rf_train_x <- train_data[
  , setdiff(names(train_data), "hospital_death"),
  drop = FALSE
]

# Remove the target variable from testing predictors
rf_test_x <- test_data[
  , setdiff(names(test_data), "hospital_death"),
  drop = FALSE
]

# Extract training target
rf_train_y <- train_data$hospital_death

# Extract testing target
rf_test_y <- test_data$hospital_death

# Display number of predictors
ncol(rf_train_x)

#Step 4.5.2: Select parameters for Random Forest

# Calculate the number of predictor variables
number_of_predictors <- ncol(rf_train_x)

# Generate three candidate mtry values
mtry_values <- unique(
  pmax(
    1,
    pmin(
      number_of_predictors,
      round(c(
        sqrt(number_of_predictors) / 2,
        sqrt(number_of_predictors),
        sqrt(number_of_predictors) * 2
      ))
    )
  )
)

# Create the Random Forest tuning grid
rf_grid <- expand.grid(
  mtry = mtry_values
)

# Display the parameter values to be tested
print("Random Forest mtry values:")
print(rf_grid)

#Step 4.5.3: Configure cross-validation for Random Forest

# Configure Random Forest cross-validation
rf_control <- trainControl(

  # Use three-fold cross-validation
  method = "cv",

  # Use three folds
  number = 3,

  # Calculate class probabilities
  classProbs = TRUE,

  # Evaluate using ROC-AUC
  summaryFunction = twoClassSummary,

  # Save validation predictions
  savePredictions = "final"
)
#Step 4.5.4: Train the Random Forest model

# Train Random Forest using three-fold cross-validation
set.seed(2026)

rf_model <- train(

  # Training predictors
  x = rf_train_x,

  # Training outcome
  y = rf_train_y,

  # Random Forest algorithm
  method = "rf",

  # Test different mtry values
  tuneGrid = rf_grid,

  # Number of trees
  ntree = 100,

  # Calculate variable importance
  importance = TRUE,

  # Cross-validation settings
  trControl = rf_control,

  # Select model based on ROC-AUC
  metric = "ROC"
)

# Display Random Forest model results
print(rf_model)

# Display the selected mtry value
print("Best Random Forest parameter:")
print(rf_model$bestTune)

# Display cross-validation results
print(rf_model$results)
#Step 4.6: Compare cross-validation results

# 4.6 Compare cross-validation performance

# Extract Logistic Regression ROC results
logistic_roc <- max(logistic_model$results$ROC)

# Extract the best Random Forest ROC result
rf_roc <- max(rf_model$results$ROC)

# Create a comparison table
cross_validation_results <- data.frame(

  Model = c(
    "Logistic Regression",
    "Random Forest"
  ),

  Cross_Validation_ROC_AUC = c(
    logistic_roc,
    rf_roc
  )
)

# Display comparison
print("Cross-validation model comparison:")

print(
  cross_validation_results
)
