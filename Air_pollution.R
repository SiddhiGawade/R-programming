## =========================================================
## Practical: Air-Quality Data Cleaning Using R
## Loops, Functions, Error Handling, Missing Data Handling
## =========================================================

file_path <- "C:/Users/student/Downloads/beijing+multi+site+air+quality+data/PRSA2017_Data_20130301-20170228/PRSA_Data_20130301-20170228/PRSA_Data_Aotizhongxin_20130301-20170228.csv"

## ---------------------------------------------------------
## TASK 1: Import and Inspect the Dataset
## ---------------------------------------------------------
data <- tryCatch(
  {
    read.csv(file_path)
  },
  error = function(e) {
    if (grepl("cannot open", e$message)) {
      cat("Error: The file was not found or cannot be opened.\n")
    } else if (grepl("more columns than column names|invalid|EOF", e$message)) {
      cat("Error: The file format is incorrect.\n")
    } else {
      cat("Error:", e$message, "\n")
    }
    return(NULL)
  }
)

if (!is.null(data)) {
  cat("Dataset imported successfully.\n\n")

  cat("First Six Records:\n")
  print(head(data))

  cat("\nStructure of Dataset:\n")
  str(data)

  cat("\nNumber of Rows and Columns:\n")
  cat("Rows =", nrow(data), "\n")
  cat("Columns =", ncol(data), "\n")

  cat("\nDoes dataset contain missing values?\n")
  print(any(is.na(data)))

  cat("\nTotal Missing Values:\n")
  print(sum(is.na(data)))
}

## ---------------------------------------------------------
## TASK 2: Understand NA, NULL, and NaN
## ---------------------------------------------------------
cat("\n===== TASK 2: NA vs NULL vs NaN =====\n")

# NA - an unavailable/missing observation
temperature <- c(28, 30, NA, 32)
cat("\ntemperature vector:\n")
print(temperature)
cat("is.na(temperature):\n")
print(is.na(temperature))

# NULL - an absent/empty R object
missing_object <- NULL
cat("\nmissing_object:\n")
print(missing_object)
cat("is.null(missing_object):", is.null(missing_object), "\n")

# NaN - an undefined numerical result
undefined_value <- 0 / 0
cat("\nundefined_value (0/0):", undefined_value, "\n")
cat("is.nan(undefined_value):", is.nan(undefined_value), "\n")

# Cross-checks to show these are NOT interchangeable
cat("\nCross-checks:\n")
cat("is.na(NaN)  ->", is.na(NaN), "  (NaN also counts as NA)\n")
cat("is.nan(NA)  ->", is.nan(NA), "  (but NA is NOT NaN)\n")
cat("is.null(NA) ->", is.null(NA), "\n")

## Dataset-level NA / NaN checks
colSums(is.na(data))
colSums(is.nan(as.matrix(data)))

is.null(data)
any(is.na(data))
sum(is.na(data))
any(is.nan(as.matrix(data)))
sum(is.nan(as.matrix(data)))

## ---------------------------------------------------------
## TASK 3: Missing-Value Summary Function
## ---------------------------------------------------------
missing_summary <- function(df) {
  vars <- c("PM2.5", "PM10", "SO2", "NO2", "TEMP", "WSPM", "wd")

  summary_df <- data.frame(
    Variable = character(),
    Total_Records = numeric(),
    Missing_Values = numeric(),
    Missing_Percentage = numeric(),
    stringsAsFactors = FALSE
  )

  for (var in vars) {
    if (var %in% names(df)) {
      total <- nrow(df)
      missing <- sum(is.na(df[[var]]))
      percent <- (missing / total) * 100

      summary_df <- rbind(summary_df,
                           data.frame(
                             Variable = var,
                             Total_Records = total,
                             Missing_Values = missing,
                             Missing_Percentage = round(percent, 2)
                           ))

      if (percent > 20) {
        warning(paste(var, "contains more than 20% missing values"))
      }
    } else {
      warning(paste("Variable", var, "not found in dataset"))
    }
  }

  return(summary_df)
}

result <- missing_summary(data)
cat("\n===== TASK 3: Missing-Value Summary =====\n")
print(result)

## Selected variables used throughout Tasks 8 & 9
variables <- c("PM2.5", "PM10", "SO2", "NO2", "TEMP", "WSPM", "wd")

## *** Snapshot BEFORE any cleaning happens (fixes Task 8/9) ***
missing_before_vec <- sapply(variables, function(v) sum(is.na(data[[v]])))

## ---------------------------------------------------------
## TASK 4: Identify Invalid Numerical Results
## ---------------------------------------------------------
data$pollution_ratio <- data$PM2.5 / data$PM10

cat("\n===== TASK 4: pollution_ratio checks =====\n")
cat("Number of NA values:\n");        print(sum(is.na(data$pollution_ratio)))
cat("Number of NaN values:\n");       print(sum(is.nan(data$pollution_ratio)))
cat("Number of Infinite values:\n");  print(sum(is.infinite(data$pollution_ratio)))
cat("Number of Positive Infinity values:\n")
print(sum(data$pollution_ratio == Inf, na.rm = TRUE))
cat("Number of Negative Infinity values:\n")
print(sum(data$pollution_ratio == -Inf, na.rm = TRUE))

data$pollution_ratio[
  is.nan(data$pollution_ratio) | is.infinite(data$pollution_ratio)
] <- NA

cat("\nAfter Replacement:\n")
cat("NA values:", sum(is.na(data$pollution_ratio)), "\n")
cat("NaN values:", sum(is.nan(data$pollution_ratio)), "\n")
cat("Infinite values:", sum(is.infinite(data$pollution_ratio)), "\n")

## ---------------------------------------------------------
## TASK 5: Handle Missing Numerical Values Using a Loop
## ---------------------------------------------------------
numeric_variables <- c("PM2.5", "PM10", "SO2", "NO2", "TEMP", "WSPM")

cat("\n===== TASK 5: Numeric loop cleaning =====\n")
for (var in numeric_variables) {
  if (var %in% names(data)) {
    missing_before <- sum(is.na(data[[var]]))
    median_value <- median(data[[var]], na.rm = TRUE)
    data[[var]][is.na(data[[var]])] <- median_value
    missing_after <- sum(is.na(data[[var]]))

    cat("\n-----------------------------\n")
    cat("Variable:", var, "\n")
    cat("Missing Values Before:", missing_before, "\n")
    cat("Median Used:", median_value, "\n")
    cat("Missing Values After:", missing_after, "\n")
  } else {
    cat("\nColumn", var, "does not exist in the dataset.\n")
  }
}

## ---------------------------------------------------------
## TASK 6: Handle Missing Categorical Values
## ---------------------------------------------------------
calculate_mode <- function(x) {
  x <- x[!is.na(x)]
  unique_values <- unique(x)
  mode_value <- unique_values[which.max(tabulate(match(x, unique_values)))]
  return(mode_value)
}

missing_before_wd <- sum(is.na(data$wd))
mode_wd <- calculate_mode(data$wd)
data$wd[is.na(data$wd)] <- mode_wd
missing_after_wd <- sum(is.na(data$wd))

cat("\n===== TASK 6: wd cleaning =====\n")
cat("Mode of wd:", mode_wd, "\n")
cat("Missing Values Before:", missing_before_wd, "\n")
cat("Missing Values After:", missing_after_wd, "\n")

## *** Snapshot AFTER cleaning (fixes Task 8/9) ***
missing_after_vec <- sapply(variables, function(v) sum(is.na(data[[v]])))

## ---------------------------------------------------------
## TASK 7: Reusable Error-Handling Function
## ---------------------------------------------------------
clean_variable <- function(df, var_name) {
  tryCatch(
    {
      if (!(var_name %in% names(df))) {
        stop("Variable does not exist in the dataset.")
      }
      if (!is.numeric(df[[var_name]])) {
        stop("The selected variable is not numerical.")
      }
      if (all(is.na(df[[var_name]]))) {
        stop("The variable contains only missing values.")
      }

      median_value <- median(df[[var_name]], na.rm = TRUE)
      if (is.na(median_value)) {
        stop("Median cannot be calculated.")
      }

      df[[var_name]][is.na(df[[var_name]])] <- median_value
      cat("Variable", var_name, "cleaned successfully.\n")
      cat("Median used:", median_value, "\n")
      return(df[[var_name]])
    },
    error = function(e) {
      cat("Error:", e$message, "\n")
      return(NULL)
    }
  )
}

cat("\n===== TASK 7: clean_variable() demonstrations =====\n")
pm25_clean <- clean_variable(data, "PM2.5")          # success case
invisible(clean_variable(data, "no_such_column"))     # variable does not exist
invisible(clean_variable(data, "wd"))                 # categorical, not numeric

## ---------------------------------------------------------
## TASK 8: Compare Missing Values Before and After Cleaning
## ---------------------------------------------------------
comparison <- data.frame(
  Variable        = variables,
  Missing_Before  = as.numeric(missing_before_vec[variables]),
  Missing_After   = as.numeric(missing_after_vec[variables]),
  stringsAsFactors = FALSE
)
comparison$Values_Replaced <- comparison$Missing_Before - comparison$Missing_After

cat("\n===== TASK 8: Comparison Table =====\n")
print(comparison)

## ---------------------------------------------------------
## TASK 9: Visualization
## ---------------------------------------------------------
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("reshape2", quietly = TRUE)) install.packages("reshape2")
library(ggplot2)
library(reshape2)

missing_comparison <- data.frame(
  Variable        = variables,
  Before_Cleaning = as.numeric(missing_before_vec[variables]),
  After_Cleaning  = as.numeric(missing_after_vec[variables])
)

plot_data <- melt(
  missing_comparison,
  id.vars = "Variable",
  variable.name = "Cleaning_Status",
  value.name = "Missing_Count"
)

ggplot(plot_data, aes(x = Variable, y = Missing_Count, fill = Cleaning_Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Comparison of Missing Values Before and After Data Cleaning",
    x = "Variables",
    y = "Number of Missing Values",
    fill = "Cleaning Status"
  ) +
  theme_minimal()

## ---------------------------------------------------------
## TASK 10: Export the Cleaned Dataset
## ---------------------------------------------------------
write.csv(data, "cleaned_air_quality_data.csv", row.names = FALSE)
cat("\nCleaned dataset exported successfully.\n")
