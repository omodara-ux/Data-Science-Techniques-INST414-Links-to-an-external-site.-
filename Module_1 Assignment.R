# Install required packages if they're not already installed
required_packages <- c("tidyverse", "corrplot", "car")

for(package in required_packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Error handling function
handle_error <- function(expr) {
  tryCatch(
    expr,
    error = function(e) {
      message("Error: ", e$message)
      return(NULL)
    },
    warning = function(w) {
      message("Warning: ", w$message)
      return(NULL)
    }
  )
}

# Read and clean the data
student_mat <- handle_error({
  read.csv("student-mat.csv", sep = ";")
})
student_por <- handle_error({
  read.csv("student-por.csv", sep = ";")
})

if (is.null(student_mat) || is.null(student_por)) {
  stop("Failed to read the data files. Please check if the files exist and are readable.")
}

# Merge datasets based on common attributes
common_cols <- intersect(names(student_mat), names(student_por))
student_data <- merge(student_mat, student_por, by = common_cols, all = TRUE)

# Convert categorical variables to factors
categorical_cols <- c('school', 'sex', 'address', 'famsize', 'Pstatus', 'guardian', 'schoolsup', 'famsup', 'paid', 'activities', 'nursery', 'higher', 'internet', 'romantic', 'Mjob', 'Fjob')
student_data[categorical_cols] <- lapply(student_data[categorical_cols], as.factor)

# Convert numeric variables
numeric_cols <- c('age', 'Medu', 'Fedu', 'traveltime', 'studytime', 'failures', 'famrel', 'freetime', 'goout', 'Dalc', 'Walc', 'health', 'absences', 'G1', 'G2', 'G3', 'Mjob', 'Fjob')
student_data[numeric_cols] <- lapply(student_data[numeric_cols], as.numeric)

# Check for missing values
missing_values <- colSums(is.na(student_data))
print("Missing values in each column:")
print(missing_values)

# Correlation analysis for numeric variables
numeric_data <- student_data[numeric_cols]
correlation_matrix <- cor(numeric_data)

# Identify top 3 statistically significant socio-economic factors using linear regression
model <- handle_error({
  lm(G3 ~ Medu + Fedu + Dalc + studytime + failures + absences + Fjob, data = student_data)
})

if (!is.null(model)) {
  summary_stats <- summary(model)
  print("Model summary:")
  print(summary_stats)
  
  # Extract top 3 significant factors based on p-values
  sig_factors <- summary_stats$coefficients[,4] # p-values
  top_3_factors <- names(sort(sig_factors)[2:4]) # Exclude Intercept
  print("Top 3 statistically significant socio-economic factors impacting G3:")
  print(top_3_factors)
  
  # Create correlation plot4
  png("correlation_plot.png")
  corrplot(correlation_matrix, method = "color", type = "upper", 
           tl.col = "black", tl.srt = 45)
  dev.off()
  
  # Create residual plots for model diagnostics
  png("residual_plots.png")
  par(mfrow = c(2,2))
  plot(model)
  dev.off()
}






