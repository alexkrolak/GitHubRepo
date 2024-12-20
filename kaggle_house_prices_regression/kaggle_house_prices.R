

# libraries
{
  library(tidyverse)
  library(data.table)
  library(skimr)
  library(DataExplorer)
  library(caret)
}

# UDFs
{
  # Function to clean column names
  make_names <- function(col_names){
    # Transform the column names
    cleaned_names <- col_names %>%
      tolower() %>%                   # Convert to lowercase
      gsub("[^a-z0-9_]", "_", .) %>%  # Replace spaces and special characters with underscores
      gsub("_+", "_", .) %>%            # Replace multiple underscores with a single underscore
      sapply(function(col) {            # Add "x" to column names starting with numbers
        if (grepl("^[0-9]", col)) paste0("x", col) else col
      })
    
    # Return the transformed column names
    return(cleaned_names)
  }
  
  # Recode missing to UNK
  code_missing_to_UNK <- function(df) {
    # Ensure the input is a dataframe
    if (!is.data.frame(df)) {
      stop("Input must be a dataframe.")
    }
    
    # Iterate over character columns and replace NAs with "UNK"
    df[] <- lapply(df, function(col) {
      if (is.character(col)) {
        col[is.na(col)] <- "UNK"
      }
      else if (is.numeric(col)){
        col <- col %>% as.character()
        col[is.na(col)] <- "UNK"
      }
      return(col)
    })
    
    return(df)
  }
  
  # Convert my column to factor with smallest value as baseline
  convert_to_factor_with_mode <- function(column) {
    # Ensure the input is numeric
    if (!is.numeric(column)) {
      stop("Input column must be numeric.")
    }
    
    # Find the most frequent value (mode)
    mode_value <- names(which.max(table(column)))
    
    # Convert the column to a factor
    factor_column <- factor(column)
    
    # Set the mode as the baseline
    factor_column <- relevel(factor_column, ref = as.character(mode_value))
    
    return(factor_column)
  }
  
  # Changes all columns to use their mode as the baseline
  update_columns_to_mode_factor <- function(df, columns) {
    # Ensure the input is a dataframe
    if (!is.data.frame(df)) {
      stop("Input must be a dataframe.")
    }
    
    # Check that all specified columns exist in the dataframe
    if (!all(columns %in% colnames(df))) {
      stop("Some specified columns do not exist in the dataframe.")
    }
    
    # Apply the conversion to each specified column
    for (col in columns) {
      if (is.numeric(df[[col]])) {
        df[[col]] <- convert_to_factor_with_mode(df[[col]])
      } else {
        warning(paste("Skipping column", col, "because it is not numeric."))
      }
  }
  
  return(df)
}
}

# Data Import
{
  
  
  train_dt <- fread("C://Users/GottaGoFaster/Documents/github_personal/kaggle_house_prices_regression/train.csv") %>% data.table
  names(train_dt) %<>% {.} %>% make_names()
  cols_to_convert <- c("mssubclass", "overallcond", "overallqual" ) %>% union(train_dt %>% dplyr::select_if(is.character) %>% names)
  # convert columns to factors and baseline at the mode:
  train_dt <- train_dt %>% update_columns_to_mode_factor(., cols_to_convert)
  
  
  test_dt <- fread("C://Users/GottaGoFaster/Documents/github_personal/kaggle_house_prices_regression/test.csv") %>% data.table
  names(test_dt) %<>% {.} %>% make_names()
  # convert columns to factors and baseline at the mode:
  test_dt <- test_dt %>% update_columns_to_mode_factor(., cols_to_convert)
  
  
  # Import sample submission to validate final export
  sample_submission <- fread("C://Users/GottaGoFaster/Documents/github_personal/kaggle_house_prices_regression/sample_submission.csv") %>% data.table

}

# EDA
{
  # Run a very naive linear model to see how far I can predict already:
  {
    # Define what columns should be in the model's explanatory variables
    my_cols <- c("overallqual", "mosold", "yrsold", "housestyle", "exterior1st", "exterior2nd", "x1stflrsf", "mssubclass", "neighborhood") %>% unique()
    # Define the model formula as just the y variable with basic linear behavior for all explanatory vars
    formula <- paste0("saleprice ~ ", paste0(my_cols, collapse = " + ")) %>% as.formula
    
    # Split the data into 80% training and 20% validation
    data_split <- sample(1:nrow(train_dt), size = 0.8 * nrow(train_dt))
    training_data <- train_dt[data_split, ]
    validation_data <- train_dt[-data_split, ]
    
    # Handle categorical columns
    {
      # Categorical columns
      categorical_col_names <- train_dt %>% 
        dplyr::select(my_cols) %>%
        dplyr::select_if(~!is.numeric(.)) %>%
        names()
      
      # If there are categorical vars, handle them
      if (length(categorical_col_names) > 0) {
        
        # Ensure all categorical columns in both training and validation datasets
        for (col in categorical_col_names) {
          # Convert to character first
          training_data[[col]] <- as.character(training_data[[col]])
          validation_data[[col]] <- as.character(validation_data[[col]])
          
          # Replace NAs with "UNK"
          training_data[[col]][is.na(training_data[[col]])] <- "UNK"
          validation_data[[col]][is.na(validation_data[[col]])] <- "UNK"
        }
        
        # Ensure consistent levels for all categorical variables across both training and validation data
        all_levels <- lapply(categorical_col_names, function(cat) {
          unique(training_data[[cat]])  # Use only training data levels
        })
        names(all_levels) <- categorical_col_names
        
        # Apply the consistent factor levels to both training and validation data
        for (cat in categorical_col_names) {
          # Convert to factor and set levels to include all observed levels in the training data only
          training_data[[cat]] <- factor(training_data[[cat]], levels = all_levels[[cat]])
          
          # For validation data, remove any levels not seen in training data
          validation_data[[cat]] <- factor(validation_data[[cat]], levels = all_levels[[cat]])
          
          # Remove rows from the validation set where there are unseen factor levels
          unseen_levels <- !(validation_data[[cat]] %in% levels(training_data[[cat]]))
          validation_data <- validation_data[!unseen_levels, ]
        }
      }
    }
    
    # Fit the linear model on the training data
    model <- lm(formula, data = training_data)
    
    # Predict on the validation data
    validation_predictions <- predict(model, newdata = validation_data)
    
    # Compare predictions vs true values
    validation_results <- data.frame(
      true = validation_data$saleprice,
      predicted = validation_predictions
    )
    
    # Calculate performance metrics (including R-squared)
    validation_metrics <- postResample(validation_results$predicted, validation_results$true) %>% round(4)
    
    # Display the R-squared value along with other metrics (RMSE, MAE)
    print(validation_metrics)
    
    
    # Display the summary of the model
    summary(model)
    
  }
}











