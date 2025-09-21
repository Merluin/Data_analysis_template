# Header ----
#  Programmer:  QUETTIER THOMAS
#  Description: usefull fonctions 
#  Update:      20/09/25

# Stat functions: ----
#' Compute Eta Squared from afex::aov_ez Output
#'
#' @description
#' Calculates eta squared effect sizes for each term in an ANOVA table
#' returned by `afex::aov_ez()`.
#'
#' @param fit An object of class `afex_aov` from the `afex::aov_ez()` function.
#'
#' @return A tibble with two columns:
#' \describe{
#'   \item{Term}{The name of the term in the ANOVA}
#'   \item{Eta_Squared}{Eta squared effect size for each term}
#' }
#'
#' @examples
#' \dontrun{
#'   library(afex)
#'   data(obk.long, package = "afex")
#'   fit <- aov_ez("id", "value", obk.long, within = c("phase", "hour"))
#'   eta2(fit)
#' }
#'
#' @export
eta2 <- function(fit) {
  # Check that the input is of the correct class
  if (!inherits(fit, "afex_aov")) {
    stop("Input must be an object returned by afex::aov_ez()")
  }
  
  # Extract ANOVA table from the fit
  anova_table <- fit$anova_table
  
  # Extract numeric values needed for eta squared
  num_df <- anova_table$`num Df`
  den_df <- anova_table$`den Df`
  mse     <- anova_table$`MSE`
  f_value <- anova_table$`F`
  
  # Compute sum of squares for effects and residuals
  ss_effect   <- num_df * mse * f_value
  ss_residual <- den_df * mse
  ss_total    <- ss_effect + ss_residual
  
  # Compute eta squared
  eta_squared <- ss_effect / ss_total
  
  # Prepare clean result as tibble (optional: replace with data.frame if not using tidyverse)
  result <- tibble::tibble(
    Term = rownames(anova_table),
    Eta_Squared = eta_squared
  )
  
  return(result)
}

#' Compute Performance Metrics per Participant
#'
#' @description
#' Computes error rate, slow response rate, and a total performance index
#' for each participant based on accuracy (`acc`) and reaction time (`rt`).
#'
#' @param dataset A data.frame or tibble containing the variables `id`, `acc`, and `rt`.
#'        - `id`: participant identifier
#'        - `acc`: accuracy score (1 = correct, 0 = error)
#'        - `rt`: reaction time in seconds
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{mean_error}{Proportion of incorrect responses}
#'   \item{mean_slow}{Proportion of correct responses with RT > 700ms}
#'   \item{total}{Sum of `mean_error` and `mean_slow`}
#' }
#'
#' @examples
#' \dontrun{
#'   compute_performance_metrics(my_data)
#' }
#'
#' @export
performance_metrics <- function(dataset) {
  if (!all(c("id", "acc", "rt") %in% names(dataset))) {
    stop("The dataset must contain columns: id, acc, and rt")
  }
  
  temp <- dataset %>%
    group_by(id) %>%
    summarise(
      n = n(),
      error = sum(acc == 0, na.rm = TRUE),                         # Trials with errors
      slow  = sum(rt > 0.7 & acc != 0, na.rm = TRUE),              # Slow correct responses
      mean_error = error / n,
      mean_slow = ifelse((n - error) > 0, slow / (n - error), NA), # Avoid division by zero
      total = mean_error + mean_slow
    ) %>%
    ungroup() %>%
    select(-id, -n)
  
  return(temp)
}

#' Compute Mean RT per Condition (Correct & Fast Trials)
#'
#' @description
#' Computes the mean reaction time (RT) per condition for each participant,
#' including only trials where accuracy is correct (`acc == 1`) and RT ≤ 700 ms.
#'
#' @param data A data.frame or tibble with columns: `id`, `condition`, `acc`, `rt`.
#'
#' @return A tibble with one row per participant and one column per condition,
#'         each prefixed with `"rt_"`, containing the mean RT.
#'
#' @examples
#' \dontrun{
#'   compute_rt_summary(dataset_exp1)
#' }
#'
#' @export
rt_summary <- function(data) {
  if (!all(c("id", "condition", "acc", "rt") %in% names(data))) {
    stop("Data must contain columns: id, condition, acc, rt.")
  }
  
  # List of all unique IDs
  all_ids <- data %>% distinct(id)
  
  # Compute mean RT per id x condition (for correct & fast trials)
  rt_filtered <- data %>%
    filter(acc == 1, rt <= 0.7) %>%
    group_by(id, condition) %>%
    summarise(mean_rt = mean(rt, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = condition, values_from = mean_rt)
  
  # Join back to all_ids to keep everyone
  all_ids %>%
    left_join(rt_filtered, by = "id") %>%
    rename_with(~ paste0("rt_", .x), .cols = -id)
}

#' Compute Mean Accuracy per Condition
#'
#' @description
#' Computes the mean accuracy (`acc`) per condition for each participant.
#'
#' @param data A data.frame or tibble with columns: `id`, `condition`, `acc`.
#'
#' @return A tibble with one row per participant and one column per condition,
#'         each prefixed with `"acc_"`, containing the mean accuracy.
#'
#' @examples
#' \dontrun{
#'   compute_acc_summary(dataset_exp1)
#' }
#'
#' @export
acc_summary <- function(data) {
  if (!all(c("id", "condition", "acc") %in% names(data))) {
    stop("Data must contain columns: id, condition, acc.")
  }
  
  data %>%
    group_by(id, condition) %>%
    summarise(mean_acc = mean(acc, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = condition, values_from = mean_acc) %>%
    rename_with(~ paste0("acc_", .x), .cols = -id) %>%
    select(-id)
}


#' Compute Signal Detection Theory (SDT) Metrics
#'
#' @description Computes d′ (sensitivity) and c (bias) from binary classification data,
#'              given a signal and noise condition. Applies a correction to avoid infinite z-scores.
#'
#' @param data A data.frame or tibble containing at least the columns `condition` and `acc`.
#'             - `condition`: indicates trial type (signal or noise)
#'             - `acc`: binary accuracy (1 = correct, 0 = incorrect)
#' @param signal A character value indicating the name of the signal condition.
#' @param noise A character value indicating the name of the noise condition.
#' @param ... Optional grouping variables (e.g., participant ID, block, etc.)
#'
#' @return A tibble containing Hits, Misses, False Alarms, Correct Rejections,
#'         Hit Rate (HR), False Alarm Rate (FAR), z-scores, d′ and c for each group.
#'
#' @examples
#' \dontrun{
#'   compute_sdt_metrics(my_data, signal = "fear", noise = "joy", id)
#' }
#'
#' @export
SDT <- function(data, signal, noise, ...) {
  signal <- as.character(signal)
  noise <- as.character(noise)
  
  # Check for required columns
  if (!all(c("condition", "acc") %in% names(data))) {
    stop("Data must contain 'condition' and 'acc' columns.")
  }
  
  sdt_data <- data %>%
    mutate(
      sdt = case_when(
        condition == signal & acc == 1 ~ "Hits",
        condition == signal & acc == 0 ~ "Misses",
        condition == noise & acc == 0 ~ "False.Alarms",
        condition == noise & acc == 1 ~ "Correct.Rejections",
        TRUE ~ NA_character_
      ),
      count = 1
    ) %>%
    filter(!is.na(sdt)) %>%
    group_by(across(c(..., sdt))) %>%
    summarise(count = sum(count), .groups = "drop") %>%
    pivot_wider(
      names_from = sdt,
      values_from = count,
      values_fill = list(count = 0)
    ) %>%
    mutate(
      HR = (Hits + 0.5) / ((Hits + Misses) + 1),  # Correction for extreme values
      FAR = (False.Alarms + 0.5) / ((False.Alarms + Correct.Rejections) + 1),
      ZHR = qnorm(HR),
      ZFAR = qnorm(FAR),
      d_prime = ZHR - ZFAR,
      c = -0.5 * (ZHR + ZFAR)
    )
  
  return(sdt_data)
}


# File manipulation ----

#' Convert Decimal Separator in a Column
#'
#' @description Converts a character vector using commas as decimal separators (e.g., from Excel) to numeric values with dots.
#'
#' @param column A character or factor vector with comma-separated decimal numbers.
#'
#' @return A numeric vector.
#' @export
convert_column <- function(column) {
  column <- as.character(column)  # Ensure it's character before substitution
  as.numeric(gsub(",", ".", column, fixed = TRUE))
}


#' Identify Outliers Based on ±2 Standard Deviations
#'
#' @description Identifies participants with values in a specific column that are more than 2 SDs from the mean.
#'
#' @param data A data.frame containing the data.
#' @param participant_column A string with the name of the participant ID column.
#' @param variable_column A string with the name of the variable to check for outliers.
#'
#' @return A vector of participant IDs considered outliers.
#' @export
identify_outliers <- function(data, participant_column, variable_column) {
  values <- data[[variable_column]]
  mean_value <- mean(values, na.rm = TRUE)
  sd_value <- sd(values, na.rm = TRUE)
  
  lower <- mean_value - 2 * sd_value
  upper <- mean_value + 2 * sd_value
  
  outliers <- data %>%
    filter(values < lower | values > upper) %>%
    pull({{ participant_column }})
  
  return(outliers)
}

#' Replace Values in a Column of a CSV File
#'
#' @description Opens a CSV file, replaces all values in a specified column with a new value, and saves it.
#'
#' @param target_column A string specifying the name of the column to be replaced.
#' @param new_value The value to assign to all entries in the column.
#'
#' @return No return. Saves the modified CSV to the original file.
#' @export
replace_csv <- function(target_column, new_value) {
  filepath <- file.choose()
  data <- read.csv(filepath, stringsAsFactors = FALSE)
  
  if (!target_column %in% colnames(data)) {
    stop("Column not found in the selected CSV.")
  }
  
  data[[target_column]] <- new_value
  write.csv(data, filepath, row.names = FALSE)
  
  message(sprintf("Replaced all values in column '%s' with '%s'.\nFile saved to: %s", target_column, new_value, filepath))
}

#' Add a New Column to a CSV File
#'
#' @description Opens a CSV file, adds a new column with a specified value, and saves it.
#'
#' @param new_column A string with the name of the new column to add.
#' @param new_value The value to populate the new column. Can be scalar or vector of correct length.
#'
#' @return No return. Saves the modified CSV to the original file.
#' @export
add_to_csv <- function(new_column, new_value) {
  filepath <- file.choose()
  data <- read.csv(filepath, stringsAsFactors = FALSE)
  
  # Check length compatibility
  if (length(new_value) != 1 && length(new_value) != nrow(data)) {
    stop("`new_value` must be of length 1 or match the number of rows in the CSV.")
  }
  
  data[[new_column]] <- new_value
  write.csv(data, filepath, row.names = FALSE)
  
  message(sprintf("Added new column '%s' to the file.\nFile saved to: %s", new_column, filepath))
}





#' Concatenate CSV files into a single dataset
#'
#' @description
#' Aggregates all `.csv` files located in a specified folder,
#' extracting the participant ID from the filename and cleaning
#' character columns. It then saves the resulting dataset as an `.RData` file.
#'
#' @param path The directory containing the `.csv` files.
#' @param dataset_name The name to use when saving the resulting `.RData` file.
#'
#' @return No return value. Saves the concatenated dataset in `data/` folder.
#' @export
dataset_concatenation <- function(path, dataset_name) {
  library(tidyverse)
  
  folder_dir <- path
  
  dataset <- list.files(path = folder_dir, full.names = TRUE) %>%
    lapply(function(x) {
      participant_id <- str_extract(basename(x), "\\d+")
      df <- read.csv(x, sep = ",", header = TRUE, stringsAsFactors = FALSE)
      df$participant <- as.numeric(participant_id)
      if ("gender" %in% names(df)) {
        df$gender <- as.character(df$gender)
      }
      return(df)
    }) %>%
    bind_rows() %>%
    mutate(across(where(is.character), str_remove_all, pattern = "[\\[|\\] ']"))
  
  save(dataset, file = paste0("data/", dataset_name, ".RData"))
}