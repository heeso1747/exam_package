#' Quick Summary of a Data Frame
#'
#' This function generates a summary of a dataset, including the mean, median, min, max, and NA count for each numeric column.
#' @param data A data frame to summarize.
#' @return A data frame containing summary statistics.
#' @importFrom stats cor median
#' @examples
#' data(mtcars)
#' quick_summary(mtcars)
#' @export
quick_summary <- function(data) {
  if (!is.data.frame(data)) stop("Input must be a data frame.")

  # Summary for numeric columns
  numeric_cols <- sapply(data, is.numeric)
  summary_data <- data.frame(
    Variable = names(data)[numeric_cols],
    Mean = sapply(data[, numeric_cols], mean, na.rm = TRUE),
    Median = sapply(data[, numeric_cols], median, na.rm = TRUE),
    Min = sapply(data[, numeric_cols], min, na.rm = TRUE),
    Max = sapply(data[, numeric_cols], max, na.rm = TRUE),
    NAs = sapply(data[, numeric_cols], function(x) sum(is.na(x)))
  )

  return(summary_data)
}
