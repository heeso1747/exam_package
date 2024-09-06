#' Generate a Correlation Matrix
#'
#' This function generates a correlation matrix for all numeric variables in a dataset.
#' @param data A data frame.
#' @return A correlation matrix.
#' @importFrom stats cor median
#' @examples
#' data(mtcars)
#' correlation_matrix(mtcars)
#' @importFrom stats cor median
#' @export
correlation_matrix <- function(data) {
  if (!is.data.frame(data)) stop("Input must be a data frame.")

  # Select numeric columns
  numeric_data <- data[sapply(data, is.numeric)]

  # Calculate correlation matrix
  cor_matrix <- cor(numeric_data, use = "complete.obs")

  return(cor_matrix)
}
