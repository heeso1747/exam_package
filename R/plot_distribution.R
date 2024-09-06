#' Plot Distribution of Variables
#'
#' This function plots the distribution of numeric variables in a dataset.
#' @param data A data frame.
#' @param column The name of the column to plot.
#' @return A ggplot object.
#' @examples
#' data(mtcars)
#' plot_distribution(mtcars, "mpg")
#' @import ggplot2
#' @importFrom stats cor median
#' @export
plot_distribution <- function(data, column) {
  if (!is.data.frame(data)) stop("Input must be a data frame.")
  if (!column %in% names(data)) stop("Column not found in data.")
  if (!is.numeric(data[[column]])) stop("Column must be numeric.")

  # Plot the distribution
  ggplot2::ggplot(data, ggplot2::aes_string(x = column)) +
    ggplot2::geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
    ggplot2::labs(title = paste("Distribution of", column), x = column, y = "Count") +
    ggplot2::theme_minimal()
}
