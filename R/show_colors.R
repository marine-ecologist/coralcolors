#' Display colors in a grid using ggplot2
#'
#' @description
#' `show_colors()` creates a simple color swatch grid with optional hex labels and optional title.
#'
#' @param colours Character vector of hex colors to display.
#' @param labels Logical, whether to show color codes as text labels. Default `TRUE`.
#' @param label_size Numeric, size of the label text.
#' @param label_color Color for the label text (default `"white"`).
#' @param ncol Number of columns in the grid. If `NULL` will be calculated automatically.
#' @param nrow Number of rows in the grid. If `NULL` will be calculated automatically.
#' @param title Optional plot title text.
#' @param title_size Numeric, size of the title text.
#' @param title_color Color of the title text.
#'
#' @return A `ggplot` object showing the color swatches.
#'
#' @examples
#' \dontrun{
#' show_colors(c("#0570c2", "#F9B908", "#ad8bb8", "#009382"), title = "My Palette")
#' }
#'
#' @export
show_colors <- function(colours,
                        labels = TRUE,
                        label_size = 5,
                        label_color = "white",
                        ncol = NULL,
                        nrow = NULL,
                        title = NULL,
                        title_size = 14,
                        title_color = "black") {
  n <- length(colours)
  if (is.null(ncol) && is.null(nrow)) ncol <- ceiling(sqrt(n))
  if (is.null(ncol)) ncol <- ceiling(n / nrow)
  if (is.null(nrow)) nrow <- ceiling(n / ncol)

  df <- data.frame(
    colour = colours,
    x = rep(seq_len(ncol), each = nrow)[1:n],
    y = rep(seq_len(nrow), ncol)[1:n]
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, fill = colour)) +
    ggplot2::geom_tile(color = "black", linewidth = 0.5) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_y_reverse() +
    ggplot2::theme_void() +
    ggplot2::coord_equal()

  if (labels) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = colour),
      color = label_color,
      size = label_size
    )
  }

  if (!is.null(title)) {
    p <- p +
      ggplot2::ggtitle(title) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          size = title_size,
          color = title_color,
          hjust = 0.5
        )
      )
  }

  p
}
