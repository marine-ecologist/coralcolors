#' Extract a color palette from an image
#'
#' @param image_path Path to the image file.
#' @param n Number of colors to return.
#' @param sat_min Minimum saturation threshold to filter greys.
#' @param bright_min Minimum brightness.
#' @param bright_max Maximum brightness.
#'
#' @return Character vector of hex colors.
#' @export
create_palette <- function(image_path, n = 5, sat_min = 20, bright_min = 100, bright_max = 200) {
  img <- magick::image_read(image_path) |> magick::image_scale("500")
  im  <- imager::magick2cimg(img)
  df  <- as.data.frame(im, wide = "c") |>
    dplyr::mutate(hex = grDevices::rgb(c.1, c.2, c.3)) |>
    dplyr::count(hex, sort = TRUE)

  rgb_vals <- grDevices::col2rgb(df$hex)
  saturation <- apply(rgb_vals, 2, function(x) max(x) - min(x))
  brightness <- colMeans(rgb_vals)
  keep <- saturation > sat_min & brightness > bright_min & brightness < bright_max

  df <- df[keep, ] |> dplyr::slice_max(n, n = n)
  df$hex
}

#' Darken colors by blending toward black
#'
#' @param hex Character vector of hex colors.
#' @param amount Fraction [0,1], 0 = no change, 1 = full black.
#'
#' @return Darkened hex colors.
#' @export
darken <- function(hex, amount = 0.2) {
  m <- grDevices::col2rgb(hex) / 255
  m <- m * (1 - amount)
  apply(m, 2, function(x) grDevices::rgb(x[1], x[2], x[3]))
}

#' Lighten colors by blending toward white
#'
#' @param hex Character vector of hex colors.
#' @param amount Fraction [0,1], 0 = no change, 1 = full white.
#'
#' @return Lightened hex colors.
#' @export
lighten <- function(hex, amount = 0.2) {
  m <- grDevices::col2rgb(hex) / 255
  m <- m + (1 - m) * amount
  apply(m, 2, function(x) grDevices::rgb(x[1], x[2], x[3]))
}

#' bleach colors by blending toward white
#'
#' @param hex Character vector of hex colors.
#' @param amount Fraction [0,1], 0 = no change, 1 = full white.
#'
#' @return bleached hex colors.
#' @export
bleach <- function(hex, lighten = 0.5, mute = 0.7) {
  # lighten: 0-1 toward white
  # mute:    0-1 toward gray (desaturate)
  m <- grDevices::col2rgb(hex)/255

  # 1. lighten toward white
  if (lighten > 0) m <- m + (1 - m) * lighten

  # 2. mute/desaturate toward gray
  if (mute > 0) {
    lum <- 0.299*m[1,] + 0.587*m[2,] + 0.114*m[3,]
    m <- sweep(m, 2, lum, function(col, l) l + (col - l)*(1 - mute))
  }

  apply(m, 2, function(x) grDevices::rgb(x[1], x[2], x[3]))
}


#' Interactive palette plot with Plotly
#'
#' @param hex Character vector of hex colors.
#'
#' @return Plotly bar chart of colors with hover hex.
#' @export
plot_palette_plotly <- function(hex) {
  plotly::plot_ly(
    x = seq_along(hex),
    y = rep(1, length(hex)),
    type = "bar",
    marker = list(color = hex),
    text = hex,
    hoverinfo = "text"
  ) |>
    plotly::layout(
      xaxis = list(showticklabels = FALSE, showgrid = FALSE),
      yaxis = list(showticklabels = FALSE, showgrid = FALSE),
      bargap = 0,
      showlegend = FALSE
    )
}

#' Generate continuous color gradient
#'
#' @param cols Vector of base colors.
#' @param n Number of gradient colors.
#'
#' @return Character vector of interpolated colors.
#' @export
continuous_pal <- function(cols, n = 10) {
  cols <- cols[!is.na(cols) & cols != ""]
  if (length(cols) < 2) stop("Need at least two colors")
  pal_fun <- scales::gradient_n_pal(cols, space = "Lab")
  pal_fun(seq(0, 1, length.out = n))
}

#' Select diverging colors spaced apart in Lab space
#'
#' @param pal Vector of base colors.
#' @param n Number of colors to return.
#'
#' @return Character vector of diverging colors.
#' @export
diverging_pal <- function(pal, n = 7) {
  pal <- pal[!is.na(pal) & pal != ""]
  lab <- t(grDevices::col2rgb(pal))
  lab <- farver::convert_colour(lab, from = "rgb", to = "lab")

  idx <- c(1, length(pal))
  while (length(idx) < n) {
    remaining <- setdiff(seq_len(nrow(lab)), idx)
    d <- sapply(remaining, function(i)
      min(stats::dist(rbind(lab[idx, , drop=FALSE], lab[i, , drop=FALSE])))
    )
    idx <- c(idx, remaining[which.max(d)])
  }
  pal[idx]
}

#' Quick base R palette bar plot
#'
#' @param hex Character vector of hex colors.
#'
#' @return Bar plot of colors.
#' @export
plot_palette <- function(hex) {
  graphics::barplot(rep(1, length(hex)),
                    col = hex,
                    border = NA,
                    axes = FALSE,
                    space = 0)
}
