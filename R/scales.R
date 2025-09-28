#' @export
scale_color_coral <- function(palette = "acanthastrea", discrete = TRUE, direction = 1, name = waiver(), ...) {
  pal <- coral_palettes[[palette]]
  if (direction == -1) pal <- rev(pal)
  if (discrete) ggplot2::scale_color_manual(values = pal, name = name, ...)
  else ggplot2::scale_color_gradientn(colors = pal, name = name, ...)
}

#' @export
scale_colour_coral <- scale_color_coral

#' @export
scale_fill_coral <- function(palette = "acanthastrea", discrete = TRUE, direction = 1, name = waiver(), ...) {
  pal <- coral_palettes[[palette]]
  if (direction == -1) pal <- rev(pal)
  if (discrete) ggplot2::scale_fill_manual(values = pal, name = name, ...)
  else ggplot2::scale_fill_gradientn(colors = pal, name = name, ...)
}

#' @export
scale_colour_coral_d <- function(palette = "acanthastrea", direction = 1, name = waiver(), ...) {
  pal <- coral_palettes[[palette]]
  if (direction == -1) pal <- rev(pal)
  ggplot2::scale_colour_manual(values = pal, name = name, ...)
}

#' @export
scale_color_coral_d <- scale_colour_coral_d

#' @export
scale_fill_coral_d <- function(palette = "acanthastrea", direction = 1, name = waiver(), ...) {
  pal <- coral_palettes[[palette]]
  if (direction == -1) pal <- rev(pal)
  ggplot2::scale_fill_manual(values = pal, name = name, ...)
}
