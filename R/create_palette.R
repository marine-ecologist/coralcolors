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
