
output_dir <- "series-moths-and-moonlets"

convert_file <- function(path, input_size, output_size) {

  img <- magick::image_read(here::here(output_dir, input_size, path))
  img <- magick::image_resize(img, paste0(output_size, "x", output_size))
  magick::image_write(img,
    path = here::here(output_dir, output_size, path)
  )
  rm(img)
  gc()
  cat(path, "\n")
}

base_size <- 3000
images <- list.files(here::here(output_dir, base_size))
purrr::walk(images, ~convert_file(.x, base_size, 800))
