series <- "moths-and-moonlets"
licence <- "ccby"

write_readme_md <- function(series, licence) {
  
  base_url <- "https://art.djnavarro.net/gallery/"
  title <- paste0("# series-", series)
  link <- paste0(
    "[", stringr::str_to_sentence(series), "](",
    base_url, series, "/)"
  )  
  lc <- "UNSPECIFIED"
  if(licence == "ccby") lc <- "CC-BY-4.0"
  if(licence == "cc0") lc <- "CC0 public domain"
  blurb <- paste(
    "is an art repository by Danielle Navarro. The images in this",
    "repository are released under a [", lc, "](./LICENSE.md) licence."
  )
  
  brio::write_lines(
    text = c(title, "", paste(link, blurb)),
    path = here::here("README.md")
  )
  
}

write_index <- function(series) {
  docs <- here::here("docs")
  if(!dir.exists(docs)) dir.create("docs")
  brio::write_lines(
    text = paste0("series-", series),
    path = file.path(docs, "index.html")
  )
  brio::write_lines(
    text = "",
    path = file.path(docs, ".nojekyll")
  )
}

if(licence == "ccby") usethis::use_ccby_license()
if(licence == "cc0") usethis::use_cc0_license()
write_index(series)
write_readme_md(series, licence)

