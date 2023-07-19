seeds <- 5599:5999

pollinate <- function(seed) {
  
  library(Rcpp)
  library(dplyr)
  library(cairobasic)
  
  sys_id <- "55"
  sys_name <- "pollen"
  sourceCpp(here::here("source", paste0(sys_name, "_", sys_id, ".cpp")))

  output_dir <- here::here("image", paste0("sys_", sys_id))
  if(!dir.exists(output_dir)) dir.create(output_dir)
   
  # seed
  cat(seed, "\n")
  set.seed(seed)
  
  # fixed / default
  px <- 3000
  layers <- 100
  million <- 10^6
  iter <- 100 * million
  zoom <- .035 # small number = zoom out
  alpha <- .5
  
  
  # palette specification ---------------------------------------------------
  
  if(FALSE) {
    # random palette, sorted by hue    
    pal <- sample(colorir::colores$colour, 20) # base shades
    pal <- c(pal, "black", "black", "black", "white", "white", "white")
    sgn <- ifelse(runif(1) < .5, -1, 1) # flip palette order randomly
    ord <- order(sgn * rgb2hsv(col2rgb(pal))["v",]) # order by hue/sat/val
    pal <- pal[ord]
  }
  
  if(TRUE) {
    # "chunky" version of canned palette
    name <- sample(colorir::colores$palette_name, 1)
    pal <- colorir::colores$colour[colorir::colores$palette_name == name[1]]
    pal <- sample(pal)
    nr <- 20
    pal <- unlist(lapply(pal, rep, times = nr))
  }
  
  # convert to full length vector
  ncl <- 1024
  pal <- (colorRampPalette(pal))(ncl) 
  bg <- "white" # not actually used
  
  
  
  # helper functions --------------------------------------------------------
  
  generate_data <- function(seed, iter, layers, px, zoom, alpha) {
    set.seed(seed)
    df <- raster_data(iter, layers, px, zoom, alpha)
    return(df)
  }
  
  transform_data <- function(df) {
    df <- rank(df)
    df <- df - min(df)
    df <- df / max(df)
    df <- as.integer(df * (ncl - 1)) + 1
    return(df)
  }
  
  colourise_data <- function(df) {
    df <- pal[df]
    df <- matrix(df, px, px, byrow = TRUE)
    df <- t(df) # <- stupid place to do this!!!!!!!!!!!!!
    return(df)
  }
  
  render_data <- function(df, fpath, px, bg) {
    rs <- as.raster(df)
    jpeg(
      filename = fpath,
      width = px,
      height = px,
      bg = bg 
    )
    op <- par(mar = c(0,0,0,0))
    plot(rs)
    dev.off()
    par(op)
  }
  
  fpath <- function(seed) {
    dir <- paste0("sys_", sys_id)
    prefix <- paste0(sys_name, "_", sys_id, "_")
    fname <- paste0(prefix, seed, ".jpg")
    fp <- here::here("image", dir, fname)
    return(fp)
  }
  
  # generate the data -------------------------------------------------------
  
  cat("generating...\n")
  
  
  df1 <- generate_data(seed, iter, layers, px, zoom, alpha)
  
  cat("transforming...\n")
  
  rank1 <- transform_data(df1)
  cols1 <- colourise_data(rank1)
  
  cat("rendering...\n")
  
  render_data(cols1, fpath(seed), px, bg)
  
}

for(s in seeds) pollinate(s)