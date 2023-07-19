#include <Rcpp.h>
using namespace Rcpp;

// function to be called from R
// [[Rcpp::export]]
NumericMatrix raster_data(int iter, int layers, int pixels, double zoom, double alpha) {
  
  NumericMatrix image(pixels, pixels); // initially zero
  NumericMatrix coeffs(9, layers);
  
  // set coefficients
  for(int i = 0; i < 9; i++) {
    for(int j = 0; j < layers; j++) {
      coeffs(i, j) = R::runif(-2, 2);
    }
  }
  
  // image level constant: these affect the celestial spheres
  double y_offset = R::runif(-.5, .5);
  double x_offset = R::runif(-.5, .5);
  double r_offset = R::runif(0, 6.28);
  
  // image level constant: these shape the lissajous attractor
  // scale multiplies the bigger one, s_offset rotates it
  double s_offset = R::runif(0, 6.28);
  double scale = R::runif(2, 6);
  
  // set image matrix to zeros
  for(int r = 0; r < pixels; r++) {
    for(int c = 0; c < pixels; c++) {
      image(c, r) = 0;
    }
  }
  
  // iterate
  int layer;
  int variant;
  
  // convenience variables
  double s = 0;
  double u = .3;
  
  // offsets
  double x_sh = 0;
  double y_sh = 0;
  
  // indices for storing coordinates
  int x_ind;
  int y_ind;
  
  // values for current state
  double x = 0;
  double y = 0;
  double z = 0;
  
  // values for storing temp coords
  double x_tmp = 0;
  double y_tmp = 0;
  
  // values for previous state
  double x_old = R::runif(-1, 1);
  double y_old = R::runif(-1, 1);
  double z_old = R::runif(-1, 1);
  
  
  // introduces some noise and echo to the attractor 
  const double v_shift = 10;
  
  // shift & scale for the shatter (in a box around the circle)
  const double s_shatter = R::runif(1.5, 3.5);
  const double s_scale = R::runif(.5, 2);

  
  // iterate...
  for(int it = 1; it < iter; it++) {
    
    layer = rand() % layers;   // which affine transform to use?
    variant = rand() % 5;      // which variant function to use?
    
    // coordinates after random transform
    x = x_old + coeffs(0, layer) * x_old + coeffs(1, layer) * y_old + coeffs(2, layer);
    y = y_old + coeffs(3, layer) * x_old + coeffs(4, layer) * y_old + coeffs(5, layer);
    z = z_old + coeffs(6, layer) * x_old + coeffs(7, layer) * y_old + coeffs(8, layer);
    
    // apply function to the transformed coordinates
    
    // variant 0 is the path itself
    
    if(variant == 0) {
      s = x*x + y*y + z*z + 2;
      x = x + v_shift * x/s;
      y = y + v_shift * y/s;
      //z = z + v_shift * z/s;
      if(z < 0) z = 0;
      
    // variants 1 and 2 are the celestial spheres 
    } else if(variant == 1){
      
      s = 1;
      x = cos(x) + x_offset;
      y = cos(y) - 1.3 + y_offset;
      u = x*x + y*y;
      
      x_tmp = s * x / u;
      y_tmp = s * y / u;
      
      // convert to x,y coords of a circle
      x = x_tmp * cos(r_offset) - y_tmp * sin(r_offset);
      y = x_tmp * sin(r_offset) + y_tmp * cos(r_offset);
      
      // colour
      z = s;
      if(z < 0) z = 0; 
      
    } else if(variant == 2){
      
      s = 1.2 + (rand() % 5)/3;

      x = cos(x) + x_offset;
      y = cos(y) - .9 + (rand() % 3) + y_offset;
      u = x*x + y*y;
      
      x_tmp = scale * s * x / u;
      y_tmp = scale * s * y / u;
      
      // convert to x,y coords of a circle
      x = x_tmp * cos(r_offset) - y_tmp * sin(r_offset);
      y = x_tmp * sin(r_offset) + y_tmp * cos(r_offset);

      // colour
      z = s;
      if(z < 0) z = 0; 
      
    // variants 3 and 4 are the lissajous attractor 
    } else if(variant == 3){
      s = x * y;
      x_tmp = cos(s) * sin(s);
      y_tmp = cos(s) + sin(s);
      x = x_tmp * cos(s_offset) - y_tmp * sin(s_offset);
      y = x_tmp * sin(s_offset) + y_tmp * cos(s_offset);
      //z = 1 + sin(z);
    } else {
      s = x * y + s_offset;
      x_tmp = cos(s) * scale * sin(s);
      y_tmp = (cos(s) + sin(s)) * scale;
      x = x_tmp * cos(s_offset) - y_tmp * sin(s_offset);
      y = x_tmp * sin(s_offset) + y_tmp * cos(s_offset);
      //z = 1 + sin(z);
    }
    
    // compute indices to be updated
    x_ind = int (x * pixels * zoom) + pixels/2;
    y_ind = int (y * pixels * zoom) + pixels/2;
    
    // store results if they fall within the range
    if(variant == 1 | variant == 2) {
      if(x_ind >= 0 & x_ind < pixels) {
        if(y_ind >= 0 & y_ind < pixels) {
          image(x_ind, y_ind) = alpha * z + (1- alpha) * image(x_ind, y_ind);
        }
      }
    }
    
    // move new to old
    x_old = x;
    y_old = y;
    z_old = z * .5 + z_old * .5; 
  }
  
  return image;
}


