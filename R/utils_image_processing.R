
reticulate::source_python("python/python_utils_image_processing.py")

#' transfer_rgb_to_greyscale_pangor 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

transfer_rgb_to_greyscale_pangor <- function(){}

#' downsample_greyscale_image_pangor 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

downsample_greyscale_image_pangor <- function(greyscale_img,
                                              width,
                                              height){
  
  downsampled_img <- downsample_greyscale(greyscale_img = greyscale_img,
                                          width = width,
                                          height = height)
  
  return(downsampled_img)
  
}