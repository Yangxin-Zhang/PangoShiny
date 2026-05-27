

reticulate::source_python("python/python_utils_get_nifti_header.py")


#' get nifiti header
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

get_nifti_header_information <- function(file){
  
  img_header <- get_nifti_header_python(file = file)
  img_shape <- unlist(img_header[[1]])
  names(img_shape) <- c("x","y","z")
  img_affine <- img_header[[2]]
  
  return(list(
    "shape" = img_shape,
    "affine" = img_affine
  ))
  
}

#' get nifiti 2D matrix
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

get_nifti_2D_matrix <- function(file,direction,layer){
  img_mat <- get_nifti_2D_layer_matrix_python(file = file,
                                              direction = direction,
                                              layer = layer)
  return(t(img_mat)[ncol(img_mat):1,])
}