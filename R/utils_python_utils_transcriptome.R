
reticulate::source_python("python/python_utils_h5ad_manipulate.py")

#' export_as_h5ad_from_10xh5 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @import reticulate
#' @noRd

export_as_h5ad_from_10xh5 <- function(Expression_Matrix_H5,
                                      Position_Matrix,
                                      File_Path){
  
  Export_As_H5ad_From_10X_H5_Python(Expression_Matrix_H5 = Expression_Matrix_H5,
                                    Position_Matrix = Position_Matrix,
                                    File_Path = File_Path)
  cat("File Path: ",File_Path)
}

#' get_h5ad_obs
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @import arrow
#' @noRd

get_h5ad_obs <- function(File_Path){
  
  tmp_parquet <- tempfile(fileext = ".parquet")
  
  adata <- Read_H5ad_Python(File_Path = File_Path)
  
  Get_H5ad_obs(adata = adata,
               tmp_path = tmp_parquet)
  
  adata_obs <- read_parquet(tmp_parquet)
  
  return(adata_obs)
}




