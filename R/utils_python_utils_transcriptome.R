
reticulate::source_python("python/python_utils_h5ad_manipulate.py")
reticulate::source_python("python/python_class_anndata_pango.py")

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
  
  adata <- Anndata_Pango(H5ad_Path = File_Path)
  
  adata_obs <- read_parquet(adata$get_anndata_obs_pango_py(tmp_parquet))
  
  return(adata_obs)
}

#' get_h5ad_var
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @import arrow
#' @noRd

get_h5ad_var <- function(File_Path){
  
  tmp_parquet <- tempfile(fileext = ".parquet")
  
  adata <- Anndata_Pango(H5ad_Path = File_Path)
  
  adata_obs <- read_parquet(adata$get_anndata_var_pango_py(tmp_parquet))
  
  return(adata_obs)
}

#' conduct_analysis_pipeline
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

conduct_analysis_pipeline_pango <- function(File_Path,Store_Path = NULL){
  
  adata <- Anndata_Pango(H5ad_Path = File_Path)
  adata <- adata$conduct_qc_pango_py()$conduct_pearson_residuals_pango_py()$conduct_leiden_cluster_pango_py()
  
  if (is.null(Store_Path)) {
    Store_Path <- File_Path
  }
  
  adata$save_anndata_pango_py(file_path = Store_Path)
  
  return(Store_Path)
  
}

