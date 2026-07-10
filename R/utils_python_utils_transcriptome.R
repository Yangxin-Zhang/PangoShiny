
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
  
  adata_obs <- AnnData_Pango_R(File_Path) %>%
    H5ad_Obs_Pango()
  
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
  
  adata_var <- AnnData_Pango_R(File_Path) %>%
    H5ad_Var_Pango()
  
  return(adata_var)
}

#' get_h5ad_counts
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @import arrow
#' @noRd

get_h5ad_counts <- function(File_Path){
  
  adata_counts <- AnnData_Pango_R(File_Path) %>%
    H5ad_Counts_Matrix_Pango()

  return(adata_counts)
  
}

#' get_h5ad_pca
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @import arrow
#' @noRd

get_h5ad_pca <- function(File_Path){
  
  adata_pca <- AnnData_Pango_R(File_Path) %>%
    H5ad_PCA_Pango()
  
  return(adata_pca)
  
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

#' create_SCE_obj
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @import SingleCellExperiment
#' @importFrom Matrix t
#' @noRd

create_SCE_obj <- function(count_matrix,
                           pca_matrix,
                           obs_df,
                           File_Path){

  adata <- AnnData_Pango_R(h5ad_path = File_Path) %>%
    SCE_Obj_Pango()
  
  col_data <- obs_df[,c("barcode","spatial_y","spatial_x","pxl_col_in_fullres","pxl_row_in_fullres")]
  colnames(col_data) <- c("spot.idx","array_row","array_col","pxl_row_in_fullres","pxl_col_in_fullres")

  sce_obj <- SingleCellExperiment(
    assays = list(counts = Matrix::t(count_matrix)),
    colData = col_data)
  # 
  # pca_matrix <- as.data.frame(pca_matrix)
  # rownames(pca_matrix) <- col_data$cell_id
  # reducedDim(sce_obj, "PCA") <- pca_matrix
  
  # sce_obj <- SingleCellExperiment(assays = list(counts = Matrix::t(count_matrix)))
  return(sce_obj)
  
}







