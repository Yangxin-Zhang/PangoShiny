#' conduct_spatial_enhance
#'
#' @description A utils function
#'
#' @import BayesSpace
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

conduct_spatial_enhance <- function(H5ad_Path){
  
  set.seed(2026)
  
  sce <- AnnData_Pango_R(H5ad_Path) %>%
    SCE_Obj_Pango()
  
  dist <- BayesSpace:::.compute_interspot_distances(sce)
  
  if (dist[["xdist"]] > dist[["ydist"]]) {
    
    sce <- exchange_direction(sce)
    
  }
  
  sce <- spatialCluster(sce,
                        q = length(unique(sce$spatialleiden)),
                        d = 15,
                        nrep = 1000,
                        burn.in = 100)
  
  enhanced <- spatialEnhance(
    sce,
    q = length(unique(sce$spatialleiden)),
    use.dimred = "PCA",
    platform = "Visium",
    nrep = 1000,
    burn.in = 100,
    gamma = 2,
    verbose = TRUE
  )
  
  return(enhanced)
  
}

#' exchange_direction
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

exchange_direction <- function(sce){
  
  tool_data <- sce$pxl_row_in_fullres
  sce$pxl_row_in_fullres <- sce$pxl_col_in_fullres
  sce$pxl_col_in_fullres <- tool_data
  
  tool_data <- sce$array_row
  sce$array_row <- sce$array_col
  sce$array_col <- tool_data
  
  return(sce)
  
}

#' save_sce_obs
#'
#' @description A utils function
#'
#' @importFrom arrow write_parquet
#' 
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

save_sce_obs <- function(sce){
  
  tmp_file <- tempfile(fileext = ".parquet")
  sce_obs <- as.data.frame(colData(sce))
  write_parquet(sce_obs,tmp_file)
  
  return(tmp_file)
  
}

#' save_sce_pca
#'
#' @description A utils function
#'
#' @importFrom arrow write_parquet
#' 
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

save_sce_pca <- function(sce){
  
  tmp_file <- tempfile(fileext = ".parquet")
  sce_pca <- as.data.frame(reducedDim(sce))
  write_parquet(sce_pca,tmp_file)
  
  return(tmp_file)
  
}

