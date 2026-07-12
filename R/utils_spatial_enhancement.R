
reticulate::source_python("python/python_utils_h5ad_manipulate.py")

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
  
  enhanced$array_row <- round((enhanced$array_row-min(enhanced$array_row))/(1/3))
  enhanced$array_col <- round((enhanced$array_col-min(enhanced$array_col))/(1/3))
  
  enhanced <- transfer_enhanced_sce_coords(enhanced)
  
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
  sce_obs$rownames <- rownames(sce_obs)
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
  sce_pca$rownames <- rownames(sce_pca)
  write_parquet(sce_pca,tmp_file)
  
  return(tmp_file)
  
}

#' save_sce_spatial
#'
#' @description A utils function
#'
#' @importFrom arrow write_parquet
#' 
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

save_sce_spatial <- function(sce){
  
  tmp_file <- tempfile(fileext = ".parquet")
  sce_spatial <- data.frame("array_row" = sce$array_row,
                            "array_col" = sce$array_col,
                            "transfered_row" = sce$transfered_row,
                            "transfered_col" = sce$transfered_col,
                            "pxl_row_in_fullres" = sce$pxl_row_in_fullres,
                            "pxl_col_in_fullres" = sce$pxl_col_in_fullres)
  
  sce_spatial$rownames <- colnames(sce)
  
  write_parquet(sce_spatial,tmp_file)
  
  return(tmp_file)
  
}

#' construct_anndata_from_sce_pangor
#'
#' @description A utils function
#'
#' 
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

construct_anndata_from_sce_pangor <- function(sce,file_path){
  
  sce_obs <- save_sce_obs(sce)
  sce_pca <- save_sce_pca(sce)
  sce_spatial <- save_sce_spatial(sce)
  
  adata <- construct_H5ad_from_sce(sce_obs,sce_pca,sce_spatial,file_path)
  
  return(adata)
  
}

#' transfer_enhanced_sce_coords
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

transfer_enhanced_sce_coords <- function(enhanced_sce){
  
  sce_obs <- as.data.frame(colData(enhanced_sce))
  sce_obs$transfered_col <- NaN
  sce_obs$transfered_row <- sce_obs$array_row
  
  for (i in 1:length(unique(sce_obs$array_row))/3) {
    
    n_col <- c((i*3-3),(i*3-2),(i*3-1))
    
    if (i%%2 == 0) {
      
      sce_obs[sce_obs$array_row %in% n_col,"array_col"] <- sce_obs[sce_obs$array_row %in% n_col,"array_col"]-3
      
    }
    
  }
  
  col_1_seq <- seq(from = 1,
                   to = 381,
                   by = 2)
  drop_1_seq <- seq(from = 3,
                    to = 191,
                    by = 3)
  col_1_seq <- col_1_seq[-drop_1_seq]
  
  col_2_seq <- seq(from = 4,
                   to = 378,
                   by = 2)
  drop_2_seq <- seq(from = 3,
                    to = 188,
                    by = 3)
  col_2_seq <- col_2_seq[-drop_2_seq]
  col_2_seq <- c(0,col_2_seq,382)
  
  col_df <- data.frame("col_1" = col_1_seq,
                       "col_2" = col_2_seq,
                       "col_3" = col_1_seq,
                       "transfered_col" = seq(from = 0,
                                              length.out = 128,
                                              by = 1))
  
  for (i in 1:nrow(col_df)) {
    
    sce_obs[((sce_obs$array_row+3)%%3 == 0) & (sce_obs$array_col == col_df[i,"col_1"]),"transfered_col"] <- col_df[i,"transfered_col"]
    sce_obs[((sce_obs$array_row+3)%%3 == 1) & (sce_obs$array_col == col_df[i,"col_2"]),"transfered_col"] <- col_df[i,"transfered_col"]
    sce_obs[((sce_obs$array_row+3)%%3 == 2) & (sce_obs$array_col == col_df[i,"col_3"]),"transfered_col"] <- col_df[i,"transfered_col"]
    
  }
  
  enhanced_sce@colData <- S4Vectors::DataFrame(sce_obs)
  
  return(enhanced_sce)
  
}