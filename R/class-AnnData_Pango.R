
# class-AnnData_Pango.R

#' S4 Class
#' 
#' @exportClass AnnData_Pango_R

setClass(Class = "AnnData_Pango_R",
         slots = list(
           h5ad_path = "character",
           adata_obj = "environment"
         ))


#' initialize method
#' 
#' @noRd

setMethod(f = "initialize",
          signature = "AnnData_Pango_R",
          definition = function(.Object,
                                h5ad_path){
            
            if (length(h5ad_path) > 1) {
              
              .Object@h5ad_path <- h5ad_path
              if (is.null(names(.Object@h5ad_path))) {
                
                names(.Object@h5ad_path) <- paste0("batch_",seq(1,length(.Object@h5ad_path)))
                
              }
              
              comb_adata <- construct_harmony_anndata_pangor(.Object@h5ad_path)
              
              .Object@adata_obj <- new.env()
              .Object@adata_obj$Anndata_Pango_Py <- Anndata_Pango_Py()
              .Object@adata_obj$Anndata_Pango_Py$adata_obj <- comb_adata
              
            } else {
              
              .Object@h5ad_path <- h5ad_path
              .Object@adata_obj <- new.env()
              .Object@adata_obj$Anndata_Pango_Py <- Anndata_Pango_Py(H5ad_Path = .Object@h5ad_path)
              
            }
            
            return(.Object)
          })

#' built function
#' 
#' @export

AnnData_Pango_R <- function(h5ad_path){
  
  obj <- new(Class = "AnnData_Pango_R",
             h5ad_path = h5ad_path)
  
  return(obj)
  
}


#' SCE_Obj_Pango
#' 
#' @noRd

setGeneric(name = "SCE_Obj_Pango",
           def = function(Object){
             standardGeneric("SCE_Obj_Pango")
           })

setMethod(f = "SCE_Obj_Pango",
          signature = "AnnData_Pango_R",
          definition = function(Object){
            
            pca <- H5ad_PCA_Pango(Object)
            count_matrix <- H5ad_Counts_Matrix_Pango(Object)
            col_data <- H5ad_Obs_Pango(Object)
            
            col_data[c("array_row","array_col","pxl_row_in_fullres","pxl_col_in_fullres")] <- col_data[c("spatial_y","spatial_x","pxl_col_in_fullres","pxl_row_in_fullres")]
            
            sce_obj <- SingleCellExperiment(
              assays = list(counts = Matrix::t(count_matrix)),
              colData = col_data)
            
            reducedDim(sce_obj,"PCA") <- pca
            
            return(sce_obj)
            
          })

#' H5ad_Obs_Pango
#' 
#' @noRd
#' @export

setGeneric(name = "H5ad_Obs_Pango",
           def = function(Object){
             standardGeneric("H5ad_Obs_Pango")
           })

setMethod(f = "H5ad_Obs_Pango",
          signature = "AnnData_Pango_R",
          definition = function(Object){
            tmp_parquet <- tempfile(fileext = ".parquet")
            adata_obs <- read_parquet(Object@adata_obj$Anndata_Pango_Py$get_anndata_obs_pango_py(tmp_parquet))
            return(adata_obs)
          })

#' H5ad_Var_Pango
#' 
#' @noRd
#' @export

setGeneric(name = "H5ad_Var_Pango",
           def = function(Object){
             standardGeneric("H5ad_Var_Pango")
           })

setMethod(f = "H5ad_Var_Pango",
          signature = "AnnData_Pango_R",
          definition = function(Object){
            tmp_parquet <- tempfile(fileext = ".parquet")
            adata_var <- read_parquet(Object@adata_obj$Anndata_Pango_Py$get_anndata_var_pango_py(tmp_parquet))
            return(adata_var)
          })

#' H5ad_Var_Names_Pango
#' 
#' @noRd
#' @export

setGeneric(name = "H5ad_Var_Names_Pango",
           def = function(Object){
             standardGeneric("H5ad_Var_Names_Pango")
           })

setMethod(f = "H5ad_Var_Names_Pango",
          signature = "AnnData_Pango_R",
          definition = function(Object){
            
            adata_var_names <- Object@adata_obj$Anndata_Pango_Py$get_anndata_var_names_pango_py()
            
            return(adata_var_names)
            
          })

#' H5ad_Counts_Matrix_Pango
#' 
#' @noRd
#' @export

setGeneric(name = "H5ad_Counts_Matrix_Pango",
           def = function(Object){
             standardGeneric("H5ad_Counts_Matrix_Pango")
           })

setMethod(f = "H5ad_Counts_Matrix_Pango",
          signature = "AnnData_Pango_R",
          definition = function(Object){
            
            adata_counts <- Object@adata_obj$Anndata_Pango_Py$get_anndata_counts_pango_py()
            
            tmp_parquet <- tempfile(fileext = ".parquet")
            colnames(adata_counts) <- unlist(read_parquet(Object@adata_obj$Anndata_Pango_Py$get_anndata_gene_pango_py(tmp_parquet)))
            rownames(adata_counts) <- unlist(read_parquet(Object@adata_obj$Anndata_Pango_Py$get_anndata_barcode_pango_py(tmp_parquet)))
            
            return(adata_counts)
          })

#' H5ad_PCA_Pango
#' 
#' @noRd
#' @export

setGeneric(name = "H5ad_PCA_Pango",
           def = function(Object,PCA_Label = NULL){
             standardGeneric("H5ad_PCA_Pango")
           })

setMethod(f = "H5ad_PCA_Pango",
          signature = "AnnData_Pango_R",
          definition = function(Object,PCA_Label = NULL){
            
            tmp_parquet <- tempfile(fileext = ".parquet")
            
            if (is.null(PCA_Label)) {
              PCA_Label <- "X_pca"
            }
            
            adata_pca <- read_parquet(Object@adata_obj$Anndata_Pango_Py$get_anndata_pca_pango_py(tmp_parquet,PCA_Label)) %>%
              as.data.frame()
            
            rownames(adata_pca) <- unlist(read_parquet(Object@adata_obj$Anndata_Pango_Py$get_anndata_barcode_pango_py(tmp_parquet)))
            
            return(adata_pca)
          })

#' H5ad_Obsm_Pango
#' 
#' @noRd
#' @export

setGeneric(name = "H5ad_Obsm_Pango",
           def = function(Object,obsm_label){
             standardGeneric("H5ad_Obsm_Pango")
           })

setMethod(f = "H5ad_Obsm_Pango",
          signature = "AnnData_Pango_R",
          definition = function(Object,obsm_label){
            
            tmp_parquet <- tempfile(fileext = ".parquet")
            
            adata_obsm <- read_parquet(Object@adata_obj$Anndata_Pango_Py$get_anndata_obsm_pango_py(tmp_parquet,obsm_label)) %>%
              as.data.frame()
            
            adata_obsm$barcode <- unlist(read_parquet(Object@adata_obj$Anndata_Pango_Py$get_anndata_barcode_pango_py(tmp_parquet)))
            
            return(adata_obsm)
          })

#' H5ad_Uns_DataFrame_Pango
#' 
#' @noRd
#' @export

setGeneric(name = "H5ad_Uns_DataFrame_Pango",
           def = function(Object,uns_label){
             standardGeneric("H5ad_Uns_DataFrame_Pango")
           })

setMethod(f = "H5ad_Uns_DataFrame_Pango",
          signature = "AnnData_Pango_R",
          definition = function(Object,uns_label){
            
            tmp_parquet <- tempfile(fileext = ".parquet")
            
            adata_uns <- read_parquet(Object@adata_obj$Anndata_Pango_Py$get_anndata_uns_dataframe_pango_py(tmp_parquet,uns_label)) %>%
              as.data.frame()
            
            return(adata_uns)
          })

#' H5ad_Pearson_Residuals_Uns_Pango
#' 
#' @noRd
#' @export

setGeneric(name = "H5ad_Pearson_Residuals_Uns_Pango",
           def = function(Object){
             standardGeneric("H5ad_Pearson_Residuals_Uns_Pango")
           })

setMethod(f = "H5ad_Pearson_Residuals_Uns_Pango",
          signature = "AnnData_Pango_R",
          definition = function(Object){
            
            tmp_parquet <- tempfile(fileext = ".parquet")
            
            adata_uns_pear <- arrow::read_parquet(Object@adata_obj$Anndata_Pango_Py$get_anndata_pearson_residuals_df_pango_py(tmp_parquet)) %>%
              as.data.frame() %>%
              tibble::column_to_rownames("__index_level_0__")
            
            return(adata_uns_pear)
            
          })

#' Initialize_AnnData_Pango_R
#' 
#' @noRd
#' @export

Initialize_AnnData_Pango_R <- function(Expression_Matrix_H5,
                                       Position_Matrix){
  
  tmp_dir <- tempdir()
  
  adata_path <- Export_As_H5ad_From_10X_H5_Python(Expression_Matrix_H5 = Expression_Matrix_H5,
                                                  Position_Matrix = Position_Matrix,
                                                  File_Path = tmp_dir)
  
  adata <- AnnData_Pango_R(adata_path)
  
  return(adata)
  
}

#' Save_AnnData_Pango_R
#' 
#' @noRd
#' @export

setGeneric(name = "Save_AnnData_Pango_R",
           def = function(Object,H5ad_Path){
             standardGeneric("Save_AnnData_Pango_R")
           })

setMethod(f = "Save_AnnData_Pango_R",
          signature = "AnnData_Pango_R",
          definition = function(Object,H5ad_Path){
            
            Object@adata_obj$Anndata_Pango_Py$save_anndata_pango_py(file_path = H5ad_Path)
            
            return(Object)
            
          })

#' Quality_Control_Pango_R
#' 
#' @noRd
#' @export

setGeneric(name = "Quality_Control_Pango_R",
           def = function(Object){
             standardGeneric("Quality_Control_Pango_R")
           })

setMethod(f = "Quality_Control_Pango_R",
          signature = "AnnData_Pango_R",
          definition = function(Object){
            
            Object@adata_obj$Anndata_Pango_Py$conduct_qc_pango_py()

            return(Object)
            
          })

#' Pearson_Residuals_Pango_R
#' 
#' @noRd
#' @export

setGeneric(name = "Pearson_Residuals_Pango_R",
           def = function(Object){
             standardGeneric("Pearson_Residuals_Pango_R")
           })

setMethod(f = "Pearson_Residuals_Pango_R",
          signature = "AnnData_Pango_R",
          definition = function(Object){
            
            Object@adata_obj$Anndata_Pango_Py$conduct_pearson_residuals_pango_py()
            
            return(Object)
            
          })

#' Neighbors_Construction_Pango_R
#' 
#' @noRd
#' @export

setGeneric(name = "Neighbors_Construction_Pango_R",
           def = function(Object){
             standardGeneric("Neighbors_Construction_Pango_R")
           })

setMethod(f = "Neighbors_Construction_Pango_R",
          signature = "AnnData_Pango_R",
          definition = function(Object){
            
            Object@adata_obj$Anndata_Pango_Py$conduct_neighbors_construction_py()
            
            return(Object)
            
          })

#' Spatial_Leiden_Cluster_Pango_R
#' 
#' @noRd
#' @export

setGeneric(name = "Spatial_Leiden_Cluster_Pango_R",
           def = function(Object){
             standardGeneric("Spatial_Leiden_Cluster_Pango_R")
           })

setMethod(f = "Spatial_Leiden_Cluster_Pango_R",
          signature = "AnnData_Pango_R",
          definition = function(Object){
            
            Object@adata_obj$Anndata_Pango_Py$conduct_spatial_leiden_cluster_pango_py()
            
            return(Object)
            
          })

#' Harmony_Batch_Correction_Pango_R
#' 
#' @noRd
#' @export

setGeneric(name = "Harmony_Batch_Correction_Pango_R",
           def = function(Object){
             standardGeneric("Harmony_Batch_Correction_Pango_R")
           })

setMethod(f = "Harmony_Batch_Correction_Pango_R",
          signature = "AnnData_Pango_R",
          definition = function(Object){
            
            Object@adata_obj$Anndata_Pango_Py$conduct_harmony_batch_correction()
            
            return(Object)
            
          })

#' Spatial_Enhancement_Pango_R
#' 
#' @noRd
#' @export

setGeneric(name = "Spatial_Enhancement_Pango_R",
           def = function(Object){
             standardGeneric("Spatial_Enhancement_Pango_R")
           })

setMethod(f = "Spatial_Enhancement_Pango_R",
          signature = "AnnData_Pango_R",
          definition = function(Object){
            
            tmp_file <- tempfile(fileext = ".h5ad")
            
            adata_obj <- Object@h5ad_path %>%
              conduct_spatial_enhance() %>%
              construct_anndata_from_sce_pangor(tmp_file)
            
            Object@adata_obj$Anndata_Pango_Py$adata_obj <- adata_obj
            
            return(Object)
            
          })

#' DEG_Analysis_Pango_R
#' 
#' @noRd
#' @export

setGeneric(name = "DEG_Analysis_Pango_R",
           def = function(Object){
             standardGeneric("DEG_Analysis_Pango_R")
           })

setMethod(f = "DEG_Analysis_Pango_R",
          signature = "AnnData_Pango_R",
          definition = function(Object){
            
            Object@adata_obj$Anndata_Pango_Py$conduct_DEG_analysis_pango_py()
            
            return(Object)
            
          })

#' GSEA_Analysis_Pango_R
#' 
#' @noRd
#' @export

setGeneric(name = "GSEA_Analysis_Pango_R",
           def = function(Object){
             standardGeneric("GSEA_Analysis_Pango_R")
           })

setMethod(f = "GSEA_Analysis_Pango_R",
          signature = "AnnData_Pango_R",
          definition = function(Object){
            
            Object@adata_obj$Anndata_Pango_Py$conduct_enrich_analysis_pango_py()
            
            return(Object)
            
          })

#' ORA_Analysis_Pango_R
#' 
#' @noRd
#' @export

setGeneric(name = "ORA_Analysis_Pango_R",
           def = function(Object){
             standardGeneric("ORA_Analysis_Pango_R")
           })

setMethod(f = "ORA_Analysis_Pango_R",
          signature = "AnnData_Pango_R",
          definition = function(Object){
            
            tmp_file_go <- tempfile(fileext = "_go.parquet")
            tmp_file_wp <- tempfile(fileext = "_wp.parquet")
            tmp_file_kegg <- tempfile(fileext = "_kegg.parquet")
            
            ora_result <- ORA_analysis_pipeline(Object)
            
            write_parquet(ora_result[["GO"]],tmp_file_go)
            write_parquet(ora_result[["WP"]],tmp_file_wp)
            write_parquet(ora_result[["KEGG"]],tmp_file_kegg)
            
            Object@adata_obj$Anndata_Pango_Py$write_dataframe_to_uns_pango_py(tmp_file_go,"GO_ORA_Results")
            Object@adata_obj$Anndata_Pango_Py$write_dataframe_to_uns_pango_py(tmp_file_wp,"WP_ORA_Results")
            Object@adata_obj$Anndata_Pango_Py$write_dataframe_to_uns_pango_py(tmp_file_kegg,"KEGG_ORA_Results")
            
            return(Object)
            
          })

#' UMAP_Plot_Pango_R
#' 
#' @noRd
#' @import dplyr ggplot2
#' @export

setGeneric(name = "UMAP_Plot_Pango_R",
           def = function(Object){
             standardGeneric("UMAP_Plot_Pango_R")
           })

setMethod(f = "UMAP_Plot_Pango_R",
          signature = "AnnData_Pango_R",
          definition = function(Object){
            
            adata_obs <- H5ad_Obs_Pango(Object)
            adata_umap <- H5ad_Obsm_Pango(Object,"X_umap")
            colnames(adata_umap) <- c("umap_1","umap_2","barcode")
            plt_dataset <- left_join(adata_umap,adata_obs,by = "barcode")
            
            plt <- ggplot() +
              geom_point(data = plt_dataset,
                         mapping = aes(x = umap_1,
                                       y = umap_2,
                                       colour = as.factor(spatialleiden)),
                         size = 0.01) +
              theme_publish_pango_r()
            
            return(plt)
            
          })

#' Violin_Plot_QC_Pango_R
#' 
#' @noRd
#' @import dplyr ggplot2
#' @export

setGeneric(name = "Violin_Plot_QC_Pango_R",
           def = function(Object){
             standardGeneric("Violin_Plot_QC_Pango_R")
           })

setMethod(f = "Violin_Plot_QC_Pango_R",
          signature = "AnnData_Pango_R",
          definition = function(Object){
            
            adata_bf_obs <- H5ad_Uns_DataFrame_Pango(Object,"before_qc_obs")
            adata_bf_obs$batch <- "batch"
            adata_obs <- H5ad_Obs_Pango(Object)
            adata_obs$batch <- "batch"
            
            vio_plt_bf <- ggplot() +
              geom_violin(data = adata_bf_obs,
                          mapping = aes(x = batch,y = log1p_total_counts)) +
              theme_publish_pango_r()
            
            vio_plt <- ggplot() +
              geom_violin(data = adata_obs,
                          mapping = aes(x = batch,y = log1p_total_counts)) +
              theme_publish_pango_r()
            
            return(list(vio_plt_bf,vio_plt))
            
          })
