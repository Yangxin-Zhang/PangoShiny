
reticulate::source_python("python/python_utils_h5ad_manipulate.py")

#' construct_harmony_anndata_pangor 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

construct_harmony_anndata_pangor <- function(h5ad_ls){
  
  h5ad_comb <- list()
  for (i in 1:length(h5ad_ls)) {
    
    h5ad_comb <- append(h5ad_comb,list(c(h5ad_ls[i],names(h5ad_ls[i]))))
    
  }
  
  adata_ls <- lapply(h5ad_comb,function(h5ad_path){

    harmony_adata <- construct_anndata_for_harmony(h5ad_path[1],h5ad_path[2])

    return(harmony_adata)

  })
  
  tmp_file <- tempfile(fileext = ".h5ad")
  adata_harmony <- combine_harmony_anndata(adata_ls,tmp_file,names(h5ad_ls))
  
  return(adata_harmony)
  
}