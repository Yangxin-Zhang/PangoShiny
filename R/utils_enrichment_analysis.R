#' ORA_analysis_pipeline 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

ORA_analysis_pipeline <- function(adata_r_obj){
  
  gene_marker <- H5ad_Uns_DataFrame_Pango(adata_r_obj,"marker_genes")
  adata_var_names <- H5ad_Var_Names_Pango(adata_r_obj)
  adata_obs <- H5ad_Obs_Pango(adata_r_obj)
  
  gene_marker$group <- gene_marker$group %>%
    unlist() %>%
    as.character()
  
  ncluster <- adata_obs["spatialleiden"] %>%
    unlist() %>%
    as.character() %>%
    as.factor() %>%
    unique()
  
  bk_genes <- bitr(adata_var_names,
                   fromType = "SYMBOL",
                   toType = "ENTREZID",
                   OrgDb = org.Mm.eg.db)
  
  go_res_ls <- vector("list",length = length(ncluster))
  names(go_res_ls) <- ncluster
  
  wp_res_ls <- vector("list",length = length(ncluster))
  names(wp_res_ls) <- ncluster
  
  kegg_res_ls <- vector("list",length = length(ncluster))
  names(kegg_res_ls) <- ncluster
  
  for (i in ncluster) {
    
    ge_set <- gene_marker[(gene_marker$group==i)&(gene_marker$logfoldchanges>1)&(gene_marker$pvals_adj <= 0.05),]$names %>%
      unique() %>%
      unlist() %>%
      bitr(fromType = "SYMBOL",
           toType = "ENTREZID",
           OrgDb = org.Mm.eg.db)
    
    if (length(ge_set) == 0) {
      
      next
      
    }
    
    go_re <- go_enrichment_pango(ge_set$ENTREZID,bk_genes$ENTREZID,i)
    
    go_res_ls[i] <- list(go_re)
    
    wp_re <- wikipath_enrichment_pango(ge_set$ENTREZID,bk_genes$ENTREZID,i)

    wp_res_ls[i] <- list(wp_re)
    
    kegg_re <- kegg_enrichment_pango(ge_set$ENTREZID,bk_genes$ENTREZID,i)

    kegg_res_ls[i] <- list(kegg_re)
    
  }
  
  go_res_ls <- bind_rows(go_res_ls)
  wp_res_ls <- bind_rows(wp_res_ls)
  kegg_res_ls <- bind_rows(kegg_res_ls)
  
  return(list("GO" = go_res_ls,
              "WP" = wp_res_ls,
              "KEGG" = kegg_res_ls))
  
}

#'  go_enrichment_pango
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

go_enrichment_pango <- function(gene_set,background_set,group){
  
  go_re <- enrichGO(gene = gene_set,
                    OrgDb = org.Mm.eg.db,
                    keyType = "ENTREZID",
                    ont = "ALL",
                    pvalueCutoff = 0.05,
                    universe = background_set)
  
  if (nrow(go_re@result) == 0) {
    
    go_re <- as.data.frame(go_re)
    
  } else {
    
    go_re <- go_re %>%
      clusterProfiler::simplify() %>%
      as.data.frame() 
    
  }
  
  if (nrow(go_re) == 0) {
    go_re$group <- character()
  } else {
    go_re$group <- group
  }
  
  return(go_re)
  
}

#'  kegg_enrichment_pango
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

kegg_enrichment_pango <- function(gene_set,background_set,group){
  
  kegg_re <- enrichKEGG(gene = gene_set,
                        organism = "mmu",
                        universe = background_set) %>%
    as.data.frame()
  
  if (nrow(kegg_re) == 0) {
    kegg_re$group <- character()
  } else {
    kegg_re$group <- group
  }
  
  return(kegg_re)
  
}

#'  wikipath_enrichment_pango
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

wikipath_enrichment_pango <- function(gene_set,background_set,group){
  
  wp_re <- enrichWP(gene = gene_set,
                    organism = "Mus musculus",
                    universe = background_set) %>%
    as.data.frame()
  
  if (nrow(wp_re) == 0) {
    wp_re$group <- character()
  } else {
    wp_re$group <- group
  }
  
  return(wp_re)
  
}