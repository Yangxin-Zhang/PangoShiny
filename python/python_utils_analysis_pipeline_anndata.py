
# python_utils_analysis_pipeline_anndata.py

import scanpy as sc
from python_utils_general_1 import detect_outliers_onesided

def anndata_qc_pango(adata):
  
  mask = (~detect_outliers_onesided(adata.obs['log1p_n_genes_by_counts'])) & \
           (~detect_outliers_onesided(adata.obs['log1p_total_counts']))
  
  adata = adata[mask,:]
  
  return adata

def anndata_pearson_residuals_pango(adata):
  
  sc.experimental.pp.recipe_pearson_residuals(adata,n_top_genes=3000,n_comps=50)
  
  return adata

def anndata_leiden_cluster(adata):
  
  adata_hvg = adata[:,adata.var["highly_variable"]].copy()

  sc.pp.neighbors(adata_hvg, n_pcs=20, use_rep='X_pca')
  sc.tl.umap(adata_hvg)  
  sc.tl.leiden(adata_hvg, resolution=0.5,flavor="igraph")
  
  adata.obs["leiden"] = adata_hvg.obs["leiden"]
  adata.obs["umap_1"] = adata_hvg.obsm["X_umap"][:,0]
  adata.obs["umap_2"] = adata_hvg.obsm["X_umap"][:,1]

  return adata
  
  
  
  

