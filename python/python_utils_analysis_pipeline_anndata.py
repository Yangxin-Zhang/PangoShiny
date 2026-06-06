
# python_utils_analysis_pipeline_anndata.py

import scanpy as sc
import squidpy as sq
import spatialleiden as sl
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
  
  sc.pp.neighbors(adata, n_pcs=20, use_rep='X_pca',random_state=2026)
  sc.tl.umap(adata)  
  sc.tl.leiden(adata, resolution=0.5,flavor="igraph")
  
  adata.obs["umap_1"] = adata.obsm["X_umap"][:,0]
  adata.obs["umap_2"] = adata.obsm["X_umap"][:,1]

  return adata

def anndata_spatial_leiden_cluster(adata):
  
  sq.gr.spatial_neighbors(adata, coord_type="generic", n_neighs=10)
  
  adata.obsp["spatial_connectivities"] = sl.distance2connectivity(adata.obsp["spatial_distances"])
  
  sl.spatialleiden(adata,layer_ratio=1.8, directed=(False, True), random_state=2026)


  
  
  
  

