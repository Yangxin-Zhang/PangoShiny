
# python_utils_analysis_pipeline_anndata.py

import scanpy as sc
import squidpy as sq
import spatialleiden as sl
import harmonypy as hm
import pandas as pd
from python_utils_general_1 import detect_outliers_onesided

def anndata_qc_pango(adata):
  
  sc.pp.filter_cells(adata, min_genes=1)
  sc.pp.filter_genes(adata, min_counts=1)
  
  mask = (~detect_outliers_onesided(adata.obs['log1p_n_genes_by_counts'])) & \
           (~detect_outliers_onesided(adata.obs['log1p_total_counts']))
  
  adata = adata[mask,:]
  
  adata_hvg = sc.experimental.pp.highly_variable_genes(adata,flavor="pearson_residuals", n_top_genes=3000,inplace = False)
  adata_hvg = adata[:,adata_hvg["highly_variable"]].copy()
  
  sc.pp.filter_genes(adata_hvg, min_counts=1)
  sc.pp.filter_cells(adata_hvg, min_genes=1)
  
  adata = adata[adata_hvg.obs_names,:].copy()
  
  return adata

def anndata_pearson_residuals_pango(adata):
  
  sc.experimental.pp.recipe_pearson_residuals(adata,n_top_genes=3000,n_comps=50)
  
  return adata

def neighbors_construction(adata):
  
  sc.pp.neighbors(adata, n_pcs=20, use_rep='X_pca',random_state=2026)
  sq.gr.spatial_neighbors(adata, coord_type="generic", n_neighs=4,n_rings = 2)

  return adata

def anndata_leiden_cluster(adata):
  
  sc.tl.umap(adata)  
  sc.tl.leiden(adata, resolution=0.5,flavor="igraph")
  
  adata.obs["umap_1"] = adata.obsm["X_umap"][:,0]
  adata.obs["umap_2"] = adata.obsm["X_umap"][:,1]

  return adata

def anndata_spatial_leiden_cluster(adata):

  sl.spatialleiden(adata,layer_ratio=1.8, directed=(False, True), random_state=2026)
  adata.obs["spatialleiden"] = adata.obs["spatialleiden"].astype(int)

  return adata

def anndata_harmony_batch_correction(adata):
  
  adata.obsm["X_pca_harmony"] = hm.run_harmony(adata.obsm["X_pca"],adata.obs,"batch").Z_corr
  
  sc.pp.neighbors(adata, n_pcs=15, use_rep='X_pca_harmony',key_added="neighbors_harmony")
  sc.tl.umap(adata,neighbors_key="neighbors_harmony",key_added="X_umap_harmony")
  sc.tl.leiden(adata, resolution=0.5,flavor="igraph",key_added="leiden_harmony",neighbors_key="neighbors_harmony")
  
  return adata

def establish_contrast_cluster(adata,aim_cluster,contrast_batch,cluster_symbol="spatialleiden",interval_cluster="leiden_harmony"):
  
  harmony_clusters = adata.obs[adata.obs[cluster_symbol].isin(aim_cluster)][interval_cluster].unique()
  
  sum_leiden = {}
  for j in range(len(contrast_batch)):
    
    sm_le = []
    for i in range(len(harmony_clusters)):
      sm_le.append(((adata.obs[interval_cluster] == harmony_clusters[i])&(adata.obs[cluster_symbol].isin(aim_cluster))&(adata.obs["batch"] == contrast_batch[j])).sum())
    
    sum_leiden[contrast_batch[j]] = sm_le
  
  sum_df = pd.DataFrame(
    sum_leiden,
    index = harmony_clusters
  )

  return sum_df
  
  
  
  

