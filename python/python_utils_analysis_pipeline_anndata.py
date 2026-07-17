
# python_utils_analysis_pipeline_anndata.py

import scanpy as sc
import squidpy as sq
import spatialleiden as sl
import harmonypy as hm
import pandas as pd
import gseapy as gp
from python_utils_general_1 import detect_outliers_onesided,transfer_dataframe_dtype

def anndata_qc_pango(adata,plotting_dataset_path=None):
  
  if plotting_dataset_path is not None:
    raw_path = f"{plotting_dataset_path}/raw_obs.parquet"
    adata.obs.to_parquet(raw_path)
  
  adata.uns["before_qc_obs"] = transfer_dataframe_dtype(adata.obs.copy())
  
  sc.pp.filter_cells(adata, min_genes=1)
  sc.pp.filter_genes(adata, min_counts=1)
  
  mask = (~detect_outliers_onesided(adata.obs['log1p_n_genes_by_counts'])) & \
             (~detect_outliers_onesided(adata.obs['log1p_total_counts']))
  
  adata = adata[mask,:].copy()
  
  adata_hvg = sc.experimental.pp.highly_variable_genes(adata,flavor="pearson_residuals", n_top_genes=3000,inplace = False)
  adata_hvg = adata[:,adata_hvg["highly_variable"]].copy()
  
  sc.pp.filter_genes(adata_hvg, min_counts=1)
  sc.pp.filter_cells(adata_hvg, min_genes=1)
  
  adata = adata[adata_hvg.obs_names,:].copy()
  
  sc.pp.calculate_qc_metrics(adata = adata,
                             percent_top=None,
                             log1p=True,
                             inplace=True)
                               
  return adata

def anndata_pearson_residuals_pango(adata):
  
  sc.experimental.pp.recipe_pearson_residuals(adata,n_top_genes=3000,n_comps=50)
  
  adata.uns["pearson_residuals_normalization"]["pearson_residuals_df"].index = adata.uns["pearson_residuals_normalization"]["pearson_residuals_df"].index.astype(object)
  
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

  sc.tl.umap(adata)  
  sl.spatialleiden(adata,layer_ratio=1.8, directed=(False, True), random_state=2026)

  return adata

def anndata_enrich_analysis(adata):
  
  spt_led = adata.obs["spatialleiden"].unique()
  
  enrich_dic = {}
  for i in spt_led:
    
    ad_obj = adata[adata.obs["spatialleiden"] == i].copy()
    sc.pp.filter_genes(ad_obj,min_cells=3)
    
    marker_df = ad_obj.uns["marker_genes"]
    marker_df = marker_df[((marker_df["group"].astype(str) == str(i))&(marker_df["logfoldchanges"] >= 1))]
    
    gene_set_enrich = gp.enrichr(gene_list=marker_df["names"],
                                 gene_sets=["GO_Biological_Process_2026","GO_Cellular_Component_2026","GO_Molecular_Function_2026","KEGG_2026","WikiPathways_2024_Mouse"],
                                 organism='mouse',
                                 background=list(ad_obj.var_names.values))
                                 
    gene_set_enrich = gene_set_enrich.results
    gene_set_enrich["spatialleiden"] = i
    enrich_dic[f"{i}"] = gene_set_enrich
  
  combined_enrich_dic = pd.concat(enrich_dic.values(), ignore_index=True)
  
  for col in combined_enrich_dic.select_dtypes(exclude=['number',"bool","category"]).columns:
    combined_enrich_dic[col] = combined_enrich_dic[col].astype(object)
  
  for col in combined_enrich_dic.select_dtypes(include=["category"]).columns:
        if combined_enrich_dic[col].cat.categories.dtype == "string":
          combined_enrich_dic[col] = combined_enrich_dic[col].astype(object)
          
  adata.uns["gene_set_enrichment"] = combined_enrich_dic
  
  return adata
  
def anndata_DEG_analysis(adata):
  
  adata.X = adata.layers["counts"]
  adata.obs["spatialleiden"] = adata.obs["spatialleiden"].astype("category")
  sc.pp.normalize_total(adata,target_sum=1e4)
  sc.pp.log1p(adata)
  sc.tl.rank_genes_groups(adata,groupby="spatialleiden",method="wilcoxon")

  marker_df = sc.get.rank_genes_groups_df(adata,group=None)
  
  for col in marker_df.select_dtypes(exclude=['number',"bool","category"]).columns:
    marker_df[col] = marker_df[col].astype(object)

  for col in marker_df.select_dtypes(include=["category"]).columns:
        if marker_df[col].cat.categories.dtype == "string":
          marker_df[col] = marker_df[col].astype(object)
          
  adata.uns["marker_genes"] = marker_df

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
  
  
  
  

