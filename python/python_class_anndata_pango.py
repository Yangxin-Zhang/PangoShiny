
# python_class_anndata_pango.py

import scanpy as sc
import pandas as pd

from python_utils_general_1 import save_anndata_as_h5ad
from python_utils_analysis_pipeline_anndata import anndata_leiden_cluster,anndata_qc_pango,anndata_pearson_residuals_pango

class Anndata_Pango:
    def __init__(self,H5ad_Path):
        self.h5ad_path = H5ad_Path
        self.adata_obj = sc.read_h5ad(H5ad_Path,backed="r")

    def get_anndata_obs_pango_py(self,tmp_path):
      
      adata_obs = self.adata_obj.obs.copy()
      adata_obs = adata_obs.reset_index().rename(columns={'index': 'barcode'})
      adata_obs.to_parquet(tmp_path,index = False)
      
      return tmp_path
    
    def get_anndata_var_pango_py(self,tmp_path):
      
      adata_var = self.adata_obj.var.copy()
      adata_var = adata_var.reset_index().rename(columns={'index': 'barcode'})
      adata_var.to_parquet(tmp_path,index = False)
      
      return tmp_path
    
    def get_anndata_counts_pango_py(self):
      
      return self.adata_obj.layers["counts"].copy()
    
    def get_anndata_barcode_pango_py(self,tmp_path):
      
      adata_obs_names = self.adata_obj.obs_names.to_numpy()
      adata_obs_names = pd.DataFrame(adata_obs_names,columns = ["obs_names"])
      adata_obs_names.to_parquet(tmp_path,index = False)

      return tmp_path
    
    def get_anndata_gene_pango_py(self,tmp_path):
      
      adata_var_names = self.adata_obj.var_names.to_numpy()
      adata_var_names = pd.DataFrame(adata_var_names,columns = ["var_names"])
      adata_var_names.to_parquet(tmp_path,index = False)

      return tmp_path
    
    def get_anndata_pca_pango_py(self,tmp_path):
      
      adata_pca = self.adata_obj.obsm["X_pca"]
      adata_pca = pd.DataFrame(adata_pca)
      adata_pca.to_parquet(tmp_path,index = False)

      return tmp_path
    
    def conduct_qc_pango_py(self):
      
      self.adata_obj = anndata_qc_pango(adata = self.adata_obj.to_memory())

      return self
    
    def conduct_pearson_residuals_pango_py(self):
      
      self.adata_obj = anndata_pearson_residuals_pango(adata = self.adata_obj.to_memory())
      
      return self
    
    def conduct_leiden_cluster_pango_py(self):
      
      self.adata_obj = anndata_leiden_cluster(adata = self.adata_obj.to_memory())
      
      return self
    
    def save_anndata_pango_py(self,file_path):
      
      save_anndata_as_h5ad(adata = self.adata_obj.to_memory(),file_na = file_path)
      
