
# python_class_anndata_pango.py

import scanpy as sc

class Anndata_Pango:
    def __init__(self,H5ad_Path):
        self.h5ad_path = H5ad_Path
        self.adata_obj = sc.read_h5ad(H5ad_Path,backed="r")

    def Get_Anndata_obs(self,tmp_path):
      adata_obs = self.adata_obj.obs.copy()
      adata_obs = adata_obs.reset_index().rename(columns={'index': 'barcode'})
      adata_obs.to_parquet(tmp_path)
      return tmp_path
