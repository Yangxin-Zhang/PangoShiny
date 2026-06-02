
# python_utils_general_1.py

import numpy as np
import os

def detect_outliers_onesided(data, side='both', threshold=3.5):
    median = np.median(data)
    mad = np.median(np.abs(data - median)) * 1.4826
    if mad == 0:
        return np.zeros_like(data, dtype=bool)
    modified_z = 0.6745 * (data - median) / mad
    if side == 'high':
        return modified_z > threshold
    elif side == 'low':
        return modified_z < -threshold
    elif side == "both":
        return np.abs(modified_z) > threshold
      
def save_anndata_as_h5ad(adata,file_na):
  
    adata.obs_names = adata.obs_names.astype(object)
    adata.var_names = adata.var_names.astype(object)

    for col in adata.var.select_dtypes(include=['category',"str","string"]).columns:
        adata.var[col] = adata.var[col].astype(object)

    for col in adata.obs.select_dtypes(include=['category',"str","string"]).columns:
        adata.obs[col] = adata.obs[col].astype(object)
    
    try:
      adata.uns["pearson_residuals_normalization"]["pearson_residuals_df"].index = \
      adata.uns["pearson_residuals_normalization"]["pearson_residuals_df"].index.astype(object)
    except:
      pass
    
    if os.path.exists(file_na):
      os.remove(file_na)
    
    adata.write(file_na)
  
