
# python_utils_general_1.py

import numpy as np
import os
import pandas as pd
from sklearn.preprocessing import  MinMaxScaler

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
    
    adata.obs = transfer_dataframe_dtype(adata.obs)
    adata.var = transfer_dataframe_dtype(adata.var)
    
    try:
      adata.uns["pearson_residuals_normalization"]["pearson_residuals_df"].index = adata.uns["pearson_residuals_normalization"]["pearson_residuals_df"].index.astype(object)
    except:
      pass
    
    for key,value in adata.uns.items():
      if isinstance(value,pd.DataFrame):
        adata.uns[key] = transfer_dataframe_dtype(value)
    
    if os.path.exists(file_na):
      os.remove(file_na)
    
    adata.write(file_na,convert_strings_to_categoricals=False)
    
def tansfer_visium_10x_spatial_matrix(sp_mt):
  
  try:
    for i in range(max(sp_mt["array_row"])+1):
      r = sp_mt["array_row"] == i
      r = r.values
      if i % 2 == 0:
          sp_mt.loc[r, "transfered_col"] = sp_mt.loc[r, "array_col"] / 2 + 1
      elif i % 2 != 0:
          sp_mt.loc[r, "transfered_col"] = (sp_mt.loc[r, "array_col"] + 1) / 2
          
    sp_mt["transfered_row"] = sp_mt["array_row"]
    sp_mt["transfered_col"] = sp_mt["transfered_col"] - 1
    sp_mt["transfered_row"] = sp_mt["transfered_row"].astype("int64")
    sp_mt["transfered_col"] = sp_mt["transfered_col"].astype("int64")
    
  except:
    for i in range(max(sp_mt["row"])+1):
      r = sp_mt["row"] == i
      r = r.values
      if i % 2 == 0:
          sp_mt.loc[r, "transfered_col"] = sp_mt.loc[r, "col"] / 2 + 1
      elif i % 2 != 0:
          sp_mt.loc[r, "transfered_col"] = (sp_mt.loc[r, "col"] + 1) / 2
          
    sp_mt["transfered_row"] = sp_mt["row"]
    sp_mt["transfered_col"] = sp_mt["transfered_col"] - 1
    sp_mt["transfered_row"] = sp_mt["transfered_row"].astype("int64")
    sp_mt["transfered_col"] = sp_mt["transfered_col"].astype("int64")

  return sp_mt

def map_vector_to_greyscale(data):
  
  return MinMaxScaler(feature_range=(0,255)).fit_transform(data).astype("int64")

def transfer_dataframe_dtype(dataframe):
  
  for col in dataframe.columns[dataframe.isna().any()].tolist():
    if pd.api.types.is_string_dtype(dataframe[col]):
      dataframe[col] = dataframe[col].fillna("None")
  
  try:
    if not pd.api.types.is_any_real_numeric_dtype(dataframe.index):
      dataframe.index = dataframe.index.astype(object)
  except:
    pass
  
  dataframe.index.name = None
  
  for col in dataframe.select_dtypes(exclude=['number',"bool","category"]).columns:
        dataframe[col] = dataframe[col].astype(object)

  for col in dataframe.select_dtypes(include=["category"]).columns:
        if dataframe[col].cat.categories.dtype == "string":
          dataframe[col] = dataframe[col].astype(object)
  
  return dataframe
          
