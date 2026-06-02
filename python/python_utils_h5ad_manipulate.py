
# python_utils_h5ad_manipulate.py

import pandas as pd
import scanpy as sc
import os
import warnings
import anndata
import arrow

def Export_As_H5ad_From_10X_H5_Python(Expression_Matrix_H5,
                                      Position_Matrix,
                                      File_Path):
                                 
    warnings.filterwarnings("ignore", category=UserWarning)
    
    adata = sc.read_10x_h5(Expression_Matrix_H5)
    adata.var_names_make_unique()

    adata.layers["counts"] = adata.X.copy()
    
    pos_mat_ext = os.path.splitext(Position_Matrix)[1].lower()
    
    if pos_mat_ext == ".csv":
        px = pd.read_csv(Position_Matrix, index_col=0)
        px = px.loc[adata.obs_names]
        try:
          adata.obsm['spatial'] = px[["array_row", "array_col"]].to_numpy()
        except:
          adata.obsm['spatial'] = px[["row", "col"]].to_numpy()
    elif pos_mat_ext == ".parquet":       
      px = pd.read_parquet(Position_Matrix)
      try:
        adata.obsm['spatial'] = px[px["barcode"].isin(adata.obs_names)][["array_row", "array_col"]].to_numpy()
      except:
        adata.obsm['spatial'] = px[px["barcode"].isin(adata.obs_names)][["row", "col"]].to_numpy()

    adata.var['mt'] = adata.var_names.str.startswith('mt-')
    adata.var["rp"] = adata.var_names.str.startswith(("Rpl","Rps"))

    sc.pp.calculate_qc_metrics(adata = adata,
                               qc_vars=["mt","rp"],
                               percent_top=None,
                               log1p=True,
                               inplace=True)

    H5ad_File_Name = Expression_Matrix_H5.split("/")[-1].split(".")[0]+"."+"h5ad"

    adata.obs_names = adata.obs_names.astype(object)
    adata.var_names = adata.var_names.astype(object)

    for col in adata.var.select_dtypes(include=['category',"str","string"]).columns:
        adata.var[col] = adata.var[col].astype(object)

    for col in adata.obs.select_dtypes(include=['category',"str","string"]).columns:
        adata.obs[col] = adata.obs[col].astype(object)

    adata.write((File_Path+"/"+H5ad_File_Name))

def Read_H5ad_Python(File_Path):
  
  return sc.read_h5ad(File_Path,backed = "r")

def Get_H5ad_obs(adata,tmp_path):
  adata_obs = adata.obs.copy()
  adata_obs = adata_obs.reset_index().rename(columns={'index': 'barcode'})
  adata_obs.to_parquet(tmp_path)

