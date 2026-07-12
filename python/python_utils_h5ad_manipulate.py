
# python_utils_h5ad_manipulate.py

import pandas as pd
import scanpy as sc
import os
import warnings
import anndata
import pyarrow.parquet as pq
import anndata as ad
import squidpy as sq
import spatialleiden as sl

# import rpy2.robjects as robjects

from python_utils_general_1 import save_anndata_as_h5ad,tansfer_visium_10x_spatial_matrix

def Export_As_H5ad_From_10X_H5_Python(Expression_Matrix_H5,
                                      Position_Matrix,
                                      File_Path):
                                 
    warnings.filterwarnings("ignore", category=UserWarning)
    
    adata = sc.read_10x_h5(Expression_Matrix_H5)
    adata.var_names_make_unique()

    pos_mat_ext = os.path.splitext(Position_Matrix)[1].lower()
    
    if pos_mat_ext == ".csv":
      px = pd.read_csv(Position_Matrix, index_col=0)
      px = tansfer_visium_10x_spatial_matrix(sp_mt = px)
      px = px.loc[adata.obs_names]
      
      try:
        adata.obsm['spatial'] = px[["array_row", "array_col"]].to_numpy()
        adata.obsm['pxl_in_fullres'] = px[["pxl_row_in_fullres","pxl_col_in_fullres"]].to_numpy()
      except:
        adata.obsm['spatial'] = px[["row", "col"]].to_numpy()
      
      adata.obsm['transfered_spatial'] = px[["transfered_row", "transfered_col"]].to_numpy()
      adata.obs["transfered_spatial_x"] = adata.obsm['transfered_spatial'][:,0]
      adata.obs["transfered_spatial_y"] = adata.obsm['transfered_spatial'][:,1]
    
    elif pos_mat_ext == ".parquet":       
      px = pd.read_parquet(Position_Matrix)
      px.index = px["barcode"]
      px = px.loc[adata.obs_names]

      try:
        adata.obsm['spatial'] = px[px["barcode"].isin(adata.obs_names)][["array_row", "array_col"]].to_numpy()
        adata.obsm['pxl_in_fullres'] = px[["pxl_row_in_fullres","pxl_col_in_fullres"]].to_numpy()
      except:
        adata.obsm['spatial'] = px[px["barcode"].isin(adata.obs_names)][["row", "col"]].to_numpy()
    
    adata.obs["spatial_x"] = adata.obsm['spatial'][:,0]
    adata.obs["spatial_y"] = adata.obsm['spatial'][:,1]

    adata.obs["pxl_row_in_fullres"] = adata.obsm["pxl_in_fullres"][:,0]
    adata.obs["pxl_col_in_fullres"] = adata.obsm["pxl_in_fullres"][:,1]
    adata.var['mt'] = adata.var_names.str.startswith('mt-')
    adata.var["rp"] = adata.var_names.str.startswith(("Rpl","Rps"))

    sc.pp.calculate_qc_metrics(adata = adata,
                               qc_vars=["mt","rp"],
                               percent_top=None,
                               log1p=True,
                               inplace=True)
                               
    adata.layers["counts"] = adata.X.copy()

    H5ad_File_Name = Expression_Matrix_H5.split("/")[-1].split(".")[0]+"."+"h5ad"

    save_anndata_as_h5ad(adata = adata,file_na = (File_Path+"/"+H5ad_File_Name))
    
    return (File_Path+"/"+H5ad_File_Name)

def construct_H5ad_from_sce(sce_obs,sce_pca,sce_spatial,file_path):
  
  adata_obs = pq.read_table(sce_obs).to_pandas().set_index("rownames")
  adata_pca = pq.read_table(sce_pca).to_pandas().set_index("rownames")
  adata_spatial = pq.read_table(sce_spatial).to_pandas().set_index("rownames")
  
  adata = ad.AnnData(
    obs = adata_obs,
    obsm = {
      "X_pca": adata_pca.values,
      "spatial": adata_spatial[["transfered_row","transfered_col"]].to_numpy(),
      "pxl_in_fullres": adata_spatial[["pxl_row_in_fullres","pxl_col_in_fullres"]].to_numpy()
    }
  )
  
  sc.pp.neighbors(adata, n_pcs=15, use_rep='X_pca')
  sq.gr.spatial_neighbors(adata, coord_type="generic", n_neighs=4,n_rings = 2,spatial_key="spatial")
  sl.spatialleiden(adata,layer_ratio=1.8, directed=(False, True), random_state=2026)
  adata.obs["spatialleiden"] = adata.obs["spatialleiden"].astype(int)
  
  # save_anndata_as_h5ad(adata = adata,file_na = file_path)

  return adata

def construct_anndata_for_harmony(h5ad_path,batch):
  
  adata = sc.read_h5ad(h5ad_path)
  
  adata_harmony = ad.AnnData(
    obs = pd.DataFrame(
      {
        "spatialleiden": [f"{x}_{batch}" for x in list(adata.obs["spatialleiden"].values)],
        "batch": batch,
        "array_row": adata.obsm["spatial"][:,0],
        "array_col": adata.obsm["spatial"][:,1]
      },
      index = adata.obs.index + "_" + batch
    ),
    obsm = {
      "X_pca": adata.obsm["X_pca"],
      "spatial": adata.obsm['spatial'],
      "pxl_in_fullres": adata.obsm['pxl_in_fullres']
    }
  )
  
  # adata_harmony.obsp["connectivities"] = adata.obsp["connectivities"].copy()
  # adata_harmony.obsp["spatial_connectivities"] = adata.obsp["spatial_connectivities"].copy()

  return adata_harmony

def combine_harmony_anndata(adata_ls,file_path,batch):
  
  comb_adata = ad.concat(adata_ls,label="batch",keys=batch)
  
  # save_anndata_as_h5ad(adata = comb_adata,file_na = file_path)

  return comb_adata
