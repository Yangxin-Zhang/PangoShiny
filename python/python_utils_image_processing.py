
# python_utils_image_processing.py

import numpy as np
from python_utils_general_1 import map_vector_to_greyscale

def build_greyscale_matrix_from_spatial_matrix(sp_mt):
  
  mat_size = max(max(sp_mt["x"]),max(sp_mt["y"]))*1.1
  mat_size = mat_size.astype("int64")
  
  pix_mat = np.zeros((1,mat_size, mat_size), dtype=np.uint8)
  
  for index, row in sp_mt.iterrows():
    pix_mat[0,row["x"], row["y"]] = map_vector_to_greyscale(row["feature_value"])
  
  return pix_mat

