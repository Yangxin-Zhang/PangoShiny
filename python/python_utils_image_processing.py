
# python_utils_image_processing.py

import numpy as np
import tifffile
from skimage import color, exposure, transform
from skimage.filters import gaussian
from python_utils_general_1 import map_vector_to_greyscale

def build_greyscale_matrix_from_spatial_matrix(sp_mt):
  
  mat_size = max(max(sp_mt["x"]),max(sp_mt["y"]))*1.1
  mat_size = mat_size.astype("int64")
  
  pix_mat = np.zeros((1,mat_size, mat_size), dtype=np.uint8)
  
  for index, row in sp_mt.iterrows():
    pix_mat[0,row["x"], row["y"]] = map_vector_to_greyscale(row["feature_value"])
  
  return pix_mat

def transfer_rgb_to_greyscale(img_path):
  
  tif_plot = tifffile.imread(img_path)
  grey_plot = color.rgb2gray(tif_plot)
  grey_plot = exposure.rescale_intensity(grey_plot,out_range=(0,255)).astype(np.uint8)
  
  return grey_plot

def downsample_greyscale(greyscale_img,width,height):
  
  img_norm = exposure.rescale_intensity(greyscale_img, out_range='float')
  img_norm = gaussian(img_norm, sigma=1, channel_axis=-1, preserve_range=True)
  downsampled = transform.resize(img_norm, (width,height), preserve_range=True)
  downsampled = exposure.rescale_intensity(downsampled,out_range=(0,255)).astype(np.uint8)
  
  return downsampled


