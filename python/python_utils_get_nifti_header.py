
# python_utils_get_nifti_header.py

import nibabel as nib
import numpy as np

def get_nifti_header_python(file):
  img = nib.load(file)
  return(img.shape,img.affine)

def get_nifti_2D_layer_matrix_python(file,direction,layer):
  img = nib.load(file)
  
  if direction == "x":
    return(img.dataobj[layer,:,:])
  elif direction == "y":
    return(img.dataobj[:,layer,:])
  elif direction == "z":
    return(img.dataobj[:,:,layer])
