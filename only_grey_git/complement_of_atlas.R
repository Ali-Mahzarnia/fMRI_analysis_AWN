
library(readxl)
library(dplyr)
library(magrittr) 

path_new_atlas_grey = '/Users/ali/Desktop/Jul23/fmri_jayvik/codes/only_gray/new_atlas_grey_no_csf.csv'
new_atlas_grey = read.csv(path_new_atlas_grey, header = T)


atlas_path= '/Users/ali/Desktop/Jul23/fmri_jayvik/codes/only_gray/CHASSSYMM3AtlasLegends.xlsx'
full_atlas = read_xlsx(atlas_path)


complement_index = setdiff( full_atlas$index2 ,  new_atlas_grey$index2  ) 
full_atlas = as.data.frame(full_atlas)
complement_atlas = subset(full_atlas, index2 %in% complement_index)

write.csv( new_atlas_white , "new_atlas_white.csv")
