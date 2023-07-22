
library(readxl)
library(dplyr)
library(magrittr) 

new_label_index_path = '/Users/ali/Desktop/Jul23/fmri_jayvik/codes//reduced_label.csv'
new_label_index = read.csv( new_label_index_path , header = F)
new_label_index = as.numeric(t(new_label_index))
atlas_path= '/Users/ali/Desktop/Jul23/fmri_jayvik/codes/only_gray/CHASSSYMM3AtlasLegends.xlsx'
full_atlas = read_xlsx(atlas_path)
full_atlas_index = as.numeric(unique(full_atlas$index2))


new_label_index %in% unique(full_atlas$index2)
missing = setdiff(full_atlas_index,new_label_index)
which(missing == full_atlas_index)
missing_atlas = full_atlas[full_atlas_index%in% missing ,]
#write.csv( missing_atlas , "missing_atlas.csv")
new_atlas= full_atlas[!full_atlas_index%in% missing ,]
#write.csv( new_atlas , "new_atlas.csv")



index_white_matter = new_atlas$Level_4=="white_matter"
new_atlas_white
new_atlas_white= new_atlas[index_white_matter ,]
write.csv( new_atlas_white , "new_atlas_white.csv")



index_grey_matter = !new_atlas$Level_4=="white_matter"
new_atlas_grey= new_atlas[index_grey_matter ,]
write.csv( new_atlas_grey , "new_atlas_grey.csv")


