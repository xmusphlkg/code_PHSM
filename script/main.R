
source("./script/1_a_overview.R")
print("Figure 1 generated")
rm(list = ls())

source("./script/2_a_diseases.R")
print("Figure 2 generated")
rm(list = ls())

source("./script/3_a_select_model.R")
print("Appendix 1_2 figures generated")
rm(list = ls())

reticulate::source_python('./script/3_b_merge_appendix.py')
print("Appendix 1_2 combinded")
rm(list = ls())

source("./script/3_c_best_model.R")
print("Figure 3 generated")
rm(list = ls())

source("./script/4_a_forecast.R")
print("Figure 4 generated")
rm(list = ls())

source("./script/5_a_analysis.R")
print("Figure 5 generated")
rm(list = ls())

source("./script/6_a_relation.R")
print("Figure 6 generated")
rm(list = ls())

source("./script/7_a_cross.R")
print("Figure 7 generated")
rm(list = ls())

source("./script/8_a_province.R")
print("Appendix 1_1 figures generated")
rm(list = ls())

reticulate::source_python('./script/8_b_merge_appendix.py')
print("Appendix 1 combinded")
rm(list = ls())

