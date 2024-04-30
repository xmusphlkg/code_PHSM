# Introduction

This is the code and data for the paper "Temporal shifts in 24 notifiable infectious diseases in China before and during the COVID-19 pandemic" by Kangguo Li et al. (2024).

## Data

All data were collected from the monthly Notifiable Infectious Diseases Reports published by the National Health Commission of China. These reports aggregate data derived from the National Notifiable Disease Surveillance System, which was established in 2004. The study period spans from January 2008 to December. However, for specific NIDs, such as HFMD, AHC, infectious diarrhea, mumps, rubella, echinococcosis and typhus, data were collected by the NNDSS from January 2008 to February 2009 but were not reflected in the monthly NIDs Reports. For these diseases, we relied on data provided by the Chinese Public Health Science Data Center (https://www.phsciencedata.cn/share/ky_sjml.jsp), maintained by the Chinese CDC, which also aggregates data from the NNDSS based on onset date and includes early NID data. And available data for the study period is available at [data](./data) folder.

### Code

The code is scripted in R version 4.3.2 and Python version 3.10.8.

R session information:

```
R version 4.3.2 (2023-10-31 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19045)

Matrix products: default


locale:
[1] LC_COLLATE=en  LC_CTYPE=en    LC_MONETARY=en LC_NUMERIC=C   LC_TIME=en
system code page: 65001

time zone: Asia/Shanghai
tzcode source: internal

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods   base

other attached packages:
 [1] sf_1.0-8              ggdendroplot_0.1.0    factoextra_1.0.7      ggtext_0.1.2
 [5] ggh4x_0.2.6           greyforecasting_0.1.4 doParallel_1.0.17     iterators_1.0.14
 [9] foreach_1.5.2         ggpubr_0.4.0          bsts_0.9.9            xts_0.12.1
[13] zoo_1.8-11            BoomSpikeSlab_1.2.5   Boom_0.9.11           caret_6.0-93
[17] lattice_0.21-9        prophet_1.0           rlang_1.1.1           Rcpp_1.0.9
[21] forecastHybrid_5.0.19 thief_0.3             forecast_8.18         astsa_1.16
[25] jsonlite_1.8.7        tseries_0.10-52       scales_1.3.0          RColorBrewer_1.1-3
[29] extrafont_0.18        paletteer_1.6.0       patchwork_1.1.2       Cairo_1.6-0
[33] ggsci_2.9             openxlsx_4.2.5        lubridate_1.9.2       forcats_1.0.0
[37] stringr_1.5.0         dplyr_1.1.0           purrr_1.0.1           readr_2.1.4
[41] tidyr_1.3.0           tibble_3.1.8          ggplot2_3.4.4         tidyverse_2.0.0

loaded via a namespace (and not attached):
 [1] DBI_1.1.3            pROC_1.18.2          rematch2_2.1.2       magrittr_2.0.3       e1071_1.7-11
 [6] compiler_4.3.2       vctrs_0.6.5          reshape2_1.4.4       quadprog_1.5-8       pkgconfig_2.0.3
[11] backports_1.4.1      ellipsis_0.3.2       utf8_1.2.2           prodlim_2019.11.13   tzdb_0.3.0
[16] recipes_1.0.5        broom_1.0.5          R6_2.5.1             stringi_1.7.8        car_3.1-0
[21] parallelly_1.32.1    extrafontdb_1.0      rpart_4.1.19         lmtest_0.9-40        future.apply_1.10.0
[26] Matrix_1.5-1         splines_4.3.2        nnet_7.3-19          timechange_0.2.0     tidyselect_1.2.0
[31] abind_1.4-5          rstudioapi_0.14      timeDate_4021.106    codetools_0.2-19     curl_4.3.3
[36] listenv_0.8.0        plyr_1.8.8           quantmod_0.4.20      withr_2.5.0          urca_1.3-3
[41] future_1.29.0        survival_3.5-7       units_0.8-0          proxy_0.4-27         RcppParallel_5.1.7
[46] xml2_1.3.3           zip_2.2.1            pillar_1.8.1         carData_3.0-5        KernSmooth_2.23-22
[51] stats4_4.3.2         generics_0.1.3       TTR_0.24.3           hms_1.1.2            munsell_0.5.0
[56] globals_0.16.2       class_7.3-22         glue_1.6.2           tools_4.3.2          data.table_1.14.2
[61] ModelMetrics_1.2.2.2 gower_1.0.1          ggsignif_0.6.3       cowplot_1.1.1        grid_4.3.2
[66] Rttf2pt1_1.3.11      ipred_0.9-13         colorspace_2.0-3     nlme_3.1-163         fracdiff_1.5-1
[71] cli_3.6.1            fansi_1.0.3          lava_1.7.1           gtable_0.3.1         rstatix_0.7.1
[76] digest_0.6.29        classInt_0.4-8       prismatic_1.1.1      ggrepel_0.9.1        farver_2.1.1
[81] lifecycle_1.0.3      hardhat_1.2.0        gridtext_0.1.5       MASS_7.3-60
```

Python session information:

```
Package               Version
--------------------- ---------
openpyxl              3.0.10
pandas                1.5.0
PyPDF2                3.0.1
reportlab             4.0.5
```

### Preparation

The replication of this project's code is advised to be executed on a server, with the following recommended specifications:

The CPU offering no fewer than 30 threads and a minimum of 32GB of operating memory.

### Run

The code for the analysis is available at [code](./code) folder. The code is organized as follows:

- [1](./script/1_a_overview.R): data preparation and analysis
- [2](./script/2_b_diseases.R): analysis each disease
- [3](./script/3_a_forecast.R): training and testing time series models
- [4](./script/3_a_select_model.R): select the best model for each disease
- [5](./script/4_a_forecast.R): forecast the incidence of each disease
- [6](./script/5_a_relation.R): analysis the relationship between monthly incidence and PHSMs index
- [7](./script/6_a_cross.R): cross-correlation analysis

# Lisence

The code and data are released under GNU General Public License v3.0.

# Citation

If you find this work useful in your research, please cite the following paper:

```
Kangguo Li, et al. Temporal shifts in 24 notifiable infectious diseases in China before and during the COVID-19 pandemic. Nature Communications. https://doi.org/10.1038/s41467-024-48201-8 2024.
```
