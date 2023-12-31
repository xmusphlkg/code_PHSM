# Introduction

This is the code and data for the paper "Temporal trends and shifts of 24 notifiable infectious diseases in China before and during the COVID-19 epidemic" by Kangguo Li et al. (2023).

## Data

All data were collected from the monthly Notifiable Infectious Diseases Reports published by the National Health Commission of China. These reports aggregate data derived from the National Notifiable Disease Surveillance System, which was established in 2004. The study period spans from January 2008 to July 2023. However, for specific monthly reports in 2013 (January, February, April, and August), data were sourced directly from the Chinese public health science data center (https://www.phsciencedata.cn/share/ky_sjml.jsp). And avaliable data for the study period is available at [data](./data) folder.

## Code

The code is written in R 4.3.1. The code for the analysis is available at [code](./code) folder. The code is organized as follows:

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
@misc{doi:10.21203/rs.3.rs-3637503/v1,
	doi = {10.21203/rs.3.rs-3637503/v1},
	publisher = {Research Square Platform LLC},
	title = {Temporal trends and shifts of 24 notifiable infectious diseases in China before and during the COVID-19 epidemic},
	author = {Chen, Tianmu and Li, Kangguo and Rui, Jia and Song, Wentao and Liu, Chan and Zhao, Yunkang and Qu, Huimin and Wei, Hongjie and Zhang, Ruixin and Abudunaibi, Buasiyamu and Wang, Yao and Zhou, Zecheng and Liu, Hong and Xiang, Tianxin},
	note = {[Online; accessed 2023-12-01]},
	date = {2023-11-23},
	year = {2023},
	month = {11},
	day = {23},
}
```

```
Chen, Tianmu, et al. Temporal Trends and Shifts of 24 Notifiable Infectious Diseases in China before and during the COVID-19 Epidemic. Research Square Platform LLC, 23 Nov. 2023. Accessed 1 Dec. 2023.
```
