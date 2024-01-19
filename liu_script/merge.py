import os
import pandas as pd
import numpy as np

provinces=os.listdir('./data/province')
data_center=pd.read_csv('./data/latest_data_center.csv')

for province in provinces:
    df=pd.DataFrame(columns=["date", "value", "disease_cn", "disease_en", "source", "url", "year", "month"])
    nation_and_provinces = pd.read_excel('./data/nation_and_provinces.xlsx', sheet_name=province)
    df_report=pd.read_csv(f'./data/province/{province}/{province}.csv',encoding='gbk')
