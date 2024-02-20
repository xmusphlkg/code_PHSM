import pandas as pd
import os
import sys
import numpy as np

# 读取省份数据
data = pd.ExcelFile("data/nation_and_provinces.xlsx")
sheet_names = data.sheet_names
Class = pd.read_excel("data/nation_and_provinces.xlsx", sheet_name="Class")
yilou = ['year', 'month', 'province', 'disease']
for i in range(4, len(sheet_names) + 4):
    sheet_name = sheet_names[i]
    province_data = pd.read_excel("data/nation_and_provinces.xlsx", sheet_name=sheet_name)
    # 获取每一个月每一年的数据
    year_min = province_data["year"].min()
    year_max = province_data["year"].max()
    for year in range(year_min, year_max + 1):
        year_data = province_data[province_data["year"] == year]
        month_min = year_data["month"].min()
        month_max = year_data["month"].max()
        if len(year_data) == 0:
            pass
        else:
            for month in range(int(month_min), int(month_max) + 1):
                month_data = province_data[(province_data["year"] == year) & (province_data["month"] == month)]
                # 获取每一个月的省份数据
                if len(month_data) == 0:
                    pass
                else:
                    for index in range(1, len(Class)):
                        diseasename = Class.iloc[index, 1]
                        diseasename_cn= Class.iloc[index, 3]
                        if  diseasename in month_data.iloc[:, 3].values:
                            pass
                        else:
                            if diseasename_cn in month_data.iloc[:, 2].values:
                                pass
                            else:
                                print(sheet_name, year, month, diseasename_cn, diseasename)

