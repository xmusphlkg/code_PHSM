import os
import re
import pandas as pd
import numpy as np
from openpyxl import load_workbook

provinces=os.listdir('./data/province')
data_center=pd.read_csv('./data/latest_data_center.csv')
book = load_workbook('./data/nation_and_provinces.xlsx')

for province in provinces:
    sheet_name = province
    if sheet_name in book.sheetnames:
        std = book[sheet_name]
        book.remove(std)
    data_list=[]
    df_report=pd.read_csv(f'./data/province/{province}/{province}.csv',encoding='gbk')
    for i in range(len(df_report)):
        data_list.append([pd.to_datetime(f"{df_report['year'][i]}-{df_report['month'][i]:02}", format='%Y-%m'),
                          df_report['发病数'][i],re.sub('[^\u4e00-\u9fa5a-zA-Z]', '',df_report['疾病病种'][i]),None,'Report',df_report['url'][i],
                          df_report['year'][i],df_report['month'][i]])
    df_list=pd.DataFrame(data_list,columns=["date", "value", "disease_cn", "disease_en", "source", "url", "year", "month"])
    print(province)
    try:
        with pd.ExcelWriter('./data/nation_and_provinces.xlsx', engine='openpyxl') as writer:
            writer.book = book
            df_list.to_excel(writer, sheet_name=sheet_name, index=False)
    except:
        pass

#sheet首字母变大写
def capitalize_sheet_names(file_path):
    workbook = load_workbook(file_path)
    for sheet_name in workbook.sheetnames:
        sheet = workbook[sheet_name]
        first_letter = sheet_name[0].upper()
        new_sheet_name = first_letter + sheet_name[1:]
        workbook[sheet_name].title = new_sheet_name
    workbook.save(file_path)

file_path = './data/nation_and_provinces.xlsx'
capitalize_sheet_names(file_path)



