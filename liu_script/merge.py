import os
import re
import pandas as pd
import numpy as np
from openpyxl import load_workbook
from tqdm import tqdm
import time

provinces=os.listdir('./data/province')
data_center=pd.read_csv('./data/latest_data_center.csv')
book = load_workbook('./data/nation_and_provinces.xlsx')
#同步报告数据
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
book.save('./data/nation_and_provinces.xlsx')
#sheet首字母变大写
def capitalize_sheet_names(file_path):
    workbook = load_workbook(file_path)
    for sheet_name in workbook.sheetnames:
        first_letter = sheet_name[0].upper()
        new_sheet_name = first_letter + sheet_name[1:]
        workbook[sheet_name].title = re.sub('[^\u4e00-\u9fa5a-zA-Z]', '',new_sheet_name)
    workbook.save(file_path)

file_path = './data/nation_and_provinces.xlsx'
capitalize_sheet_names(file_path)

#diseaseName2Code
df_mapping =pd.read_csv('./liu_script/diseaseName2Code.csv')
with pd.ExcelFile('./data/nation_and_provinces.xlsx') as writer:
    for sheet_name in writer.sheet_names:
        df_sheet = pd.read_excel(writer, sheet_name)
        if 'disease_cn' in df_sheet.columns:
            for index, row in df_sheet.iterrows():
                disease_cn_value = row['disease_cn']
                matching_row = df_mapping[df_mapping['Name'] == disease_cn_value]
                if not matching_row.empty:
                    code_value = matching_row.iloc[0]['Code']
                    df_sheet.at[index, 'disease_en'] = code_value

            # 将更新后的数据写回原来的sheet
            with pd.ExcelWriter('./data/nation_and_provinces.xlsx', engine='openpyxl', mode='a') as writer:
                df_sheet.to_excel(writer, sheet_name, index=False)


#同步数据中心数据
for i in tqdm(range(len(data_center)), desc="Processing", unit="iteration"):
    data_list=[]
    data_list.append([data_center['Date'][i],data_center['Cases'][i],data_center['DiseasesCN'][i],
                      data_center['Diseases'][i],'DataCenter',data_center['URL'][i],
                      str(data_center['YearMonthDay'][i]).split('/')[0],str(data_center['YearMonthDay'][i]).split('/')[1]])
    df_list = pd.DataFrame(data_list,
                           columns=["date", "value", "disease_cn", "disease_en", "source", "url", "year", "month"])
    sheet_name=data_center['Province'][i]
    try:
        existing_df = pd.read_excel(file_path, sheet_name=sheet_name)
        combined_df = pd.concat([existing_df, df_list], axis=1)
        combined_df.to_excel('a.xlsx', sheet_name=sheet_name, index=False)
    except:
        df_list.to_excel(file_path, sheet_name=sheet_name, index=False)

book.save('./data/nation_and_provinces.xlsx')