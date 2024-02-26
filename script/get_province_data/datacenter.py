
## Importing libraries
import pandas as pd
import requests
import json
import sys
import time
import os
import glob
import xml.etree.ElementTree as ET
import datetime

# download data from CPHSDC
def download_data(i, urls, max_retries=5):
    year, month, diseaseId, url = urls[i]
    file_path = f"./data/province/datacenter/{year} {month} {diseaseId}.xls"
    
    for attempt in range(max_retries):
        try:
            response = requests.get(url)
            if response.status_code == 200:
                with open(file_path, 'wb') as f:
                    f.write(response.content)
                return True
        except requests.RequestException as e:
            print(f"尝试 {attempt + 1} of {max_retries}, 请求错误：{e}")
        time.sleep(2 ** attempt)
    
    print(f"下载失败，已达到最大尝试次数：{year} {month} {diseaseId}")
    return False

## get avaliable years
response = requests.get("https://www.phsciencedata.cn/Share/getQuerystart/")
if response.status_code == 200:
    data = json.loads(response.content)
    code_list = [item['code'] for item in data]
    years = code_list
    print("CPHSDC 访问成功")
else:
    print("CPHSDC 访问失败")
    sys.exit()

# detect existing dates
folder_path = "./data/province/datacenter"

# setting avaliable years
new_years = list(range(2008, 2021))
# setting avaliable months
months = list(range(1, 13))
# setting avaliable diseaseIds
data_class = pd.read_excel("./data/nation_and_provinces.xlsx", sheet_name="Class")
diseaseIds = list(data_class['diseaseID'])
diseaseNames = list(data_class['diseasename'])
diseaseLists = list(data_class['diseaselist'])

# generate a list containing all combinations of year, month, and diseaseId
url_para = []
for diseaseId in diseaseIds:
    for year in new_years:
        for month in months:
            url = f"https://www.phsciencedata.cn/Share/frameset?__report=ReportZoneMonth.rptdesign&__title=&__showtitle=false&__toolbar=true&__navigationbar=true&&__format=xls&__locale=zh_CN&__clean=true&__filename=%E5%8D%A0%E4%BD%8D%E7%AC%A6&years={year}&diseaseId={diseaseId}&months={month}&&__dpi=96&__asattachment=true&__overwrite=false"
            file_path = f"./data/province/datacenter/{year} {month} {diseaseId}.xls"
            if not os.path.exists(file_path):
                url_para.append([year, month, diseaseId, url])

# using multi-threading to download data
if url_para:
    import threading
    max_threads = 10

    def download_data_threading(urls, start, end):
        for i in range(start, end):
            download_data(i, urls)

    threads = []
    for i in range(0, len(url_para), max_threads):
        t = threading.Thread(target=download_data_threading, args=(url_para, i, min(i + max_threads, len(url_para))))
        threads.append(t)
        t.start()
else:
    print("没有需要下载的数据")

provinceName2Code = {
    '全国': 'Total',
    '北京': 'Beijing',
    '天津': 'Tianjin',
    '河北': 'Hebei',
    '山西': 'Shanxi',
    '山东': 'Shandong',
    '内蒙古': 'Inner Mongolia',
    '辽宁': 'Liaoning',
    '吉林': 'Jilin',
    '黑龙江': 'Heilongjiang',
    '上海': 'Shanghai',
    '江苏': 'Jiangsu',
    '浙江': 'Zhejiang',
    '安徽': 'Anhui',
    '福建': 'Fujian',
    '江西': 'Jiangxi',
    '山东': 'Shandong',
    '河南': 'Henan',
    '湖北': 'Hubei',
    '湖南': 'Hunan',
    '广东': 'Guangdong',
    '广西': 'Guangxi',
    '海南': 'Hainan',
    '重庆': 'Chongqing',
    '四川': 'Sichuan',
    '贵州': 'Guizhou',
    '云南': 'Yunnan',
    '西藏': 'Tibet',
    '陕西': 'Shaanxi',
    '甘肃': 'Gansu',
    '青海': 'Qinghai',
    '宁夏': 'Ningxia',
    '新疆': 'Xinjiang',
    '台湾': 'Taiwan',
    '香港': 'Hong Kong',
    '澳门': 'Macao'
    }

def process_files(xls_file):
    '''
    Process xls files

    Args:
        xls_files (list): list of xls files
        provinceName2Code (dict): dictionary of province name to province code
        provinceName2ADCode (dict): dictionary of province name to AD code
        diseaseName2Code (dict): dictionary of disease name to disease code
    Raises:
        None
    Returns:
        None
    '''
    if os.path.getsize(xls_file) > 6 * 1024:
        file_name = os.path.basename(xls_file)
        YearMonth = file_name.split(' ')[0] + " " + file_name.split(' ')[1]
        date_obj = datetime.datetime.strptime(YearMonth, '%Y %m')
        formatted_date = date_obj.strftime("%Y/%m/%d")
        tree = ET.parse(xls_file)
        root = tree.getroot()

        data = pd.DataFrame()

        for worksheet in root.iter('{urn:schemas-microsoft-com:office:spreadsheet}Worksheet'):
            worksheet_data = []
            table = worksheet.find('{urn:schemas-microsoft-com:office:spreadsheet}Table')

            row_counter = 0
            for row in table.findall('{urn:schemas-microsoft-com:office:spreadsheet}Row'):
                if row_counter < 1:
                    row_counter += 1
                    continue

                row_data = {}

                for cell in row.findall('{urn:schemas-microsoft-com:office:spreadsheet}Cell'):
                    data_elem = cell.find('{urn:schemas-microsoft-com:office:spreadsheet}Data')
                    cell_data = data_elem.text if data_elem is not None else ''
                    cell_data = cell_data.replace(' ', '').replace('省', '').replace('市', '').replace('\t', '')
                    row_data[cell.attrib.get('{urn:schemas-microsoft-com:office:spreadsheet}Index', '')] = cell_data

                worksheet_data.append(row_data)

            if worksheet_data:
                worksheet_data.pop()

            df = pd.DataFrame(worksheet_data)
            df = df.iloc[:, 1:]
            df.columns = pd.to_numeric(df.columns, errors='coerce')
            df = df.sort_index(axis=1)
            df.iloc[0, :] = df.iloc[0, :].ffill()

            diseases = df.iloc[0, :].unique()
            diseases = diseases[diseases != '']

            for disease in diseases:
                df_disease = df.loc[:, (df.iloc[0, :] == disease) | (df.iloc[0, :] == '')]
                df_disease = df_disease.iloc[1:, :]
                df_disease = df_disease.dropna(axis=1, how='all')
                df_disease = df_disease.dropna(axis=0, how='all')
                df_disease.columns = df_disease.iloc[0, :]
                df_disease = df_disease.iloc[1:, :]
                df_disease['date'] = date_obj.strftime("%Y-%m-%d %H:%M:%S")
                df_disease['YearMonthDay'] = formatted_date
                df_disease['YearMonth'] = YearMonth
                df_disease['disease_cn'] = disease
                df_disease = df_disease.reset_index(drop=True)
                data = pd.concat([data, df_disease], ignore_index=True)

        data = data.rename(columns={'地区': 'ProvinceCN', '发病数': 'value', '死亡数': 'Deaths'})
        data['province'] = data['ProvinceCN'].replace(provinceName2Code)
        data['url'] = 'https://www.phsciencedata.cn/Share/ky_sjml.jsp'
        data['source'] = 'DataCenter'
        data = data[['date', 'value', 'disease_cn', 'province', 'source', 'url']]

        diseases = data['disease_cn'].unique()
        diseases = [disease.title() for disease in diseases]

        file_name = file_name.split('.')[0]
        output_file = f'./data/province/datacenter/{file_name}.csv'
        data.to_csv(output_file, index=False, encoding='utf-8', header=True)

# access the folder
xls_files = glob.glob(os.path.join(folder_path, '*.xls'))
xls_files.sort()

# multi-threading to process files
# if xls_files:
#     import threading
#     max_threads = 10

#     def process_files_threading(xls_files, start, end):
#         for i in range(start, end):
#             process_files(xls_files[i])

#     threads = []
#     for i in range(0, len(xls_files), max_threads):
#         t = threading.Thread(target=process_files_threading, args=(xls_files, i, min(i + max_threads, len(xls_files))))
#         threads.append(t)
#         t.start()

# combine all csv files
csv_files = glob.glob(os.path.join(folder_path, '*.csv'))
csv_files.sort()

dataframes = []

for f in csv_files:
    try:
        df = pd.read_csv(f)
        if not df.empty:
            dataframes.append(df)
    except pd.errors.EmptyDataError:
        print(f"Warning: The file {f} is empty and has been skipped.")
        continue

data = pd.concat(dataframes, ignore_index=True)
# split date into date and time
data['date'] = pd.to_datetime(data['date'])
data['date'] = data['date'].dt.strftime('%Y-%m-%d')
data.to_csv(f'./data/datacenter.csv', index=False, encoding='utf-8', header=True)