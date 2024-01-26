import os
from html import unescape
import pandas as pd
import requests
import xlrd
from bs4 import BeautifulSoup
from liu_script.dataclean import update_url_column, read_docx
from liu_script.zhejiang_dataclean import remove_space, filetype

origin_url = 'https://wsjkw.sh.gov.cn/'
data = []
name='shanghai'
for i in range(1, 10):
    if i==1:
        url = "https://wsjkw.sh.gov.cn/yqxx/index.html"
    else:
        url=f"https://wsjkw.sh.gov.cn/yqxx/index_{i-1}.html"
    headers = {
        "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
        "Accept-Encoding": "gzip, deflate, br",
        "Accept-Language": "zh-CN,zh;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6",
        "Cache-Control": "max-age=0",
        "Connection": "keep-alive",
        "Cookie": "zh_choose=s; Path=/; https_waf_cookie=d2fe8708-24f1-45be3075a8f2f7d705eef8d3e96ddaffd6f6; _pk_ref.30.0806=%5B%22%22%2C%22%22%2C1705539132%2C%22https%3A%2F%2Fwww.bing.com%2F%22%5D; _pk_testcookie.30.0806=1; _pk_ses.30.0806=1; AlteonP=AptubWHbHKzkFdkv1HW1ew$$; _pk_id.30.0806=5b36433bca6ab400.1705324366.2.1705539159.1705539132.",
        "Host": "wsjkw.sh.gov.cn",
        "If-Modified-Since": "Thu, 11 Jan 2024 03:47:21 GMT",
        "If-None-Match": 'W/"659f64c9-3abf"',
        "Referer": "https://wsjkw.sh.gov.cn/yqxx/index_9.html",
        "Sec-Fetch-Dest": "document",
        "Sec-Fetch-Mode": "navigate",
        "Sec-Fetch-Site": "same-origin",
        "Sec-Fetch-User": "?1",
        "Upgrade-Insecure-Requests": "1",
        "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36 Edg/122.0.0.0",
        "sec-ch-ua": '"Chromium";v="122", "Not(A:Brand";v="24", "Microsoft Edge";v="122"',
        "sec-ch-ua-mobile": "?0",
        "sec-ch-ua-platform": '"Windows"'
    }
    response = requests.get(url, headers=headers)
    soup = BeautifulSoup(response.content, "html.parser")
    links_with_blank_target = soup.find_all('a', {'target': '_blank'})
    for link in links_with_blank_target:
        chinese_text = link.text.strip()
        if '传染病' in chinese_text:
            url_link = link.get('href')
            data.append([chinese_text, str(origin_url) + url_link[1:],url])
def get_year_month(title):
    year = title.split('年')[0]
    month = title.split('年')[1].split('月')[0]
    return year, month
df = pd.DataFrame(data, columns=['中文解释', '链接','Referer'])
df = df[df['中文解释'].str.contains('月')]
df['年份'],df['月份'] = zip(*df['中文解释'].apply(lambda x: get_year_month(x)))
df['年份'] = df['年份'].str.replace(r'\D', '', regex=True)
df.replace('\xa0', '', regex=True, inplace=True)
df.replace('\u3000', '', regex=True, inplace=True)
df.replace('\u2002', '', regex=True, inplace=True)
df.replace(' ', '', regex=True, inplace=True)
df.to_csv(f'./data/province/{name}/{name}_url.csv', index=False, encoding='gbk')

for i in range(len(df)):
        url = df.iloc[i]['链接']
        origin_url='https://wsjkw.sh.gov.cn/'
        headers = {
            'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7',
            'Accept-Encoding': 'gzip, deflate, br',
            'Accept-Language': 'zh-CN,zh;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6',
            'Cache-Control': 'max-age=0',
            'Connection': 'keep-alive',
            'Cookie': 'Path=/; zh_choose=s; Path=/; https_waf_cookie=d2fe8708-24f1-45be3075a8f2f7d705eef8d3e96ddaffd6f6; _pk_ref.30.0806=%5B%22%22%2C%22%22%2C1705539132%2C%22https%3A%2F%2Fwww.bing.com%2F%22%5D; _pk_testcookie.30.0806=1; _pk_ses.30.0806=1; AlteonP=Ax/cTmHbHKyAw3l5yXBlVA$$; _pk_id.30.0806=5b36433bca6ab400.1705324366.2.1705539475.1705539132.',
            'Host': 'wsjkw.sh.gov.cn',
            'If-Modified-Since': 'Wed, 28 Jun 2023 08:24:36 GMT',
            'If-None-Match': 'W/"649bee44-4be8"',
            'Referer': df.iloc[i]['Referer'],
            'Sec-Fetch-Dest': 'document',
            'Sec-Fetch-Mode': 'navigate',
            'Sec-Fetch-Site': 'same-origin',
            'Sec-Fetch-User': '?1',
            'Upgrade-Insecure-Requests': '1',
            'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36 Edg/122.0.0.0',
            'sec-ch-ua': '"Chromium";v="122", "Not(A:Brand";v="24", "Microsoft Edge";v="122"',
            'sec-ch-ua-mobile': '?0',
            'sec-ch-ua-platform': '"Windows"'
        }

        sign=0
        response=requests.get(origin_url+url[1:], headers=headers)
        soup=BeautifulSoup(response.content,'html.parser',from_encoding='gbk')
        links_with_blank_target = soup.find_all({'li'})
        try:
            for url in links_with_blank_target:
                try:
                    link=url.find('a')['href']
                except:
                    pass
                if 'docx' in str(link):
                    sign=1
                    url_link = unescape(link)
                    response = requests.get(url_link, headers=headers)
                    if response.status_code == 200:
                        with open(f'./data/province/{name}/{df["年份"].iloc[i]}-{df["月份"].iloc[i]}.docx', 'wb') as f:
                            f.write(response.content)
                    else:
                        print(f'{df["年份"].iloc[i]}-{df["月份"].iloc[i]}存在，下载失败，但是200')
                if 'doc' in str(link) and 'docx' not in str(link) :
                    sign=1
                    url_link = unescape(str(origin_url) + link[1:])
                    response = requests.get(url_link, headers=headers)
                    if response.status_code == 200:
                        with open(f'./data/province/{name}/{df["年份"].iloc[i]}-{df["月份"].iloc[i]}.doc', 'wb') as f:
                            f.write(response.content)
                    else:
                        print(f'{df["年份"].iloc[i]}-{df["月份"].iloc[i]}存在，下载失败，但是200')
                if 'xls' in str(link) and 'xlsx' not in str(link) :
                    sign=1
                    url_link = unescape(str(origin_url) + link[1:])
                    response = requests.get(url_link, headers=headers)
                    if response.status_code == 200:
                        with open(f'./data/province/{name}/{df["年份"].iloc[i]}-{df["月份"].iloc[i]}.xls', 'wb') as f:
                            f.write(response.content)
                    else:
                        print(f'{df["年份"].iloc[i]}-{df["月份"].iloc[i]}存在，下载失败，但是200')
                if 'xlsx' in str(link) :
                    sign=1
                    url_link = unescape(str(origin_url) + link[1:])
                    response = requests.get(url_link, headers=headers)
                    if response.status_code == 200:
                        with open(f'./data/province/{name}/{df["年份"].iloc[i]}-{df["月份"].iloc[i]}.xlsx', 'wb') as f:
                            f.write(response.content)
                    else:
                        print(f'{df["年份"].iloc[i]}-{df["月份"].iloc[i]}存在，下载失败，但是200')
        except:
            pass
        try:
            table = soup.find('tbody')
            rows = table.find_all('tr')
            table_data = []
            for row in rows:
                cells = row.find_all(['td'])
                row_data = [cell.text.strip() for cell in cells]
                table_data.append(row_data)
            table_df = pd.DataFrame(table_data)
            table_df.replace('\xa0', '', regex=True, inplace=True)
            table_df.replace('\u3000', '', regex=True, inplace=True)
            table_df.replace('\u2002', '', regex=True, inplace=True)
            table_df.replace(' ', '', regex=True, inplace=True)
            table_df.to_csv(f'./data/province/{name}/{df["年份"].iloc[i]}-{df["月份"].iloc[i]}.csv',encoding='gbk',index=False)
            sign = 1
        except:
            pass
        if sign==0:
            print(f'{df["年份"].iloc[i]}-{df["月份"].iloc[i]}不存在传染病信息')

def process_files_combined(directory):
    data_list = []

    files = os.listdir(directory)

    for file in files:
        file_path = os.path.join(directory, file)
        file_size = os.path.getsize(file_path)

        if file_size > 100:
            file_type = filetype(file)
            if file_type == 'doc':
                pass
            if file_type == 'docx':
                text_content, table_content = read_docx(file_path)
                df = pd.DataFrame(table_content)
                for i in range(len(df)):
                    try:
                        df.iloc[i][3] = float(df.iloc[i][3])
                    except:
                        pass
                    if isinstance(df.iloc[i][0], str) and isinstance(df.iloc[i][3], (int, float)) :
                        data_list.append(
                            [remove_space(df.iloc[i][0]), df.iloc[i][3], file.split('.')[0]])
            elif file_type == 'xls':
                sheet = xlrd.open_workbook(file_path).sheet_by_index(0)
                num_rows = sheet.nrows
                num_cols = sheet.ncols
                data = []

                for row_index in range(num_rows):
                    row_data = []

                    for col_index in range(num_cols):
                        cell_value = sheet.cell_value(row_index, col_index)
                        row_data.append(cell_value)

                    data.append(row_data)

                df = pd.DataFrame(data)

                for i in range(len(df)):
                    try:
                        df.iloc[i][3] = float(df.iloc[i][3])
                    except:
                        pass
                    if isinstance(df.iloc[i][0], str) and isinstance(df.iloc[i][3], (int, float)) :
                        data_list.append(
                            [remove_space(df.iloc[i][0]), df.iloc[i][3], file.split('.')[0]])
            elif file_type == 'csv':
                df = pd.read_csv(file_path,encoding='gbk')
                for i in range(len(df)):
                    if str(df.iloc[0,0])=='0':
                        try:
                            df.iloc[i, 3] = float(df.iloc[i, 3])
                        except:
                            pass
                        if isinstance(df.iloc[i][1], str) and isinstance(df.iloc[i][3], (int, float)) and isinstance(
                                df.iloc[i][3], (int, float)):
                            data_list.append(
                                [remove_space(df.iloc[i][0]), df.iloc[i][3], file.split('.')[0]])
                    else:
                        try:
                            df.iloc[i, 3] = float(df.iloc[i, 3])
                        except:
                            pass
                        try:
                            if isinstance(df.iloc[i][1], str) and isinstance(df.iloc[i][3],
                                                                             (int, float)) and isinstance(
                                    df.iloc[i][3], (int, float)):
                                data_list.append(
                                    [remove_space(df.iloc[i][0]), df.iloc[i][3], file.split('.')[0]])
                        except:
                            pass
            elif file_type == 'xlsx':
                sheet = pd.read_excel(file_path)
                df = sheet

                for i in range(len(df)):
                    try:
                        df.iloc[i][3] = float(df.iloc[i][3])
                    except:
                        pass
                    if isinstance(df.iloc[i][0], str) and isinstance(df.iloc[i][3], (int, float)) :
                        data_list.append(
                            [remove_space(df.iloc[i][0]), df.iloc[i][3], file.split('.')[0]])

    result_df = pd.DataFrame(data_list, columns=['疾病病种', '发病数',  'date'])
    return result_df
files = os.listdir(f'./data/province/{name}/')
a=process_files_combined(f'./data/province/{name}/')
a.replace('\xa0', '', regex=True, inplace=True)
a.replace('\u3000', '', regex=True, inplace=True)
a.replace('\u2002', '', regex=True, inplace=True)
a.replace(' ', '', regex=True, inplace=True)
a.to_csv(f'./data/province/{name}/{name}.csv',index=False,encoding='gbk')
update_url_column(f'./data/province/{name}/{name}.csv',f'./data/province/{name}/{name}_url.csv')