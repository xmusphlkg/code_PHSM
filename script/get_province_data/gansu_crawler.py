import os
import pandas as pd
from dataclean import process_files_combined, update_url_column
from html import unescape
import pandas as pd
import requests
import xlrd
from bs4 import BeautifulSoup
from dataclean import update_url_column, read_docx, remove_space, filetype

# The above code is scraping data from a website related to the pandept of epidemiology and public health in Gansu.
# The website is called "公共卫生数据科学中心" (Center for Public Health Data Science), and it is responsible for monitoring
# and controlling the epidemic situation in Nation containing Gansu Province. It retrieves information from multiple
# pages and extracts links that contain the text "传染病" (which means "infectious disease" in Chinese). The extracted
# data includes the Chinese text, the complete URL of the link, and the original URL of the website.
origin_url = 'https://www.phsciencedata.cn/'
data = []
name='gansu'
for i in range(1, 111):
    url=f"https://www.phsciencedata.cn/Share/jsp/PublishManager/ky_RecentDiseasesData.jsp?post_type=309&PageNo={i}&title="
    headers = {
        "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
        "Accept-Encoding": "gzip, deflate, br",
        "Accept-Language": "zh-CN,zh;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6",
        "Connection": "keep-alive",
        "Cookie": "Path=/; JSESSIONID=6BBFB3E70709678D79E164325D4A68CE; Path=/; _pk_id.2.49bb=7c34fe776e45e907.1708351703.; acw_tc=2760776017085973952982370ed07ff66752acd127e4608fc0b0bf191b968a; Hm_lvt_003425c0fa587622228cf3ba0958661d=1708351703,1708597399; _pk_ses.2.49bb=1; SERVERID=ea3debcd63ed8946d7e8f71ad02941ca|1708597522|1708597395; Hm_lpvt_003425c0fa587622228cf3ba0958661d=1708597519",
        "DNT": "1",
        "Host": "www.phsciencedata.cn",
        "Referer": "https://www.phsciencedata.cn/Share/jsp/PublishManager/ky_RecentDiseasesData.jsp?post_type=309&PageNo=3&title=",
        "Sec-Fetch-Dest": "document",
        "Sec-Fetch-Mode": "navigate",
        "Sec-Fetch-Site": "same-origin",
        "Sec-Fetch-User": "?1",
        "Upgrade-Insecure-Requests": "1",
        "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/121.0.0.0 Safari/537.36 Edg/121.0.0.0",
        "sec-ch-ua": '"Not A(Brand";v="99", "Microsoft Edge";v="121", "Chromium";v="121"',
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

data_gansu = []
for i in data:
    if "甘肃" in i[0]:
        data_gansu.append(i)

def get_year_month(title):
    """
    The function `get_year_month` extracts the year and month from a given title, and the code snippet
    processes a DataFrame to extract the year and month from the '中文解释' column and save the result to a
    CSV file.

    :param title: The `title` parameter is a string that represents the title of a data entry
    :return: The code is returning a DataFrame object that has been modified and saved as a CSV file.
    """
    year = title.split('年')[0]
    month = title.split('年')[1].split('月')[0]
    return year, month

df = pd.DataFrame(data_gansu, columns=['中文解释', '链接','Referer'])
df = df[df['中文解释'].str.contains('月')]
df['年份'],df['月份'] = zip(*df['中文解释'].apply(lambda x: get_year_month(x)))
df['年份'] = df['年份'].str.replace(r'\D', '', regex=True)
df.replace('\xa0', '', regex=True, inplace=True)
df.replace('\u3000', '', regex=True, inplace=True)
df.replace('\u2002', '', regex=True, inplace=True)
df.replace(' ', '', regex=True, inplace=True)
df.to_csv(f'./data/province/{name}/{name}_url.csv', index=False, encoding='gbk')

# The above code is a Python script that iterates over a DataFrame called `df`. For each row in the
# DataFrame, it extracts a URL from the '链接' column and sends a GET request to that URL with specific
# headers. It then uses BeautifulSoup to parse the HTML content of the response.
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

def process_gansu_data():
    """
    Process data for the province of Gansu.
    
    This function reads Excel files from the './data/province/gansu/' directory,
    cleans the data by removing special characters, and saves the cleaned data
    as CSV files. It also combines all the CSV files into a single DataFrame,
    further cleans the combined data, and saves it as a separate CSV file.
    Finally, it updates the URL column in the combined data CSV file.
    """
    name = 'gansu'
    
    # Process individual Excel files
    files = os.listdir(f'./data/province/{name}/')
    for file in files[:-1]:
        print(file)
        df = pd.read_excel(f'./data/province/{name}/{file}', header=None)
        df.replace('\xa0', '', regex=True, inplace=True)
        df.replace('\u3000', '', regex=True, inplace=True)
        df.replace('\u2002', '', regex=True, inplace=True)
        df.replace('\u2002', '', regex=True, inplace=True)
        df.replace(' ', '', regex=True, inplace=True)
        df.reset_index()
        df = df.iloc[1:]
        df.to_csv(f'./data/province/{name}/{file[:-10]}.csv', index=False, encoding='gbk')

    # Process combined data
    files = os.listdir(f'./data/province/{name}/')
    a = process_files_combined(f'./data/province/{name}/')
    a.replace('\xa0', '', regex=True, inplace=True)
    a.replace('\u3000', '', regex=True, inplace=True)
    a.replace('\u2002', '', regex=True, inplace=True)
    a.replace(' ', '', regex=True, inplace=True)
    a.to_csv(f'./data/province/{name}/{name}.csv', index=False, encoding='gbk')
    update_url_column(f'./data/province/{name}/{name}.csv', f'./data/province/{name}/{name}_url.csv')
