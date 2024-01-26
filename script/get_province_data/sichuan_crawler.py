import os
from html import unescape
import pandas as pd
import requests
import xlrd
from bs4 import BeautifulSoup
from dataclean import update_url_column, read_docx, remove_space, filetype, process_files_combined

def get_year_month(title):
    """
    The function `get_year_month` takes a title as input and returns the year and month extracted from
    the title.
    
    :param title: The title is a string that represents a title containing a year and month
    :return: a tuple containing the year and month extracted from the given title.
    """
    year = title.split('年')[0]
    month = title.split('年')[1].split('月')[0]
    return year, month

# The code scrapes data from a website and saves it to a CSV file.

data = []
name='sichuan'
for i in range(1, 30):
    url = f"https://wsjkw.sc.gov.cn/guestweb4/s?searchWord=%25E6%25B3%2595%25E5%25AE%259A%25E4%25BC%25A0%25E6%259F%2593%25E7%2597%2585&column=%25E5%2585%25A8%25E9%2583%25A8&wordPlace=1&orderBy=1&startTime=&endTime=&pageSize=10&pageNum={i-1}&timeStamp=0&siteCode=5100000053&siteCodes=&checkHandle=1&strFileType=%25E5%2585%25A8%25E9%2583%25A8%25E6%25A0%25BC%25E5%25BC%258F&sonSiteCode=&areaSearchFlag=1&secondSearchWords=&countKey=%200&left_right_index=0"
    headers = {
        "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
        "Accept-Encoding": "gzip, deflate, br",
        "Accept-Language": "zh-CN,zh;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6",
        "Cache-Control": "max-age=0",
        "Connection": "keep-alive",
        "Cookie": "firstWord=%u6CD5%u5B9A%u4F20%u67D3%u75C5; userSearch=siteCode-5100000053&column-%E5%85%A8%E9%83%A8&uc-0&firstWord-%E6%B3%95%E5%AE%9A%E4%BC%A0%E6%9F%93%E7%97%85&searchWord-%E6%B3%95%E5%AE%9A%E4%BC%A0%E6%9F%93%E7%97%85&searchTime-20240118122726&searchUseTime-54; yfx_c_g_u_id_10000024=_ck24011521102910517165021452751; yfx_f_l_v_t_10000024=f_t_1705324229031__r_t_1705543071316__v_t_1705551675990__r_c_1",
        "Host": "wsjkw.sc.gov.cn",
        "Referer": f"https://wsjkw.sc.gov.cn/guestweb4/s?searchWord=%25E6%25B3%2595%25E5%25AE%259A%25E4%25BC%25A0%25E6%259F%2593%25E7%2597%2585&column=%25E5%2585%25A8%25E9%2583%25A8&wordPlace=1&orderBy=1&startTime=&endTime=&pageSize=10&pageNum={i-1}&timeStamp=0&siteCode=5100000053&siteCodes=&checkHandle=1&strFileType=%25E5%2585%25A8%25E9%2583%25A8%25E6%25A0%25BC%25E5%25BC%258F&sonSiteCode=&areaSearchFlag=1&secondSearchWords=&countKey=%200&left_right_index=0",
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

# Send a POST request to the specified URL with headers
response = requests.post(url, headers=headers)

# Parse the response content using BeautifulSoup
soup = BeautifulSoup(response.content, "html.parser")

# Find all <a> tags with target='_blank'
links_with_blank_target = soup.find_all('a', {'target': '_blank'})

# Iterate through the links with target='_blank' and extract Chinese text containing '传染病'
for link in links_with_blank_target:
    chinese_text = link.text.strip()
    if '传染病' in chinese_text:
        url_link = link.get('href')
        data.append([chinese_text, url_link, url])

# This code block is creating a pandas DataFrame (`df`) from the `data` list. The DataFrame has three
# columns: '中文解释' (Chinese explanation), '链接' (link), and 'Referer'.
df = pd.DataFrame(data, columns=['中文解释', '链接','Referer'])
df = df[df['中文解释'].str.contains('月')]
df['年份'],df['月份'] = zip(*df['中文解释'].apply(lambda x: get_year_month(x)))
df['年份'] = df['年份'].str.replace(r'\D', '', regex=True)
df.replace('\xa0', '', regex=True, inplace=True)
df.replace('\u3000', '', regex=True, inplace=True)
df.replace('\u2002', '', regex=True, inplace=True)
df.replace(' ', '', regex=True, inplace=True)
df = df.drop_duplicates(subset=['年份', '月份'], keep='first')
df.to_csv(f'./data/province/{name}/{name}_url.csv', index=False, encoding='gbk')

#Iterate over the files in each link and save them in csv format
for i in range(len(df)):
        url = df.iloc[i]['链接']
        headers = {
            "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
            "Accept-Encoding": "gzip, deflate, br",
            "Accept-Language": "zh-CN,zh;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6",
            "Cache-Control": "max-age=0",
            "Connection": "keep-alive",
            "Cookie": "yfx_c_g_u_id_10000024=_ck24011521102910517165021452751; yfx_f_l_v_t_10000024=f_t_1705324229031__r_t_1705543071316__v_t_1705551675990__r_c_1; hits=%5B%7B%22title%22%3A%222023%E5%B9%B43%E6%9C%88%E5%9B%9B%E5%B7%9D%E7%9C%81%E6%B3%95%E5%AE%9A%E4%BC%A0%E6%9F%93%E7%97%85%E7%96%AB%E6%83%85%E6%A6%82%E5%86%B5%22%2C%20%22URL%22%3A%22http%3A%2F%2Fwsjkw.sc.gov.cn%2Fscwsjkw%2Frdts%2F2023%2F4%2F24%2F3aa144a52af74e9590c9f8c2948b173d.shtml%22%2C%22linkLocation%22%3A%2207%22%7D%2C%7B%22title%22%3A%222023%E5%B9%B410%E6%9C%88%E5%9B%9B%E5%B7%9D%E7%9C%81%E6%B3%95%E5%AE%9A%E4%BC%A0%E6%9F%93%E7%97%85%E7%96%AB%E6%83%85%E6%A6%82%E5%86%B5%22%2C%20%22URL%22%3A%22http%3A%2F%2Fwsjkw.sc.gov.cn%2Fscwsjkw%2Frdts%2F2023%2F11%2F13%2Fc025e90108b143e5aaf3957a523efb66.shtml%22%2C%22linkLocation%22%3A%2200%22%7D%5D",
            "Host": "wsjkw.sc.gov.cn",
            "Sec-Fetch-Dest": "document",
            "Sec-Fetch-Mode": "navigate",
            "Sec-Fetch-Site": "same-origin",
            "Sec-Fetch-User": "?1",
            "Upgrade-Insecure-Requests": "1",
            "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36 Edg/122.0.0.0",
            "sec-ch-ua": "\"Chromium\";v=\"122\", \"Not(A:Brand\";v=\"24\", \"Microsoft Edge\";v=\"122\"",
            "sec-ch-ua-mobile": "?0",
            "sec-ch-ua-platform": "\"Windows\"",
        }

        sign=0
        response=requests.get(url, headers=headers)
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
                    url_link = unescape(link)
                    response = requests.get(url_link, headers=headers)
                    if response.status_code == 200:
                        with open(f'./data/province/{name}/{df["年份"].iloc[i]}-{df["月份"].iloc[i]}.doc', 'wb') as f:
                            f.write(response.content)
                    else:
                        print(f'{df["年份"].iloc[i]}-{df["月份"].iloc[i]}存在，下载失败，但是200')
                if 'xls' in str(link) and 'xlsx' not in str(link) :
                    sign=1
                    url_link = unescape(link)
                    response = requests.get(url_link, headers=headers)
                    if response.status_code == 200:
                        with open(f'./data/province/{name}/{df["年份"].iloc[i]}-{df["月份"].iloc[i]}.xls', 'wb') as f:
                            f.write(response.content)
                    else:
                        print(f'{df["年份"].iloc[i]}-{df["月份"].iloc[i]}存在，下载失败，但是200')
                if 'xlsx' in str(link) :
                    sign=1
                    url_link = unescape(link)
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

files = os.listdir(f'./data/province/{name}/')
# Create a dataframe from the files in the specified directory
a=process_files_combined(f'./data/province/{name}/')
# Replace any non-breaking spaces with empty strings
a.replace('\xa0', '', regex=True, inplace=True)
a.replace('\u3000', '', regex=True, inplace=True)
a.replace('\u2002', '', regex=True, inplace=True)
a.replace(' ', '', regex=True, inplace=True)
# Save the dataframe as a csv file
a.to_csv(f'./data/province/{name}/{name}.csv',index=False,encoding='gbk')
# Create a csv file containing the URLs of the files
update_url_column(f'./data/province/{name}/{name}.csv',f'./data/province/{name}/{name}_url.csv')

#四川省缺失:
# 2022-12不存在传染病信息
# 2021-10不存在传染病信息
# 2016-5不存在传染病信息
# 2016-4不存在传染病信息
# 2016-3不存在传染病信息
# 2016-2不存在传染病信息
# 2016-1不存在传染病信息
# 2015-12不存在传染病信息
# 2015-11不存在传染病信息
# 2015-10不存在传染病信息
# 2012-7不存在传染病信息
