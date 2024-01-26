import os
from html import unescape
import pandas as pd
import requests
from bs4 import BeautifulSoup
from dataclean import process_files_combined, update_url_column

# The code is crawling data from the website of Chongqing Province Health Commission. It sends a GET
data = []
origin_url='https://wsjkw.cq.gov.cn/zwgk_242/wsjklymsxx/ylws_266434/jbfk_266438/yqxx'
for i in range(1, 6):
    if i==1:
        url = "https://wsjkw.cq.gov.cn/zwgk_242/wsjklymsxx/ylws_266434/jbfk_266438/yqxx/index.html"
    else:
        url=f"https://wsjkw.cq.gov.cn/zwgk_242/wsjklymsxx/ylws_266434/jbfk_266438/yqxx/index_{i-1}.html"
    headers = {
        "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
        "Accept-Encoding": "gzip, deflate, br",
        "Accept-Language": "zh-CN,zh;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6",
        "Cache-Control": "max-age=0",
        "Connection": "keep-alive",
        "Host": "wsjkw.cq.gov.cn",
        "Referer": url,
        "Sec-Fetch-Dest": "document",
        "Sec-Fetch-Mode": "navigate",
        "Sec-Fetch-Site": "same-origin",
        "Sec-Fetch-User": "?1",
        "Upgrade-Insecure-Requests": "1",
        "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36 Edg/122.0.0.0",
        "sec-ch-ua": '"Chromium";v="122", "Not(A:Brand";v="24", "Microsoft Edge";v="122"',
        "sec-ch-ua-mobile": "?0",
        "sec-ch-ua-platform": '"Windows"',
    }
    response = requests.get(url, headers=headers)
    soup = BeautifulSoup(response.content, "html.parser")
    links_with_blank_target = soup.find_all('a', {'target': '_blank'})
    for link in links_with_blank_target:
        chinese_text = link.text.strip()
        if '传染病' in chinese_text:
            url_link = link.get('href')
            data.append([chinese_text, origin_url+url_link[1:],url])

def get_year_month(title):
    """
    The function `get_year_month` extracts the year and month from a given title, and the code snippet
    processes a DataFrame to extract the year and month from the '中文解释' column and save the result to a
    CSV file.
    
    :param title: The `title` parameter is a string that represents the title of a data entry
    :return: The function `get_year_month` returns the year and month extracted from the given title.
    """
    year = title.split('年')[0]
    month = title.split('年')[1].split('月')[0]
    return year, month
df = pd.DataFrame(data, columns=['中文解释', '链接','Referer'])
df = df[df['中文解释'].str.contains('月')]
df['年份'],df['月份'] = zip(*df['中文解释'].apply(lambda x: get_year_month(x)))
df['年份'] = df['年份'].str.replace(r'\D', '', regex=True)
df.to_csv('./data/province/chongqing/chongqing_url.csv', index=False, encoding='gbk')

# The code block you provided is iterating over each row in the DataFrame `df` and performing the
# following tasks:
for i in range(len(df)):
        url = df.iloc[i]['链接']
        origin_url='https://wsjkw.cq.gov.cn/zwgk_242/wsjklymsxx/ylws_266434/jbfk_266438/yqxx'
        headers = {
            "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
            "Accept-Encoding": "gzip, deflate, br",
            "Accept-Language": "zh-CN,zh;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6",
            "Cache-Control": "max-age=0",
            "Connection": "keep-alive",
            "Cookie": "_trs_user=",
            "Host": "wsjkw.cq.gov.cn",
            "Referer": df.iloc[i]['Referer'],
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
        sign=0
        response=requests.get(url, headers=headers)
        soup=BeautifulSoup(response.content,'html.parser',from_encoding='gbk')
        links_with_blank_target = soup.find({'ul'})
        try:
            for url in links_with_blank_target:
                link=url.find('a')['href']
                if 'docx' in str(link):
                    sign=1
                    url_link = unescape(str(origin_url) + link[1:])
                    response = requests.get(url_link, headers=headers)
                    if response.status_code == 200:
                        with open(f'./data/province/chongqing/{df["年份"].iloc[i]}-{df["月份"].iloc[i]}.docx', 'wb') as f:
                            f.write(response.content)
                    else:
                        print(f'{df["年份"].iloc[i]}-{df["月份"].iloc[i]}存在，下载失败，但是200')
                if 'doc' in str(link) :
                    sign=1
                    url_link = unescape(str(origin_url) + link[1:])
                    response = requests.get(url_link, headers=headers)
                    if response.status_code == 200:
                        with open(f'./data/province/chongqing/{df["年份"].iloc[i]}-{df["月份"].iloc[i]}.doc', 'wb') as f:
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
            table_df.to_csv(f'./data/province/chongqing/{df["年份"].iloc[i]}-{df["月份"].iloc[i]}.csv',encoding='gbk',index=False)
            sign = 1
        except:
            pass
        if sign==0:
            print(f'{df["年份"].iloc[i]}-{df["月份"].iloc[i]}不存在传染病信息')

# The code block you provided is performing the following tasks:
files = os.listdir('./data/province/chongqing/')
a=process_files_combined('./data/province/chongqing/')
a.to_csv('./data/province/chongqing/chongqing.csv',index=False,encoding='gbk')
update_url_column('./data/province/chongqing/chongqing.csv','./data/province/chongqing/chongqing_url.csv')

#重庆市缺失：
# 2021-11不存在传染病信息
# 2021-10不存在传染病信息
# 2021-9不存在传染病信息
# 2021-8不存在传染病信息
# 2021-7不存在传染病信息
# 2021-6不存在传染病信息
# 2021-4不存在传染病信息
# 2021-3不存在传染病信息
# 2021-2不存在传染病信息
# 2021-1不存在传染病信息
# 2020-12不存在传染病信息
# 2019-2不存在传染病信息
# 2019-1不存在传染病信息
# 2018-12不存在传染病信息
# 2018-11不存在传染病信息
# 2018-10不存在传染病信息
# 2018-9不存在传染病信息
# 2018-8不存在传染病信息
# 2018-6不存在传染病信息
# 2018-7不存在传染病信息
# 2018-1不存在传染病信息
# 2018-2不存在传染病信息
# 2018-3不存在传染病信息
# 2018-4不存在传染病信息
# 2018-5不存在传染病信息