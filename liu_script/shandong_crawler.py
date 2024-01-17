import os
from html import unescape
import pandas as pd
import requests
from bs4 import BeautifulSoup
from doc2docx import convert
import time
from liu_script.dataclean import process_files_combined, update_url_column

data = []
origin_url = "http://wsjkw.shandong.gov.cn/zwgk/zdmsgysy/fdcrb"
for i in range(1, 7):
    if i==1:
        url = "http://wsjkw.shandong.gov.cn/zwgk/zdmsgysy/fdcrb/index.html"
    else:
        url=f"http://wsjkw.shandong.gov.cn/zwgk/zdmsgysy/fdcrb/index_{i-1}.html"
    headers = {
        "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
        "Accept-Encoding": "gzip, deflate",
        "Accept-Language": "zh-CN,zh;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6",
        "Cache-Control": "max-age=0",
        "Connection": "keep-alive",
        "Cookie": "yuYin=0; td_cookie=419399912; yuYin=0; TrsAccessMonitor=TrsAccessMonitor-1705322287000-1689653460",
        "Host": "wsjkw.shandong.gov.cn",
        "If-Modified-Since": "Wed, 27 Dec 2023 08:16:37 GMT",
        "If-None-Match": "\"658bdd65-3cce\"",
        "Upgrade-Insecure-Requests": "1",
        "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36 Edg/122.0.0.0"
    }
    response = requests.get(url, headers=headers)
    soup = BeautifulSoup(response.content, "html.parser")
    links_with_blank_target = soup.find_all('span', {'class': 'newstxt'})
    for link in links_with_blank_target:
        chinese_text = link.text.strip()
        url_link = link.find('a')['href']
        data.append([chinese_text, origin_url+url_link[1:]])
def get_year_month(title):
    year = title.split('省')[1].split('年')[0]
    month = title.split('年')[1].split('月')[0]
    return year, month

df = pd.DataFrame(data, columns=['中文解释', '链接'])
df.replace('.\xa0','', inplace=True, regex=True)
df['年份'],df['月份'] = zip(*df['中文解释'].apply(lambda x: get_year_month(x)))
df.to_csv('./data/province/shandong/shandong_url.csv', index=False, encoding='gbk')

for i in range(len(df)):
    url = df.iloc[i]['链接']
    origin_url=url.split('/t')[0]
    headers = {
        "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
        "Accept-Encoding": "gzip, deflate",
        "Accept-Language": "zh-CN,zh;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6",
        "Cache-Control": "max-age=0",
        "Connection": "keep-alive",
        "Cookie": "td_cookie=586592693; yuYin=0; yuYin=0; td_cookie=586112178; yuYin=0; TrsAccessMonitor=TrsAccessMonitor-1705322287000-1689653460",
        "Host": "wsjkw.shandong.gov.cn",
        "Upgrade-Insecure-Requests": "1",
        "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36 Edg/122.0.0.0"
    }
    sign=0
    response=requests.get(url, headers=headers)
    soup=BeautifulSoup(response.content,'html.parser',from_encoding='gbk')
    links_with_blank_target = soup.find_all({'ul'})
    for url in links_with_blank_target:
        link=url.find('a')['href']
        if 'docx' in str(link):
            sign=1
            url_link = unescape(str(origin_url) + link[1:])
            response = requests.get(url_link, headers=headers)
            if response.status_code == 200:
                with open(f'./data/province/shandong/{df["年份"].iloc[i]}-{df["月份"].iloc[i]}.docx', 'wb') as f:
                    f.write(response.content)
            else:
                print(f'{df["年份"].iloc[i]}-{df["月份"].iloc[i]}存在，下载失败，但是200')
        if 'doc' in str(link) :
            sign=1
            url_link = unescape(str(origin_url) + link[1:])
            response = requests.get(url_link, headers=headers)
            if response.status_code == 200:
                with open(f'./data/province/shandong/{df["年份"].iloc[i]}-{df["月份"].iloc[i]}.doc', 'wb') as f:
                    f.write(response.content)
            else:
                print(f'{df["年份"].iloc[i]}-{df["月份"].iloc[i]}存在，下载失败，但是200')
    try:
        table = soup.find('tbody')
        rows = table.find_all('tr')
        table_data = []
        for row in rows:
            cells = row.find_all(['td', 'th'])
            row_data = [cell.text.strip() for cell in cells]
            table_data.append(row_data)
        table_df = pd.DataFrame(table_data)
        table_df.replace('\xa0', '', regex=True, inplace=True)
        table_df.to_csv(f'./data/province/shandong/{df["年份"].iloc[i]}-{df["月份"].iloc[i]}.csv',encoding='gbk')
        sign = 1
    except:
        pass
    if sign==0:
        print(f'{df["年份"].iloc[i]}-{df["月份"].iloc[i]}不存在传染病信息')
#山东省缺失
# 2016-12不存在传染病信息
# 2016-11不存在传染病信息
# 2016-10不存在传染病信息
# 2016-9不存在传染病信息
# 2016-8不存在传染病信息
# 2016-7不存在传染病信息
# 2016-6不存在传染病信息
# 2016-5不存在传染病信息
# 2016-4不存在传染病信息
# 2016-3不存在传染病信息
# 2016-2不存在传染病信息
# 2016-1不存在传染病信息
# 2015-12不存在传染病信息
# 2015-11不存在传染病信息
# 2015-10不存在传染病信息
# 2015-9不存在传染病信息
# 2015-8不存在传染病信息
# 2015-7不存在传染病信息
# 2015-5不存在传染病信息
# 2015-4不存在传染病信息
# 2015-3不存在传染病信息
# 2015-2不存在传染病信息
# 2014-12不存在传染病信息
# 2014-11不存在传染病信息
# 2014-10不存在传染病信息
# 2014-9不存在传染病信息
# 2014-8不存在传染病信息
# 2014-7不存在传染病信息
# 2014-6不存在传染病信息

files = os.listdir('./data/province/shandong/')

for file in files:
    if str(file)+"x" in files:
        os.remove(f'./data/province/shandong/{file}')
    try:
        if '.doc' in file and '.docx' not in file:
            convert(f'D:/github/code_PHSM/data/province/shandong/{file}')
    except:
        pass


a=process_files_combined('./data/province/shandong/')
a.to_csv('./data/province/shandong/shandong.csv',index=False,encoding='gbk')
update_url_column('./data/province/shandong/shandong.csv','./data/province/shandong/shandong_url.csv')