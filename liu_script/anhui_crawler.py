import os
import requests
import json
import pandas as pd
from bs4 import BeautifulSoup
from urllib.parse import quote
import re
from urllib.parse import unquote
from html import unescape
from liu_script.dataclean import remove_space,process_files_combined,update_url_column

data = []
for i in range(1, 7):
    url = "https://wjw.ah.gov.cn/site/label/8888"
    headers = {
        "Accept": "text/html, */*; q=0.01",
        "Accept-Encoding": "gzip, deflate, br",
        "Accept-Language": "zh-CN,zh;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6",
        "Connection": "keep-alive",
        "Cookie": "wzws_sessionid=gWQ5NGEyN4I4YWE4NWWgZaZ+74AxMTcuMjguMjUxLjE1OA==; wzaConfigTime=1705410361863; SHIROJSESSIONID=62d8ab3e-de74-4dfc-ba7f-52700fd22271",
        "Host": "wjw.ah.gov.cn",
        "Ls-Language": "zh",
        "Referer": "https://wjw.ah.gov.cn/public/column/7001?type=4&catId=6720591&action=list",
        "Sec-Fetch-Dest": "empty",
        "Sec-Fetch-Mode": "cors",
        "Sec-Fetch-Site": "same-origin",
        "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36 Edg/122.0.0.0",
        "X-Requested-With": "XMLHttpRequest",
        "sec-ch-ua": '"Chromium";v="122", "Not(A:Brand";v="24", "Microsoft Edge";v="122"',
        "sec-ch-ua-mobile": "?0",
        "sec-ch-ua-platform": '"Windows"',
    }

    params = {
        "_": "0.08646637127086532",
        "labelName": "publicInfoList",
        "siteId": "6788021",
        "pageSize": "15",
        "pageIndex": str(i),
        "action": "list",
        "fuzzySearch": "false",
        "fromCode": "title",
        "keyWords": "%E5%85%A8%E7%9C%81%E6%B3%95%E5%AE%9A%E6%8A%A5%E5%91%8A%E4%BC%A0%E6%9F%93%E7%97%85",
        "sortType": "1",
        "isDate": "true",
        "dateFormat": "yyyy-MM-dd",
        "length": "80",
        "organId": "7001",
        "type": "6",
        "catIds": "",
        "cId": "",
        "result": "%E6%9A%82%E6%97%A0%E7%9B%B8%E5%85%B3%E4%BF%A1%E6%81%AF",
        "file": "/xxgk/publicInfoList_newest2022_wjw",
    }

    response = requests.post(url, headers=headers, params=params)
    soup = BeautifulSoup(response.text, "html.parser")
    links_with_blank_target = soup.find_all('a', {'target': '_blank'})
    for link in links_with_blank_target:
        chinese_text = link.get_text(strip=True)
        url_link =link.get('href')
        data.append([chinese_text, url_link])


df = pd.DataFrame(data, columns=['中文解释', '链接'])
df = df[df['中文解释'].str.contains('月全省')]
df['年份'],df['月份'] = zip(*df['中文解释'].apply(lambda x: get_year_month(x)))
df.to_csv('./data/province/anhui/anhui_url.csv', index=False, encoding='gbk')

headers = {
    "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
    "Accept-Encoding": "gzip, deflate, br",
    "Accept-Language": "zh-CN,zh;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6",
    "Cache-Control": "max-age=0",
    "Connection": "keep-alive",
    "Cookie": "wzws_sessionid=gWQ5NGEyN4I4YWE4NWWgZaZ+74AxMTcuMjguMjUxLjE1OA==; SHIROJSESSIONID=62d8ab3e-de74-4dfc-ba7f-52700fd22271; wzaConfigTime=1705464974311",
    "Host": "wjw.ah.gov.cn",
    "Sec-Fetch-Dest": "document",
    "Sec-Fetch-Mode": "navigate",
    "Sec-Fetch-Site": "none",
    "Sec-Fetch-User": "?1",
    "Upgrade-Insecure-Requests": "1",
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36 Edg/122.0.0.0",
    "sec-ch-ua": '"Chromium";v="122", "Not(A:Brand";v="24", "Microsoft Edge";v="122"',
    "sec-ch-ua-mobile": "?0",
    "sec-ch-ua-platform": '"Windows"'
}
url_origin='https://wjw.ah.gov.cn/'
for i in range(len(df)):
    url=df['链接'].iloc[i]
    sign=0
    response=requests.get(url, headers=headers)
    soup=BeautifulSoup(response.content,'html.parser',from_encoding='gbk')
    #docx文件
    links_with_blank_target=soup.find_all('a',{'target':'_blank'})
    for url in links_with_blank_target:
        link=url.get('href')
        if link[-4:]==str('docx'):
            sign=1
            url_link = unescape(str(url_origin) + link)
            response = requests.get(url_link, headers=headers)
            if response.status_code == 200:
                with open(f'./data/province/anhui/{df["年份"].iloc[i]}-{df["月份"].iloc[i]}.docx', 'wb') as f:
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
        table_df.to_csv(f'./data/province/anhui/{df["年份"].iloc[i]}-{df["月份"].iloc[i]}.csv',encoding='gbk')
        sign = 1
    except:
        pass
    if sign==0:
        print(f'{df["年份"].iloc[i]}-{df["月份"].iloc[i]}不存在传染病信息')

#安徽省缺失：
# 2023-7不存在传染病信息
# 2019-3不存在传染病信息
# 2018-10不存在传染病信息

a=process_files_combined('./data/province/anhui/')
a.to_csv('./data/province/anhui/anhui.csv',index=False,encoding='gbk')
update_url_column('./data/province/anhui/anhui.csv','./data/province/anhui/anhui_url.csv')