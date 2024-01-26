import os
from html import unescape
import pandas as pd
import requests
from bs4 import BeautifulSoup
from liu_script.dataclean import process_files_combined, update_url_column
data = []
for i in range(1, 12):
    if i==1:
        url = "https://wsjkw.henan.gov.cn/zfxxgk/yqxx/index.html"
    else:
        url=f"https://wsjkw.henan.gov.cn/zfxxgk/yqxx/index_{i-1}.html"
    headers = {
        'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7',
        'Accept-Encoding': 'gzip, deflate, br',
        'Accept-Language': 'zh-CN,zh;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6',
        'Cache-Control': 'max-age=0',
        'Connection': 'keep-alive',
        'Host': 'wsjkw.henan.gov.cn',
        'Referer': 'https://wsjkw.henan.gov.cn/zfxxgk/yqxx/index_10.html',
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
    cookies = {
        'yfx_c_g_u_id_10000037': '_ck24011521085519955359597979714',
        'yfx_f_l_v_t_10000037': 'f_t_1705324135994__r_t_1705496882016__v_t_1705496882016__r_c_1'
    }
    response = requests.get(url, headers=headers,cookies=cookies)
    soup = BeautifulSoup(response.content, "html.parser")
    links_with_blank_target = soup.find_all('a', {'target': '_blank'})
    for link in links_with_blank_target:
        chinese_text = link.text.strip()
        if '传染病' in chinese_text:
            url_link = link.get('href')
            data.append([chinese_text, url_link])
def get_year_month(title):
    year = title.split('年')[0]
    month = title.split('年')[1].split('月')[0]
    return year, month
df = pd.DataFrame(data, columns=['中文解释', '链接'])
df = df[df['中文解释'].str.contains('月')]
df['年份'],df['月份'] = zip(*df['中文解释'].apply(lambda x: get_year_month(x)))
df['年份'] = df['年份'].str.replace(r'\D', '', regex=True)
df.to_csv('./data/province/henan/henan_url.csv', index=False, encoding='gbk')


for i in range(len(df)):
        url = df.iloc[i]['链接']
        origin_url=url.split('/t')[0]
        headers = {
            'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7',
            'Accept-Encoding': 'gzip, deflate, br',
            'Accept-Language': 'zh-CN,zh;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6',
            'Cache-Control': 'max-age=0',
            'Connection': 'keep-alive',
            'Cookie': 'yfx_c_g_u_id_10000037=_ck24011521085519955359597979714; yfx_f_l_v_t_10000037=f_t_1705324135994__r_t_1705496882016__v_t_1705496882016__r_c_1; yfx_c_g_u_id_10000042=_ck24011721443111881054130175162; yfx_f_l_v_t_10000042=f_t_1705499071178__r_t_1705499071178__v_t_1705499071178__r_c_0',
            'Host': 'wsjkw.henan.gov.cn',
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
                    with open(f'./data/province/henan/{df["年份"].iloc[i]}-{df["月份"].iloc[i]}.docx', 'wb') as f:
                        f.write(response.content)
                else:
                    print(f'{df["年份"].iloc[i]}-{df["月份"].iloc[i]}存在，下载失败，但是200')
            if 'doc' in str(link) :
                sign=1
                url_link = unescape(str(origin_url) + link[1:])
                response = requests.get(url_link, headers=headers)
                if response.status_code == 200:
                    with open(f'./data/province/henan/{df["年份"].iloc[i]}-{df["月份"].iloc[i]}.doc', 'wb') as f:
                        f.write(response.content)
                else:
                    print(f'{df["年份"].iloc[i]}-{df["月份"].iloc[i]}存在，下载失败，但是200')
        try:
            table_all = soup.find_all('table')
            for j in table_all:
                if '病名' in j.text:
                    table=j
            rows = table.find_all('tr')
            table_data = []
            for row in rows:
                cells = row.find_all(['td'])
                row_data = [cell.text.strip() for cell in cells]
                table_data.append(row_data)
            table_df = pd.DataFrame(table_data)
            table_df.replace('\xa0', '', regex=True, inplace=True)
            table_df.to_csv(f'./data/province/henan/{df["年份"].iloc[i]}-{df["月份"].iloc[i]}.csv',encoding='gbk',index=False)
            sign = 1
        except:
            pass
        if sign==0:
            print(f'{df["年份"].iloc[i]}-{df["月份"].iloc[i]}不存在传染病信息')

files = os.listdir('./data/province/henan/')
a=process_files_combined('./data/province/henan/')
a.to_csv('./data/province/henan/henan.csv',index=False,encoding='gbk')
update_url_column('./data/province/henan/henan.csv','./data/province/henan/henan_url.csv')