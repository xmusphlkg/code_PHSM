import os
from html import unescape
import pandas as pd
import requests
import xlrd
from bs4 import BeautifulSoup
from liu_script.dataclean import update_url_column, read_docx, remove_space, filetype, process_files_combined
origin_url='https://wjw.xinjiang.gov.cn/'
data = []
name='xinjiang'
for i in range(1, 30):
    if i==1:
        url = "https://wjw.xinjiang.gov.cn/hfpc/jbjcypj/nav_list.shtml"
    else:
        url=f"https://wjw.xinjiang.gov.cn/hfpc/jbjcypj/nav_list_{i}.shtml"
    headers = {
        'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7',
        'Accept-Encoding': 'gzip, deflate, br',
        'Accept-Language': 'zh-CN,zh;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6',
        'Cache-Control': 'max-age=0',
        'Connection': 'keep-alive',
        'Cookie': 'yfx_c_g_u_id_10000001=_ck24011521051316517617191981134; yfx_f_l_v_t_10000001=f_t_1705323913657__r_t_1705543043952__v_t_1705545533461__r_c_1',
        'Host': 'wjw.xinjiang.gov.cn',
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

    response = requests.get(url, headers=headers)
    soup = BeautifulSoup(response.content, "html.parser")
    links_with_blank_target = soup.find_all('a', {'target': '_blank'})
    for link in links_with_blank_target:
        chinese_text = link.text.strip()
        if '传染病' in chinese_text:
            url_link = origin_url+link.get('href')
            data.append([chinese_text, url_link,url])
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
        headers = {
            'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7',
            'Accept-Encoding': 'gzip, deflate, br',
            'Accept-Language': 'zh-CN,zh;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6',
            'Cache-Control': 'max-age=0',
            'Connection': 'keep-alive',
            'Cookie': 'yfx_c_g_u_id_10000001=_ck24011521051316517617191981134; yfx_f_l_v_t_10000001=f_t_1705323913657__r_t_1705543043952__v_t_1705545533461__r_c_1',
            'Host': 'wjw.xinjiang.gov.cn',
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
        response=requests.get(url[1:], headers=headers)
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
                    url_link = unescape(str(origin_url) + link[1:])
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


files = os.listdir(f'./data/province/{name}/')
a=process_files_combined(f'./data/province/{name}/')
a.replace('\xa0', '', regex=True, inplace=True)
a.replace('\u3000', '', regex=True, inplace=True)
a.replace('\u2002', '', regex=True, inplace=True)
a.replace(' ', '', regex=True, inplace=True)
a.to_csv(f'./data/province/{name}/{name}.csv',index=False,encoding='gbk')
update_url_column(f'./data/province/{name}/{name}.csv',f'./data/province/{name}/{name}_url.csv')