

import requests
import json
import re
import pandas as pd
from bs4 import BeautifulSoup


## get the data from the API
format_data = "col=1&appid=1&webid=1855&path=%2F&columnid=1229123469&sourceContentType=1&unitid=5939785&webname=%E6%B5%99%E6%B1%9F%E7%9C%81%E5%8D%AB%E7%94%9F%E5%81%A5%E5%BA%B7%E5%A7%94%E5%91%98%E4%BC%9A&permissiontype=0"
url = "https://wsjkw.zj.gov.cn/module/jpage/dataproxy.jsp?startrecord=1&endrecord=160&perpage=160"
headers = {
    'Content-Type': 'application/x-www-form-urlencoded; charset=UTF-8',
    'Origin': 'https://wsjkw.zj.gov.cn',
    'Referer': 'https://wsjkw.zj.gov.cn/'
}
response = requests.post(url, data=format_data, headers=headers)

## get year and month from the title using 年 月
def get_year_month(title):
    matches = re.findall(r'(\d{4})年(\d{1,2})月', title)
    if matches:
        year, month = matches[0]
        return year, month
    else:
        return None, None

# define a function to get table data from URLs
def get_table_data(url, base_url, date):
    response = requests.get(url)
    if response.status_code != 200:
        print(url)
        raise Exception("Failed to fetch web content, status code: {}".format(response.status_code))

    # Use pandas to directly parse the table data
    response.encoding = 'utf-8'
    text = response.text
    soup = BeautifulSoup(text, 'html.parser')
    # if no 3rd table
    if len(soup.find_all('table')) < 4:
        # find download link which text contains '统计表'
        download_link = soup.find('a', string=lambda x: x and '统计表' in x)
        if download_link:
            download_url = download_link['href']
            # add domain name to the url
            if download_url.startswith('/'):
                download_url = base_url + download_url
            # download the file to local
            r = requests.get(download_url)
            filename = date + '.' + download_url.split('.')[-1]
            # if file is empty, return None
            if len(r.content) < 100:
                print("No table found{}, url: {}".format(date, url))
                print("Skip this url")
                return None, 'None'
            with open('zhejiang/' + filename, 'wb') as f:
                f.write(r.content)
            return None, 'Downloaded'
        else:
            print("No table found{}, url: {}".format(date, url))
            print("Skip this url")
            return None, 'None'
    tables = soup.find_all('table')[3]

    data = []
    thead = tables.find('thead')
    if thead:
        thead_rows = thead.find_all('tr')
        for tr in thead_rows:
            data.append([th.get_text().strip() for th in tr.find_all(['td', 'th']) if th.get_text().strip()])

    table_body = tables.find('tbody')
    if table_body:
        rows = table_body.find_all('tr')
        for tr in rows:
            cells = tr.find_all('td')
            if cells:
                # Filter out None or empty strings after stripping
                row = [td.get_text().strip() for td in cells if td.get_text() and td.get_text().strip()]
                if row:
                    data.append(row)

    # remove row length less than 2
    data = [row for row in data if len(row) > 2]
    table_data = pd.DataFrame(data)

    # first row is the header
    header = table_data.iloc[0]
    table_data = table_data[1:]
    table_data.columns = header

    return table_data, 'Table'

soup = BeautifulSoup(response.text, 'xml')
cdata_blocks = soup.find_all('record')
links = []

for cdata_block in cdata_blocks:
    cdata = cdata_block.text
    cdata_soup = BeautifulSoup(cdata, 'html.parser')
    title = cdata_soup.a['title']
    url = cdata_soup.a['href']
    # if contains 年月 then get the year and month
    if '年' not in title:
        continue
    if '月' not in title:
        continue
    year, month = get_year_month(title)
    date = year + '-' + month
    links.append({
        'title': title,
        'url': url,
        'date': date
    })

# get the table data from the links
table_data = []
for link in links:
    url = link['url']
    date = link['date']
    table,status = get_table_data(url, 'https://wsjkw.zj.gov.cn', date)
    # add status to links
    link['status'] = status
    if table is None:
        continue
    else:
        table['date'] = date
        table_data.append(table)
# combine all the table data
table_data = pd.concat(table_data)
table_data = table_data.reset_index(drop=True)
table_data.to_csv('zhejiang/zhejiang.csv', index=False)

# save links to csv
links = pd.DataFrame(links)
links.to_csv('zhejiang/links.csv', index=False)