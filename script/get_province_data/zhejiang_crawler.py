import os
import pandas as pd
import xlrd
import requests
import json
import re
import pandas as pd
from bs4 import BeautifulSoup
from dataclean import update_url_column, motify_date
name='zhejiang'

def remove_chinese(title):
    title_type=title.split('.')[-1]
    title = ''.join([x for x in title if x.isdigit() or x == '-'])
    return title, title_type
def filetype(title):
    title_type=title.split('.')[-1]
    return title_type
def remove_space(title):
    title = title.replace(' ', '')
    return title
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

files = os.listdir('./data/province/zhejiang')
#Loop through each file in the zhejiang directory
for file in files:
    #Remove any Chinese characters from the file name
    file_new,file_type= remove_chinese(file)
    #Rename the file with the new name and file type
    os.rename('./zhejiang/'+file, './zhejiang/'+file_new+'.'+file_type)
#Loop through each file in the zhejiang directory
files = os.listdir('./data/province/zhejiang')
#Create a dataframe to store the data
df_template=pd.DataFrame(columns=['疾病病种', '发病数', '死亡数', 'date'])
#Create an empty list to store the data
dict=[]
#Loop through each file in the zhejiang directory
for file in files:
    #Get the size of the file
    file_size=os.path.getsize('./data/province/zhejiang/'+file)
    #If the file size is greater than 1000 bytes
    if file_size>1000:
        #Get the file type
        file_type= filetype(file)
        #If the file type is an xls file
        if file_type=='xls':
            #Open the xls file
            sheet = xlrd.open_workbook('./data/province/zhejiang/'+file).sheet_by_index(0)
            #Get the number of rows and columns in the file
            num_rows = sheet.nrows
            num_cols = sheet.ncols
            #Create an empty list to store the data
            data = []
            #Loop through each row in the file
            for row_index in range(num_rows):
                #Create an empty list to store the row data
                row_data = []
                #Loop through each column in the row
                for col_index in range(num_cols):
                    #Get the value of the cell
                    cell_value = sheet.cell_value(row_index, col_index)
                    #Append the value to the row data list
                    row_data.append(cell_value)
                #Append the row data list to the data list
                data.append(row_data)
            #Create a dataframe from the data list
            df = pd.DataFrame(data)
            #Loop through each row in the dataframe
            for i in range(len(df)):
                #If the value of the first column is a string and the values of the second, third and fourth columns are integers or floats
                if isinstance(df.iloc[i][0], str) and isinstance(df.iloc[i][1], (int, float)) and isinstance(df.iloc[i][2], (int, float)):
                    #Append the values of the first, second, third and fourth columns to the list
                    dict.append([remove_space(df.iloc[i][0]), df.iloc[i][1], df.iloc[i][2], file.split('.')[0]])
                #Otherwise, pass
                else:
                    pass
        #If the file type is not an xls file, pass
        else:
            pass
    #Otherwise, pass
    else:
        pass
df = pd.DataFrame(dict, columns=['疾病病种', '发病数', '死亡数', 'date'])
df.to_csv('./data/province/zhejiang/zhejiang.csv',encoding='gbk')
update_url_column(f'./data/province/{name}/{name}.csv',f'./data/province/{name}/{name}_url.csv')

file=pd.read_csv('./data/province/zhejiang/zhejiang.csv')
file.to_csv('./data/province/zhejiang/zhejiang.csv', index=False, encoding='gbk')



