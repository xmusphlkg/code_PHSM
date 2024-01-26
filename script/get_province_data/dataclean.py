import os
from html import unescape
from urllib.parse import unquote
import numpy as np
import xlrd
import pandas as pd
from bs4 import BeautifulSoup
from docx import Document
import requests


def remove_chinese(title):
    title_type = title.split('.')[-1]
    title = ''.join([x for x in title if x.isdigit() or x == '-'])
    return title, title_type


def filetype(title):
    title_type = title.split('.')[-1]
    return title_type


def remove_space(title):
    title = title.replace(' ', '')
    return title


def process_files_combined(directory):
    try:
        data_list = []
        datatype_list = (np.floating,np.integer,int, float)

        files = os.listdir(directory)

        for file in files:
            file_path = os.path.join(directory, file)
            file_size = os.path.getsize(file_path)
            print(file)

            if file_size > 100:
                file_type = filetype(file)
                if file_type == 'doc':
                    pass
                if file_type == 'docx':
                    text_content, table_content = read_docx(file_path)
                    df = pd.DataFrame(table_content)
                    for i in range(len(df)):
                        try:
                            df.iloc[i][1], df.iloc[i][2] = float(df.iloc[i][1]), float(df.iloc[i][2])
                        except:
                            pass
                        if isinstance(df.iloc[i][0], str) and isinstance(df.iloc[i][1], datatype_list) and isinstance(
                                df.iloc[i][2], datatype_list):
                            data_list.append(
                                [remove_space(df.iloc[i][0]), df.iloc[i][1], df.iloc[i][2], file.split('.')[0]])
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
                            df.iloc[i][1], df.iloc[i][2] = float(df.iloc[i][1]), float(df.iloc[i][2])
                        except:
                            pass
                        if isinstance(df.iloc[i][0], str) and isinstance(df.iloc[i][1], datatype_list) and isinstance(
                                df.iloc[i][2], datatype_list):
                            data_list.append(
                                [remove_space(df.iloc[i][0]), df.iloc[i][1], df.iloc[i][2], file.split('.')[0]])
                elif file_type == 'csv':
                    df = pd.read_csv(file_path, encoding='gbk')
                    for i in range(len(df)):
                        if str(df.iloc[0, 0]) == '0':
                            try:
                                df.iloc[i, 2] = float(df.iloc[i, 2])
                                df.iloc[i, 3] = float(df.iloc[i, 3])
                            except:
                                pass
                            if isinstance(df.iloc[i][1], str) and isinstance(df.iloc[i][2], datatype_list) and isinstance(
                                    df.iloc[i][3], datatype_list):
                                data_list.append(
                                    [remove_space(df.iloc[i][1]), df.iloc[i][2], df.iloc[i][3], file.split('.')[0]])
                        else:
                            try:
                                df.iloc[i, 1] = float(df.iloc[i, 1])
                                df.iloc[i, 2] = float(df.iloc[i, 2])
                            except:
                                pass
                            try:
                                if isinstance(df.iloc[i][0], str) and isinstance(df.iloc[i,1],datatype_list) and isinstance(
                                        df.iloc[i][2], datatype_list):
                                    data_list.append(
                                        [remove_space(df.iloc[i][0]), df.iloc[i][1], df.iloc[i][2], file.split('.')[0]])
                            except:
                                pass
                elif file_type == 'xlsx':
                    sheet = pd.read_excel(file_path)
                    df = sheet

                    for i in range(len(df)):
                        try:
                            df.iloc[i][1], df.iloc[i][2] = float(df.iloc[i][1]), float(df.iloc[i][2])
                        except:
                            pass
                        if isinstance(df.iloc[i][0], str) and isinstance(df.iloc[i][1], datatype_list) and isinstance(
                                df.iloc[i][2], datatype_list):
                            data_list.append(
                                [remove_space(df.iloc[i][0]), df.iloc[i][1], df.iloc[i][2], file.split('.')[0]])
    except:
        pass

    result_df = pd.DataFrame(data_list, columns=['疾病病种', '发病数', '死亡数', 'date'])
    return result_df


def read_docx(file_path):
    doc = Document(file_path)
    text_content = []
    for paragraph in doc.paragraphs:
        text_content.append(paragraph.text)
    table_content = []
    for table in doc.tables:
        for row in table.rows:
            row_content = [cell.text for cell in row.cells]
            table_content.append(row_content)
    return text_content, table_content


def get_year_month(title):
    year = title.split('布')[1].split('年')[0]
    month = title.split('年')[1].split('月')[0]
    return year, month


def download_and_parse_data(df_row, headers):
    url = df_row['链接']
    sign = 0

    response = requests.get(url, headers=headers)
    soup = BeautifulSoup(response.content, 'html.parser', from_encoding='gbk')
    links_with_blank_target = soup.find_all('a', {'target': '_blank'})
    for link_tag in links_with_blank_target:
        link = link_tag.get('href')
        if link.endswith('docx'):
            sign = 1
            url_link = unescape(str(url) + link)
            response = requests.get(url_link, headers=headers)
            if response.status_code == 200:
                with open(f'./data/province/anhui/{df_row["年份"]}-{df_row["月份"]}.docx', 'wb') as f:
                    f.write(response.content)
            else:
                print(f'{df_row["年份"]}-{df_row["月份"]}存在，下载失败，但是200')

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
        table_df.to_csv(f'./data/province/anhui/{df_row["年份"]}-{df_row["月份"]}.csv', encoding='gbk')
        sign = 1
    except Exception as e:
        print(f"Error processing table data: {e}")

    if sign == 0:
        print(f'{df_row["年份"]}-{df_row["月份"]}不存在传染病信息')


def scrape_and_save_table(soup, df, i):
    table = soup.find('tbody')
    rows = table.find_all('tr')
    table_data = []

    for row in rows:
        cells = row.find_all(['td', 'th'])
        row_data = [cell.text.strip() for cell in cells]
        table_data.append(row_data)

    table_df = pd.DataFrame(table_data)
    table_df.replace('\xa0', '', regex=True, inplace=True)

    file_path = f'./data/province/anhui/{df["年份"].iloc[i]}-{df["月份"].iloc[i]}.csv'
    table_df.to_csv(file_path, encoding='gbk')


# 使用方式：
# scrape_and_save_table(soup, df, i)

def update_url_column(df_csv_path, url_csv_path):
    try:
        url_df = pd.read_csv(url_csv_path, encoding='gbk')
    except:
        url_df = pd.read_csv(url_csv_path)
    df = pd.read_csv(df_csv_path, encoding='gbk')
    try:
        df['date_1'] = pd.to_datetime(df['date'],format='%b-%y')
    except:
        df['date_1'] = pd.to_datetime(df['date'])
    df['year'] = df['date_1'].dt.year.astype(str)
    df['month'] = df['date_1'].dt.month.astype(str)
    df['url'] = None

    for i in range(len(url_df)):
        try:
            url = unquote(url_df.loc[i, '链接'])
        except:
            url = unquote(url_df.loc[i, 'url'])
        try:
            url_year = int(url_df['年份'].iloc[i])
        except:
            url_year = int(url_df['年'].iloc[i])
        try:
            url_month = int(url_df['月份'].iloc[i])
        except:
            url_month = int(url_df['月'].iloc[i])

        for j in range(len(df)):
            if str(df['year'].iloc[j]) == str(url_year) and str(df['month'].iloc[j]) == str(url_month):
                df['url'].iloc[j] = url
    df.drop('date_1', axis=1, inplace=True)
    df.to_csv(df_csv_path, encoding='gbk', index=False)

def motify_date(df,df_csv_path):
    df_1=df
    df_1['date'] = pd.to_datetime(df['date'], format='%b-%y', errors='coerce').dt.strftime('20%y-%m')
    df_1=df_1.dropna(axis=0)
    df_2=df
    df_2['date'] = pd.to_datetime(df['date'], format='%y-%b', errors='coerce').dt.strftime('20%y-%m')
    df_2=df_2.dropna(axis=0)
    df=pd.concat([df_1,df_2])
    df["年"]=df["date"].str.split("-",expand=True)[0]
    df["月"]=df["date"].str.split("-",expand=True)[1]
    df.to_csv(df_csv_path, encoding='gbk', index=False)

def find_missing_elements(df_1,i, df_2,m):
    str_list = df_1[~df_1[i].isin(df_2[m])][i].tolist()
    return str_list