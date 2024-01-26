import os
import pandas as pd
import xlrd

from liu_script.dataclean import update_url_column, motify_date

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

# #重命名文件
# files = os.listdir('./data/province/zhejiang')
# for file in files:
#     file_new,file_type= remove_chinese(file)
#     os.rename('./zhejiang/'+file, './zhejiang/'+file_new+'.'+file_type)
#读取文件
files = os.listdir('./data/province/zhejiang')
df_template=pd.DataFrame(columns=['疾病病种', '发病数', '死亡数', 'date'])
dict=[]
for file in files:
    file_size=os.path.getsize('./data/province/zhejiang/'+file)
    if file_size>1000:
        file_type= filetype(file)
        if file_type=='xls':
            sheet = xlrd.open_workbook('./data/province/zhejiang/'+file).sheet_by_index(0)
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
                if isinstance(df.iloc[i][0], str) and isinstance(df.iloc[i][1], (int, float)) and isinstance(df.iloc[i][2], (int, float)):
                    dict.append([remove_space(df.iloc[i][0]), df.iloc[i][1], df.iloc[i][2], file.split('.')[0]])
                else:
                    pass
        else:
            pass
    else:
        pass
df = pd.DataFrame(dict, columns=['疾病病种', '发病数', '死亡数', 'date'])
df.to_csv('./data/province/zhejiang/zhejiang.csv',encoding='gbk')
update_url_column(f'./data/province/{name}/{name}.csv',f'./data/province/{name}/{name}_url.csv')

# file=pd.read_csv('./data/province/zhejiang/zhejiang.csv')
# file.to_csv('./data/province/zhejiang/zhejiang.csv', index=False, encoding='gbk')



