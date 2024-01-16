import os
import pandas as pd
import xlrd


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

def process_files(directory):
    df_template = pd.DataFrame(columns=['疾病病种', '发病数', '死亡数', 'date'])
    data_list = []

    files = os.listdir(directory)

    for file in files:
        file_path = os.path.join(directory, file)
        file_size = os.path.getsize(file_path)

        if file_size > 1000:
            file_type = filetype(file)

            if file_type == 'xls':
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
                    if isinstance(df.iloc[i][0], str) and isinstance(df.iloc[i][1], (int, float)) and isinstance(
                            df.iloc[i][2], (int, float)):
                        data_list.append(
                            [remove_space(df.iloc[i][0]), df.iloc[i][1], df.iloc[i][2], file.split('.')[0]])

            else:
                pass

        else:
            pass

    result_df = pd.DataFrame(data_list, columns=['疾病病种', '发病数', '死亡数', 'date'])
    return result_df
