import os
import xlrd
import pandas as pd
from docx import Document


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
    data_list = []

    files = os.listdir(directory)

    for file in files:
        file_path = os.path.join(directory, file)
        file_size = os.path.getsize(file_path)

        if file_size > 1000:
            file_type = filetype(file)

            if file_type == 'docx':
                text_content, table_content = read_docx(file_path)
                df = pd.DataFrame(table_content)
                for i in range(len(df)):
                    try:
                        df.iloc[i][1], df.iloc[i][2] = float(df.iloc[i][1]), float(df.iloc[i][2])
                    except:
                        pass
                    if isinstance(df.iloc[i][0], str) and isinstance(df.iloc[i][1], (int, float)) and isinstance(
                            df.iloc[i][2], (int, float)):
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
                    if isinstance(df.iloc[i][0], str) and isinstance(df.iloc[i][1], (int, float)) and isinstance(
                            df.iloc[i][2], (int, float)):
                        data_list.append(
                            [remove_space(df.iloc[i][0]), df.iloc[i][1], df.iloc[i][2], file.split('.')[0]])

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
