import os
import pandas as pd

# Define Directory path
dir_path = './data/data_check_province/second_check'

# Iterate over all files in the directory
for filename in os.listdir(dir_path):
    if filename.endswith('.xlsx'):
        df = pd.read_excel(os.path.join(dir_path, filename))
        duplicates = df.duplicated(subset=['year', 'month', 'disease_cn'], keep=False)
        if duplicates.all():
            df_duplicates = df[duplicates]
            print(filename, df_duplicates)
        df.to_csv(os.path.join(dir_path, filename[:-5] + '.csv'), index=False,encoding='gbk')