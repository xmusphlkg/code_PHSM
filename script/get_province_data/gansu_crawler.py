import os
import pandas as pd
from dataclean import process_files_combined, update_url_column

def process_gansu_data():
    """
    Process data for the province of Gansu.
    
    This function reads Excel files from the './data/province/gansu/' directory,
    cleans the data by removing special characters, and saves the cleaned data
    as CSV files. It also combines all the CSV files into a single DataFrame,
    further cleans the combined data, and saves it as a separate CSV file.
    Finally, it updates the URL column in the combined data CSV file.
    """
    name = 'gansu'
    
    # Process individual Excel files
    files = os.listdir(f'./data/province/{name}/')
    for file in files[:-1]:
        print(file)
        df = pd.read_excel(f'./data/province/{name}/{file}', header=None)
        df.replace('\xa0', '', regex=True, inplace=True)
        df.replace('\u3000', '', regex=True, inplace=True)
        df.replace('\u2002', '', regex=True, inplace=True)
        df.replace('\u2002', '', regex=True, inplace=True)
        df.replace(' ', '', regex=True, inplace=True)
        df.reset_index()
        df = df.iloc[1:]
        df.to_csv(f'./data/province/{name}/{file[:-10]}.csv', index=False, encoding='gbk')

    # Process combined data
    files = os.listdir(f'./data/province/{name}/')
    a = process_files_combined(f'./data/province/{name}/')
    a.replace('\xa0', '', regex=True, inplace=True)
    a.replace('\u3000', '', regex=True, inplace=True)
    a.replace('\u2002', '', regex=True, inplace=True)
    a.replace(' ', '', regex=True, inplace=True)
    a.to_csv(f'./data/province/{name}/{name}.csv', index=False, encoding='gbk')
    update_url_column(f'./data/province/{name}/{name}.csv', f'./data/province/{name}/{name}_url.csv')
