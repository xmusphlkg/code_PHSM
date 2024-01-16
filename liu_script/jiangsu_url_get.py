import pandas as pd

from urllib.parse import unquote,quote

url_df=pd.read_csv('./data/province/jiangsu/jiangsu_url.csv',encoding='gbk')
jiangsu=pd.read_csv('./data/province/jiangsu/jiangsu.csv',encoding='gbk')
jiangsu['date_1'] = pd.to_datetime(jiangsu['date'])
jiangsu['year'] = jiangsu['date_1'].dt.year.astype(str)
jiangsu['month']=jiangsu['date_1'].dt.month.astype(str)
jiangsu['url']=None

for i in range(len(url_df)):
    url=url_df.loc[i,'链接']
    url=unquote(url)
    url_year=url_df['年份'].iloc[i]
    url_month=url_df['月份'].iloc[i]
    for j in range(len(jiangsu)):
        if str(jiangsu['year'].iloc[j])==str(url_year) and str(jiangsu['month'].iloc[j])==str(url_month):
            jiangsu['url'].iloc[j]=url

jiangsu.drop('date_1',axis=1,inplace=True)
jiangsu.to_csv('./data/province/jiangsu/jiangsu.csv',encoding='gbk',index=False)
