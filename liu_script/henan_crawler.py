import os
from html import unescape
import pandas as pd
import requests
from bs4 import BeautifulSoup
from doc2docx import convert
import time
from liu_script.dataclean import process_files_combined, update_url_column
data = []
for i in range(1, 12):
    if i==1:
        url = "https://wsjkw.henan.gov.cn/zfxxgk/yqxx/index.html"
    else:
        url=f"https://wsjkw.henan.gov.cn/zfxxgk/yqxx/index_{i-1}.html"
    headers = {
        "Accept": "image/avif,image/webp,image/apng,image/svg+xml,image/*,*/*;q=0.8",
        "Accept-Encoding": "gzip, deflate, br",
        "Accept-Language": "zh-CN,zh;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6",
        "Connection": "keep-alive",
        "Cookie": "yfx_c_g_u_id_10000037=_ck24011521085519955359597979714; yfx_sv_c_g_u_id=_ck24011521085519955359597979714; yfx_f_l_v_t_10000037=f_t_1705324135994__r_t_1705496882016__v_t_1705496882016__r_c_1",
        "Host": "js.henan.gov.cn",
        "Referer": "https://wsjkw.henan.gov.cn/",
        "Sec-Fetch-Dest": "image",
        "Sec-Fetch-Mode": "no-cors",
        "Sec-Fetch-Site": "same-site",
        "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36 Edg/122.0.0.0",
        "sec-ch-ua": '"Chromium";v="122", "Not(A:Brand";v="24", "Microsoft Edge";v="122"',
        "sec-ch-ua-mobile": "?0",
        "sec-ch-ua-platform": '"Windows"',
    }
    response = requests.post(url, headers=headers)
    soup = BeautifulSoup(response.content, "html.parser")
    links_with_blank_target = soup.find_all('a', {'target': '_blank'})

