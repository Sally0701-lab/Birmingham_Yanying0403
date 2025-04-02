import requests
import pandas as pd
import os

# read CSV 
df = pd.read_csv(r'C:\Users\wyy98\Desktop\birmingham\CCLW_document_data_download-2025-03-17\Document_Data_Download-2025-03-17.csv')

# output
output_dir = r'C:\Users\wyy98\Desktop\birmingham\downloading\downloaded_pdfs'
if not os.path.exists(output_dir):
    os.makedirs(output_dir)

# loop
for index, row in df.iterrows():
    # get URL
    url = row['Document Content URL'] 
    filename = row['Document Title']   
    safe_filename = ''.join(e for e in filename if e.isalnum() or e in (' ', '.')).rstrip()
    output_path = os.path.join(output_dir, f'{safe_filename}.pdf')

    # download pdf
    try:
        response = requests.get(url)
        if response.status_code == 200:
            with open(output_path, 'wb') as file:
                file.write(response.content)
            print(f"success {output_path}")
        else:
            print(f"failure {output_path}，status code：{response.status_code}")
    except Exception as e:
        print(f"download {output_path} has error：{e}")