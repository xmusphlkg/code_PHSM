from reportlab.lib.pagesizes import A4
from reportlab.platypus import SimpleDocTemplate, Paragraph, Image
from reportlab.lib.styles import getSampleStyleSheet
from reportlab.lib.units import inch, cm
from reportlab.platypus import PageBreak
import pandas as pd
from reportlab.lib import utils

# empty pdf file
pdf_filename = './outcome/appendix/Supplementary Appendix 1_3.pdf'
doc = SimpleDocTemplate(pdf_filename, pagesize=A4, leftMargin=20, rightMargin=20, topMargin=15, bottomMargin=20)
story = []

# setting style
styles = getSampleStyleSheet()
styles['Title'].fontName = 'Times-Bold'
styles['Heading1'].fontName = 'Times-Bold'
styles['Heading2'].fontName = 'Times-Bold'
styles['Heading3'].fontName = 'Times-Bold'
styles['BodyText'].fontName = 'Times-Roman'
styles['Normal'].fontName = 'Times-Roman'
styles['Normal'].fontSize = 14
styles['Normal'].leading = styles['Heading1'].leading

# add figure
img_path = f'./outcome/appendix/Supplementary Appendix 1_3.png'
img = utils.ImageReader(img_path)
image = Image(img_path, width=10*50, height=10*50)
story.append(image)

# add figure title
title_text = "Supplementary Fig. 49. Training and comparing variant time series models for rubella."
title = Paragraph(title_text, styles['Heading2'])
story.append(title)

# add figure content
title_content = """<b>(A)</b> The forecasted number of rubella cases in the China from 2020 to 2023 trained on 
2008-2019 data. <b>(B)</b> The forecasted number of rubella cases in the China from 2019 to 2023 trained on 
2008-2018 data. <b>(C)</b> The difference between the forecasted incidence and the observed incidence of rubella 
in the China from 2020 to 2023, based on the model trained on 2008-2018 data. <b>(D)</b> The adjusted incidence 
relative ratio (IRR) distribution of rubella during different period which split by October 2022. <b>(E)</b> 
The changes of adjusted IRR of rubella during different period."""

title = Paragraph(title_content, styles['Normal'])
story.append(title)
doc.build(story)


# merge pdf
import PyPDF2
def merge_pdf(paths, output):
    pdf_writer = PyPDF2.PdfWriter()

    for path in paths:
        pdf_reader = PyPDF2.PdfReader(path)
        for page in range(len(pdf_reader.pages)):
            pdf_writer.add_page(pdf_reader.pages[page])
            
    with open(output, 'wb') as out:
        pdf_writer.write(out)
       
# merge xlsx
import pandas as pd
def merge_xlsx(paths, output_file):
    with pd.ExcelWriter(output_file, engine='openpyxl') as writer:
        for file_path,i in zip(paths, range(len(paths))):
            sheet = pd.read_excel(file_path)
            sheet_name = f"Table {i+1}"
            sheet.to_excel(writer, sheet_name=sheet_name, index=False)

if __name__ == '__main__':
    paths_1 = [
        "./outcome/appendix/Supplementary Appendix 1_1.pdf",
        './outcome/appendix/Supplementary Appendix 1_2.pdf',
        './outcome/appendix/Supplementary Appendix 1_3.pdf'
        ]
    output_1 = './outcome/appendix/Supplementary Appendix 1.pdf'
    merge_pdf(paths_1, output_1)
    
    paths_2 = [
         "./outcome/appendix/Supplementary Appendix 2_1.xlsx",
         "./outcome/appendix/Supplementary Appendix 2_2.xlsx",
         "./outcome/appendix/Supplementary Appendix 2_3.xlsx",
         "./outcome/appendix/Supplementary Appendix 2_4.xlsx"
         ]
    output_2 = './outcome/appendix/Supplementary Appendix 2.xlsx'
    merge_xlsx(paths_2, output_2)


