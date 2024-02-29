from reportlab.lib.pagesizes import A4
from reportlab.platypus import SimpleDocTemplate, Paragraph, Image
from reportlab.lib.styles import getSampleStyleSheet
from reportlab.lib.units import inch, cm
from reportlab.platypus import PageBreak
import pandas as pd
from reportlab.lib import utils

# read data
nation_and_provinces_df = pd.read_excel('./data/nation_and_provinces.xlsx', sheet_name='Class')
fig1_data_df = pd.read_excel('./outcome/appendix/Figure Data/Fig.1 data.xlsx', sheet_name='panel A')

# empty pdf file
pdf_filename = './outcome/appendix/Supplementary Appendix 1_1.pdf'
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

# add space before title
story.append(Paragraph('<br/>' * 2, styles['Normal']))

# setting title
title_text = "Supplementary Appendix 1:"
title = Paragraph(title_text, styles['Title'])
story.append(title)

title_content = "Temporal trends and shifts of 24 notifiable infectious diseases in China before and after the COVID-19 epidemic"
title = Paragraph(title_content, styles['Title'])
story.append(title)
story.append(PageBreak())

# insert image and add TOC entries
for index, row in fig1_data_df.iterrows():
    # if index == 2:
    #     break
    disease_name = row['disease']
    label = nation_and_provinces_df[nation_and_provinces_df['diseasename'] == disease_name]['label'].values[0]

    # add figure
    img_path = f'./outcome/appendix/Supplementary Appendix 1_1/{disease_name}.png'
    img = utils.ImageReader(img_path)
    image = Image(img_path, width=560, height=600)
    story.append(image)

    # add figure title
    title_text = f"Supplementary Fig. {index + 1}. Temporal variation in the monthly incidence of {label} in China from January 2008 to December 2023."
    title = Paragraph(title_text, styles['Heading2'])
    story.append(title)

    # add figure content
    title_content = f"""<b>(A)</b> The spatial distribution of cases in China; 
    <b>(B)</b> Temporal variation in the monthly incidence bwtween different provinces. 
    The heatmap represents normalized monthly incidence data for each province, 
    with color intensity corresponding to the normalized monthly incidence. 
    * Normalized monthly incidence > 10."""
    title = Paragraph(title_content, styles['Normal'])
    story.append(title)

    # add page break
    story.append(PageBreak())

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
         './outcome/appendix/Supplementary Appendix 1_2.pdf'
         ]
    output_1 = './outcome/appendix/Supplementary Appendix 1.pdf'
    merge_pdf(paths_1, output_1)
    
    paths_2 = [
         "./outcome/appendix/Supplementary Appendix 2_1.xlsx",
         "./outcome/appendix/Supplementary Appendix 2_2.xlsx",
         "./outcome/appendix/Supplementary Appendix 2_3.xlsx"
         ]
    output_2 = './outcome/appendix/Supplementary Appendix 2.xlsx'
    merge_xlsx(paths_2, output_2)


