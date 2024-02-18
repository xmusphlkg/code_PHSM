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
    img_path = f'./outcome/appendix/Supplementary_1/{disease_name}.png'
    img = utils.ImageReader(img_path)
    image = Image(img_path, width=560, height=600)
    story.append(image)

    # add figure title
    title_text = f"Supplementary Fig. {index + 1}. Training and comparing variant time series models for {label}."
    title = Paragraph(title_text, styles['Heading2'])
    story.append(title)

    # add figure content
    title_content = """<b>(A)</b> Neural Network model; <b>(B)</b> Prophet model; <b>(C)</b> Exponential smoothing (ETS) model;
    <b>(D)</b> Seasonal autoregressive integrated moving average (SARIMA) model; <b>(E)</b> Hybrid models combining
    SARIMA, ETS, STL (seasonal and trend decomposition using loess), and neural network model;
    <b>(F)</b> Bayesian structural model; <b>(G)</b> Root mean square error (RMSE) of variant models;
    <b>(H)</b> Symmetric mean absolute percentage error (SMAPE) of variant models; <b>(I)</b> Mean absolute scaled error (MASE)
    of variant models; <b>(J)</b> R-squared of variant models."""
    title = Paragraph(title_content, styles['Normal'])
    story.append(title)

    # add page break
    story.append(PageBreak())

doc.build(story)
