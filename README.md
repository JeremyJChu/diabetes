# Working with Data about an Individual Type 1 Diabetic - Failing to Fit Linear Models and Lessons Learned
Looking at a singular type 1 diabetic with insulin resistance's data through 9 months, this project shows the successes and failures in trying to fit regression models onto the data. Linearity is ultimately difficult to find in diabetes data, especially when the data is sourced from an uncontrolled diabetic. This is less a study of successful predictions, but more an organized illustration of what chaotic data results in. 

The paper can be found [here](https://github.com/JeremyJChu/diabetes/blob/main/outputs/diabetes.pdf).

## The Data
The data is collected for an individual with their explicit consent. All personable identifiable data has been removed from the raw datasets prior to cleaning. A comprehensive datasource datasheet can be found [here](https://github.com/JeremyJChu/diabetes/blob/main/outputs/datasource-datasheet.pdf) based on the template describer [here](https://arxiv.org/abs/1803.09010v7). You are free to use the data as you see fit given proper credits. For further inquiries, please email jeremychuj@gmail.com.


## Packages
This project was done entirely with R through R Studio. Results were knitted into a pdf through R Markdown. Packages used:
- tidyverse
- here
- bookdown
- grid
- gridExtra
- gghighlight
- ggpubr
- kableExtra
- knitr
- magick

## Setup
R Markdown was setup using the following settings:
```
classoption: table
output: bookdown::pdf_document2
header-includes: 
  \usepackage{float}
  \usepackage{colortbl} 
  \arrayrulecolor{white}
  \usepackage{pdfpages}
```

## How to Use 
The project is entirely reproducible. All data downloading, cleaning, and transformation are performed and segmented in the [scripts](scripts) folder. 

I have also taken the liberty in saving all the raw and cleaned datasets and stored them [here](inputs/data). 

## Models
The paper runs simple linear and logistic regression models to varying degrees of success. A model card has been created for the model that exhibited the most success, based on the template [here](https://arxiv.org/pdf/1810.03993.pdf). It can be found [here](https://github.com/JeremyJChu/diabetes/blob/main/outputs/model-card.pdf).

## Changelog
New datasets will be updated annually. Please stay tuned here for when they are uploaded.

2021-04-23
- Pushed first edited version of study.

## Meta
Jeremy Chu - [@Jeremyjchu](https://twitter.com/Jeremyjchu) - [jeremychuj@gmail.com](jeremychuj@gmail.com)

Distributed under the MIT License. See `LICENSE` for more information. 

https://github.com/JeremyJChu

## Credits
[Rohan Alexander](https://rohanalexander.com/) for support. 




