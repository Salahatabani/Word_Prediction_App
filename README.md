# Next Word Prediction
The is a simple web app (shinyapp) to predict the next word in a sentence. An example of application is the texting word prediction on your phone.
I developed this app for the [data science specialization](https://www.coursera.org/specializations/jhu-data-science?utm_source=gg&utm_medium=sem&campaignid=313639147&adgroupid=35684912840&device=c&keyword=executive%20data%20science%20coursera&matchtype=b&network=g&devicemodel=&adpostion=1t1&creativeid=189171011897&hide_mobile_promo&gclid=CjwKEAjw4vzKBRCt9Zmg8f2blgESJADN5fDg-r4XGA_gMNuFUgn3FsGhwHdk-ROxASsK5Prt9L-ZLRoCJDrw_wcB) on Coursera. 
## Dataset
contains the corpus, which is some web scrapped blog articles, news and twitter data. It also includes the output of Data_processing.R

## WordApp
The shinyapp

## Data_processing.R
R file contains the code that takes the raw twitter, blogs and news data and generates 1-gram, 2-gram, 3-gram and 4-gram probability tables to be used for the next word Prediction

## Wpredict.R
The word prediction function to be used by the shinyapp.
