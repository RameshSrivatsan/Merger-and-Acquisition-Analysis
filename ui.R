library(shiny)
library(ggplot2)
library(olsrr)

setwd("/Users/srivatsanramesh/Downloads")
aet = read.csv("AET.csv",header = TRUE,sep=',')
amzn = read.csv("AMZN.csv",header = TRUE,sep=',')
cvs = read.csv("CVS.csv",header = TRUE,sep=',')
dis = read.csv("DIS.csv",header = TRUE,sep=',')
fox = read.csv("FOX.csv",header = TRUE,sep=',')
nxpi = read.csv("NXPI.csv",header = TRUE,sep=',')
qcom = read.csv("QCOM.csv",header = TRUE,sep=',')
t = read.csv("T.csv",header = TRUE,sep=',')
twx = read.csv("TWX.csv",header = TRUE,sep=',')
wfm = read.csv("WFM.csv",header = TRUE,sep=',')

a = list(aet,amzn,cvs,dis,fox,nxpi,qcom,t,twx,wfm)

for(i in 1:length(a)){
  a[[i]] = data.frame(a[[i]])
  a[[i]]$log_returns = log10(a[[i]]$Close/a[[i]]$Open)
  a[[i]]$date_num=NA
  
  for(j in 1:252){
    a[[i]]$date_num[j]=j
  }
}
ui<-fluidPage(
  titlePanel("Merger & Acquisition Analysis"),
  navbarPage("Menu",theme = shinytheme("united"),
             tabPanel("Project Scope",
                      h5("In this app, we chose to analyse the stocks of 5 pairs of companies which have undergone Merger and Acquisition. We wished to analyse the fluctuations in the company’s stock when the M&A is announced, through statistical tests. Initially, we observed the log returns of each individual stock and then 
                         proceeded to compare the log returns of the pairs of stocks to see if they are correlated and if we can gain productive business insights from the analysis. For the scope of our project, we have chosen 5 pairs of companies, few of whom have completed the M&A transaction, few have announced the deal signing date, and the rest are in the process of negotiating the deal. The companies chosen for this project come from a very diverse range of industries including retail, e-commerce, technology and even the entertainment sector.",style="color:black;font-family:'Comic Sans MS"))
             
             
             ,
             
             
             
             
             tabPanel("Company's Equity Analysis",
                      sidebarLayout(
                        sidebarPanel(
                          
                          sliderInput(inputId = "num",label="choose bin size",value=15,min=1,max=30),
                          selectInput(inputId = "stock",label="Choose a stock",c("AETNA"=1,"AMAZON"=2,"CVS PHARMACY"=3,"DISNEY"=4,"21st CENTURY FOX"=5,"NXP SEMICONDUCTORS"=6,"QUALCOMM"=7,"AT&T"=8,"TIME WARNER"=9,"WHOLE FOODS"=10)),
                          sliderInput(inputId = "conf",label="choose confidence interval",value=0,min=10,max=100),
                          (plotOutput("residual_2"))),
                        
                        #sliderInput(inputId="conf",label="Choose a Confidence interval",value=25,min=1,max=100),
                        
                        mainPanel((tabsetPanel(tabPanel("Histogram", plotOutput("hist"),verbatimTextOutput("stats"),h5("Confidence Interval for Variance",align="left", style = "font-family: 'times'; font-si16pt"),verbatimTextOutput("CI_var"),h5("Confidence Interval for Mean",align="left", style = "font-family: 'times'; font-si16pt"),verbatimTextOutput("CI_mean")),tabPanel("QNorm",plotOutput("qqnorm")),tabPanel("Regression",plotOutput("time"),
                                                                                                                                                                                                                                                                                                                                                                                                                              h5("R-square, intercept and slope",align="Left",
                                                                                                                                                                                                                                                                                                                                                                                                                                 style = "font-family: 'times'; font-si16pt"),verbatimTextOutput("summary"))))))),
             
             
             
             tabPanel("Acquirer & Target's Equity Analysis",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "stock_1",label="Choose stock-1",c("AETNA"=1,"AMAZON"=2,"CVS PHARMACY"=3,"DISNEY"=4,"21st CENTURY FOX"=5,"NXP SEMICONDUCTORS"=6,"QUALCOMM"=7,"AT&T"=8,"TIME WARNER"=9,"WHOLE FOODS"=10)),
                          selectInput(inputId = "stock_2",label="Choose stock-2",c("AETNA"=1,"AMAZON"=2,"CVS PHARMACY"=3,"DISNEY"=4,"21st CENTURY FOX"=5,"NXP SEMICONDUCTORS"=6,"QUALCOMM"=7,"AT&T"=8,"TIME WARNER"=9,"WHOLE FOODS"=10)),
                          plotOutput("residual")),
                        mainPanel(plotOutput("scatter"))),h5("R-square, intercept and slope",align="left", style = "font-family: 'times'; font-si16pt"),verbatimTextOutput("summary_1"),verbatimTextOutput("value5"))))
