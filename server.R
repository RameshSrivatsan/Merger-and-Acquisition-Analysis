server<-function(input,output){
  
  output$hist<-renderPlot({
    h = hist(a[[as.numeric(input$stock)]]$log_return,main='Histogram for log_return',
             xlab="Stock",
             border="blue",
             col="green",
             las=1,
             breaks=input$num) 
    xfit<-seq(min(a[[as.numeric(input$stock)]]$log_return),max(a[[as.numeric(input$stock)]]$log_return),length=40) 
    yfit<-dnorm(xfit,mean=mean(a[[as.numeric(input$stock)]]$log_return),sd=sd(a[[as.numeric(input$stock)]]$log_return)) 
    yfit <- yfit*diff(h$mids[1:2])*length(a[[as.numeric(input$stock)]]$log_return) 
    lines(xfit, yfit, col="black", lwd=2)
  })
  output$time<-renderPlot({
    
    output$time<-renderPlot({  
      stock1Data=a[[as.numeric(input$stock)]]
      ggplot(stock1Data,
             aes(y = stock1Data$log_return, x = stock1Data$date_num)) + geom_point() + geom_smooth(method =
                                                                                                     'lm')
    })
    
  })
  output$stats<-renderPrint({
    summary(a[[as.numeric(input$stock)]]$log_return)
  })
  
  output$CI_var <- renderText({
    n=252
    var_=var(a[[as.numeric(input$stock)]]$log_return)
    a=(100-(100-(input$conf))/2)/100
    error1=qchisq(a,df=n-1)
    error2=qchisq(1-a,df=n-1)
    left=(n-1)*var_/error1
    right=(n-1)*var_/error2
    paste("(",left,",",right,")")
  })
  output$value5 = renderText(
    {
      if (input$stock_1 != input$stock_2){
        mean_1 = mean(a[[as.numeric(input$stock_1)]]$log_return)
        mean_2 = mean(a[[as.numeric(input$stock_2)]]$log_return)
        test1 = t.test(a[[as.numeric(input$stock_1)]]$log_return,a[[as.numeric(input$stock_2)]]$log_return,paired=TRUE,conf.level = 0.95)
        paste("The Null Hypothesis is accepted if p > 0.05 in a 95% Confidence Interval test. The p value for the two stocks is ",round(test1$p.value,digits = 3))
        
      }
    })
  output$qqnorm = renderPlot({
    qqnorm(a[[as.numeric(input$stock)]]$log_return, 
           ylab="Standardized Residuals", 
           xlab="Normal Scores", 
           main="Normal Probabilty Plot") 
    qqline(a[[as.numeric(input$stock)]]$log_return)
  })
  
  
  output$CI_mean <- renderText({
    avg_=mean(a[[as.numeric(input$stock)]]$log_return)
    std_=sd(a[[as.numeric(input$stock)]]$log_return)
    n=252
    a=(100-(100-(input$conf))/2)/100
    error <- qt(a,df=n-1)*std_/sqrt(n)
    left <- avg_-error
    right <- avg_+error
    paste("(",left,",",right,")")
  })
  #for linear egression
  output$summary <- renderPrint({
    alist=c(1:length(a[[as.numeric(input$stock)]]$log_return))
    sumry=lm(formula = a[[as.numeric(input$stock)]]$log_return ~ alist)
    a=summary(sumry)
    c(a$r.squared,a$coefficients[1],a$coefficients[2])
  })
  
  
  output$scatter<-renderPlot({
    
    
    stock1Data=a[[as.numeric(input$stock_1)]]
    stock2Data=a[[as.numeric(input$stock_2)]]
    ggplot(stock1Data,
           aes(y = stock2Data$log_return, x = stock1Data$log_return)) + geom_point() + geom_smooth(method =
                                                                                                     'lm')
    
  })
  output$residual<-renderPlot({
    model_1<-lm(a[[as.numeric(input$stock_1)]]$log_return~a[[as.numeric(input$stock_2)]]$log_return)
    ols_rsd_hist(model_1)
  })
  output$residual_2<-renderPlot({
    model_2<-lm(a[[as.numeric(input$stock)]]$log_return~a[[as.numeric(input$stock)]]$date_num)
    ols_rsd_hist(model_2)
  })
  
  output$summary_1<-renderPrint({
    model_3<-lm(a[[as.numeric(input$stock_2)]]$log_return~a[[as.numeric(input$stock_1)]]$log_return)
    if(input$stock_1 != input$stock_2){
      a = summary(model_3)
      c(a$r.squared,a$coefficients[1],a$coefficients[2])
    }    })
  
}
shinyApp(ui=ui,server=server)