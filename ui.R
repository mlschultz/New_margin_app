#{{{
sidebarLayout(
  sidebarPanel(
    
    sliderInput("shares", label = "Number of Shares", max = 1000, min = 100, value = 100, step = 100),
    sliderInput("price", label = "Price Per Share", max = 200, min = 1, value = 1, step = 1),
    sliderInput("initial", label = "Initial Margin", max = 100, min = 50, value = .1),
    sliderInput("newprice",label = "New Stock Price", max = 200, min = 1, value = 1)
    
    
  ),
  mainPanel(
    
    renderPlot({
      bondValue <- 0
      ytm1Axis <- seq(0.01, .2, by = .01)
      period <- 1
      
      cash_flows <- 0
      for (i in 1:input$maturity1){
        cash_flows[i] <- 
          (input$coupon1 * 1000) }
      
      cash_flows[length(cash_flows)] <- cash_flows[length(cash_flows)] + 1000 
      pvCashFlows <- cash_flows / (1 + input$ytm1)^(1:input$maturity1)
      
      ### Bond Calculation: 
      bondValue <- (input$coupon1 * 1000) * ((1 - 1 / (1 + input$ytm1)^(input$maturity1)) / input$ytm1) + 1000 / (1 + input$ytm1)^(input$maturity1) 
      
      ### Calculate Duration and Modified Duration ----
      Duration1 <- 1:input$maturity1 %*% (pvCashFlows / bondValue)
      modDur1 <- Duration1 / (1 + input$ytm1)
      
      #             durLine <- bondValue[index] * (1 + (modDur * (ytm1Axis - input$ytm1)))
      
      plot(0, ylim = c(0,1), xlim = c(0,1), type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "")
      text(x = 0.5, y = 0.5, labels = paste(round(modDur1, 2), "years"), cex = 5)
      
      
    })
  )
)
#}}}