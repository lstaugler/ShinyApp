library(shinydashboard)
library(xlsx)
library(DT)

df <- read.xlsx("student-mat.xlsx", 1)

function(input, output) {
  output$distPlot <- renderPlot({
    
    x    <- df$G3
    bits <- seq(min(x), max(x),length.out = input$bins + 1)
    
    hist(x, breaks = bits, col = "pink", border = "black",
         xlab = "Number of Students per Score",
         main = "Final Scores of Students")
  })
  output$tb1 <- renderDT(df)
  
  output$xvariable <- renderUI({
    req(df)
    xcol<-colnames(df)
    pickerInput(inputId = 'xvar',
                label = 'Choose x-axis variable(s)',
                choices = c(xcol[1:length(xcol)]), selected=xcol[1],
                options = list(`style` = "btn-info"),
                multiple = TRUE)
    
  })
  output$yvariable <- renderUI({
    req(df)
    ycol<-colnames(df) 
    pickerInput(inputId = 'yvar',
                label = 'Target variable',
                choices = c(ycol[33]),
                options = list(`style` = "btn-info"),
                multiple = FALSE)
  })
  
  MLR <- reactive({
    req(df,input$xvar,input$yvar)
    x <- as.numeric(df[[as.name(input$xvar)]])
    y <- as.numeric(df[[as.name(input$yvar)]])
    current_formula <- paste0(input$yvar, " ~ ", paste0(input$xvar, collapse = " + "))
    current_formula <- as.formula(current_formula)
    model <- lm(current_formula, data = df, na.action=na.exclude)
    return(model)
  })
  
  output$LMSum <- renderPrint({
    req(MLR())
    summary(MLR())
  })
  
  output$diagPlot <- renderPlot({
    req(MLR())
    par(mfrow = c(2,2))
    plot(MLR())
  })
  output$studentTable <- DT::renderDataTable({df})
}