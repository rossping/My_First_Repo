library(shiny)
library(ggplot2)
library(shinythemes)
library(readr)
library(countrycode)
library(tibble)
happiness <- read_csv('Clean_Happiness_Data.csv')
ui <- fluidPage(
    theme = shinytheme("yeti"),
    titlePanel("Find Happiness"),
    selectInput("Rank",
                label = "Happiness Rank",
                choices = names(happiness)[1]),
    
    selectInput("Predictor", 
                label = "Predictor Variable", 
                choices = names(happiness)[5:9]),
    
    selectInput("color", 
                label = "Color By", 
                choices = c("Continent","Year", "Country")),
    
    plotOutput("newplot"),
    
    varSelectInput("y", 
                   label = "Explain the Variation in Happiness Ranking",
                   happiness[5:9], multiple = T),
    
    textOutput("R_Squared"),
    
    tableOutput("Summary"),
    
    actionButton("click", "Print R-Squared, Coefficients and Residual Plot" ),
    
    plotOutput("residual")

)

server <- function(input, output) {
    output$newplot <- renderPlot({
        
        h_rank = unlist(happiness[input$Rank])
        tb = tibble(h_rank)
        h_rank = tb[[1]]
        
        predictor = unlist(happiness[input$Predictor])
        tb = tibble(predictor)
        predictor = tb[[1]]

        
     
        color = unlist(happiness[input$color])
        tb = tibble(color)
        color = tb[[1]]
        
      
        
        ggplot(happiness,aes(x = h_rank, y = predictor, color = color)) + geom_smooth(method = "lm", se = F, color = 'red', lwd = 5) + geom_point(size = 2, alpha = .5) + 
            labs(x = input$Rank, y = input$Predictor , title = paste(input$Rank,"vs", input$Predictor)) + theme(plot.title = element_text(face = "bold" , size = 25, hjust = .5) )
    
    })
    button <- eventReactive(input$click, {
        
        reg <- lm(data = happiness, as.formula(paste(input$Rank, '~', paste(input$y, collapse = "+"))))
        sumr <- summary(reg)
        paste("The Adjusted R-Squared of this model is", sumr$adj.r.squared)
    })
    coef <- eventReactive(input$click,{
        reg <- lm(data = happiness, as.formula(paste(input$Rank, '~', paste(input$y, collapse = "+"))))
        sumr <- summary(reg)
        cf <- sumr$coefficients
        cf
    })
    resid_predicted <- eventReactive(input$click,{
        reg <- lm(data = happiness, as.formula(paste(input$Rank, '~', paste(input$y, collapse = "+"))))
        sumr <- summary(reg)
        res <- sumr$residuals
        pred <- predict(reg) 
        ggplot(happiness, aes(pred , res)) + geom_point() + labs(title = "Predicted vs. Residual Values", x = "Predicted", y = "Residuals") + theme(plot.title = element_text(face = "bold" , size = 25, hjust = .5)) + geom_hline(yintercept = 0, color = 'red', lwd = 2)
    })
        
    output$R_Squared <- renderText({
        button()
    })
    output$Summary <- renderTable({
        coef()
    })
    output$residual <- renderPlot({
        resid_predicted()
    })
    
    
  
    
}

shinyApp(ui = ui, server = server)


