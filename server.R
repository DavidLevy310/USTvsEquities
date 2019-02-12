library(shiny)
library(shinydashboard)


function(input, output,session){
  output$Yields <- renderPlotly({Rates_Plot})
  output$Sectors <- renderPlot({Sector_Returns})
  output$Cor_Plt <- renderPlot({Corr_Plot2})
  
  output$graph <- renderPlotly({
    ggplot()+
      geom_line(data=Rates_Sectors_Melted %>% filter(variable==input$Product_1),
                aes(x=Date,
                    y=Percentage_Change,
                    color=variable))+
      geom_line(data=Rates_Sectors_Melted %>% filter(variable==input$Product_2),
              aes(x=Date,
                  y=Percentage_Change,
                  color=variable))})
    
  
  updateSelectizeInput(session,'Product_1', choices = Product_List, server = TRUE)
  
  updateSelectizeInput(session,'Product_2', choices = Product_List, server = TRUE)
}