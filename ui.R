library(shiny)
library(shinydashboard)
library(DT)
library(shinyWidgets)


shinyUI(dashboardPage(
  
  dashboardHeader(
    title = "US MKT INFO",titleWidth = 190),
  
  dashboardSidebar(tags$style(HTML(".main-sidebar{width: 190px;}")),
    sidebarUserPanel("David Levy",image="https://media.licdn.com/dms/image/C5603AQH8IxtKz1jsKQ/profile-displayphoto-shrink_200_200/0?e=1555545600&v=beta&t=kT6ACo03ERXpRDE87RtFEBY7CuqeQEHPxfIY5IL6zzE"),
    sidebarMenu(id="tabs",
      menuItem("About",tabName="About",icon = icon("book")),
      menuItem("Historical Returns", icon =icon("line-chart"),
              menuSubItem("Treasury Yields",tabName = "TYields", icon =icon("line-chart")),
              menuSubItem("Equity Returns",tabName = "Stocks",icon =icon("line-chart"))),
      menuItem("Correlation Matrix",tabName="Correlation_Matrix", icon=icon("grip-horizontal")),
      menuItem("Comparison Tool",tabName="Tool", icon=icon("balance-scale")))),
    
  dashboardBody(
    tabItems(
      
      ####About page
      tabItem(tabName="About",
              includeMarkdown("David_Levy.rmd")),
      
      ####Historical Returns
        
        ####Yields
      tabItem(tabName="TYields",
              fluidPage(
                plotlyOutput("Yields", height = 700))),
        
        ####Equities     
      tabItem(tabName="Stocks",
              fluidPage(
                plotOutput("Sectors", height = 700))),
      
      
      ####Correlation Matrix 
      tabItem(tabName="Correlation_Matrix",
              fluidPage(
                img(src="Correlation_Matrix.png",height=700,width=800))),
      
      
      ####Tool
      
      tabItem(tabName="Tool",
              fluidRow(
                column(width=5,selectizeInput(
                  inputId = "Product_1",label="Product1",choices=Product_List))),
              fluidRow(
                column(width=5,selectizeInput(
                  inputId = "Product_2",label="Product2",choices=Product_List))),
                
                
                fluidRow( 
                plotlyOutput("graph",height=600))
              
      ))
    )))
                                                                                              
                       
                  
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
              
       



      
    

  

