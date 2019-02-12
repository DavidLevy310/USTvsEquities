#libraries----
library(dplyr)
library(tidyverse)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(lubridate)
library(PerformanceAnalytics)
library(reshape2)
library(corrplot)
library(Stack)
library(plotly)
library(GGally)
library(ggcorrplot)
library(zoo)

#Load S&P500 Sector CSV data as DF ----
Energy = read.csv("XLE.csv",stringsAsFactors = FALSE)
Tech = read.csv("XLK.csv",stringsAsFactors = FALSE)
Financial = read.csv("XLF.csv",stringsAsFactors = FALSE)
Cons_Disc = read.csv("XLY.csv",stringsAsFactors = FALSE)
Cons_Staples = read.csv("XLP.csv",stringsAsFactors = FALSE)
Healthcare = read.csv("XLV.csv",stringsAsFactors = FALSE)
Industrials = read.csv("XLI.csv",stringsAsFactors = FALSE)
Materials = read.csv("XLB.csv",stringsAsFactors = FALSE)
Utilities = read.csv("XLU.csv",stringsAsFactors = FALSE)

#Load Interest Rates CSV into DF
Rates_Change = read.csv("Rates_Change.csv",stringsAsFactors = FALSE)
Rates_Yield = read.csv("Rates_Yield.csv",stringsAsFactors = FALSE)

#Date field type to Date----
Rates_Yield = Rates_Yield %>% 
  mutate(DATE=as.Date(DATE,format="%m/%d/%y"))

Rates_Change = Rates_Change %>% 
  mutate(DATE=as.Date(DATE,format="%m/%d/%y"))


#Change Rate DFs to numeric
Rates_Change = Rates_Change %>% 
  mutate(Two_Yr = as.numeric(gsub("%","",X2.Year))) %>% 
  mutate(Five_Yr = as.numeric(gsub("%","",X5.Year))) %>% 
  mutate(Ten_Yr = as.numeric(gsub("%","",X10.Year))) %>% 
  mutate(Thirty_Yr = as.numeric(gsub("%","",X30.Year)))

Rates_Change = Rates_Change %>% 
  mutate(X2.Year=NULL) %>% 
  mutate(X5.Year=NULL) %>% 
  mutate(X10.Year=NULL) %>% 
  mutate(X30.Year=NULL)


#Change Monthly Return Field Type to numeric----
Energy = Energy %>% 
  mutate(Total.Monthly.Return=as.numeric(gsub("%","",Total.Monthly.Return)))

Tech = Tech %>% 
  mutate(Total.Monthly.Return=as.numeric(gsub("%","",Total.Monthly.Return)))

Financial = Financial %>% 
  mutate(Total.Monthly.Return=as.numeric(gsub("%","",Total.Monthly.Return)))

Cons_Disc = Cons_Disc%>% 
  mutate(Total.Monthly.Return=as.numeric(gsub("%","",Total.Monthly.Return)))

Cons_Staples = Cons_Staples %>% 
  mutate(Total.Monthly.Return=as.numeric(gsub("%","",Total.Monthly.Return)))

Healthcare = Healthcare %>% 
  mutate(Total.Monthly.Return=as.numeric(gsub("%","",Total.Monthly.Return)))

Industrials = Industrials %>% 
  mutate(Total.Monthly.Return=as.numeric(gsub("%","",Total.Monthly.Return)))

Materials = Materials %>% 
  mutate(Total.Monthly.Return=as.numeric(gsub("%","",Total.Monthly.Return)))

Utilities = Utilities %>% 
  mutate(Total.Monthly.Return=as.numeric(gsub("%","",Total.Monthly.Return)))

#Change Date field type to Date----
Energy = Energy %>% 
  mutate(Date=as.Date(Date,format = "%m/%d/%y"))

Tech = Tech %>% 
  mutate(Date=as.Date(Date,format = "%m/%d/%y"))

Financial = Financial %>% 
  mutate(Date=as.Date(Date,format = "%m/%d/%y"))

Cons_Disc = Cons_Disc %>% 
  mutate(Date=as.Date(Date,format = "%m/%d/%y"))

Cons_Staples = Cons_Staples %>% 
  mutate(Date=as.Date(Date,format = "%m/%d/%y"))

Healthcare = Healthcare %>% 
  mutate(Date=as.Date(Date,format = "%m/%d/%y"))

Industrials = Industrials %>% 
  mutate(Date=as.Date(Date,format = "%m/%d/%y"))

Materials = Materials %>% 
  mutate(Date=as.Date(Date,format = "%m/%d/%y"))

Utilities = Utilities %>% 
  mutate(Date=as.Date(Date,format = "%m/%d/%y"))


#Mutate to include cumulative returns----
Tech %>% 
  mutate(testreturn=Total.Monthly.Return/100) %>% 
  mutate(Cumulative=cumprod(1+(testreturn))-1) %>% 
  group_by(Yearly=floor_date(Date,"month")) 

#Normalize Each index to start at 100 + Add Column to track % Gain/Loss from start----

Cons_Disc = 
  Cons_Disc %>% mutate(Index=(100/21.63090)*Adj.Close)

Cons_Disc = 
  Cons_Disc %>% mutate(Pct_Change=Index-100)

Cons_Staples = 
  Cons_Staples %>% mutate(Index=(100/17.134813)*Adj.Close)

Cons_Staples = 
  Cons_Staples %>% mutate(Pct_Change=Index-100)

Energy = 
  Energy %>% mutate(Index=(100/14.890138)*Adj.Close)

Energy =
  Energy %>% mutate(Pct_Change=Index-100)

Financial =
  Financial %>% mutate(Index=(100/9.928497)*Adj.Close)

Financial = 
  Financial %>% mutate(Pct_Change=Index-100)

Healthcare = 
  Healthcare %>% mutate(Index=(100/20.688719)*Adj.Close)

Healthcare = 
  Healthcare %>% mutate(Pct_Change=Index-100)

Industrials = 
  Industrials %>% mutate(Index=(100/16.979313)*Adj.Close)

Industrials = 
  Industrials %>% mutate(Pct_Change=Index-100)

Materials =
  Materials %>% mutate(Index=(100/13.418863)*Adj.Close)

Materials =
  Materials %>% mutate(Pct_Change=Index-100)

Tech = 
  Tech %>% mutate(Index=(100/30.051966)*Adj.Close)

Tech =
  Tech %>% mutate(Pct_Change=Index-100)

Utilities =
  Utilities %>% mutate(Index=(100/14.44481)*Adj.Close) 

Utilities =
  Utilities %>% mutate(Pct_Change=Index-100)

#Equities Plots ----


Sector_Returns = ggplot() + 
  geom_line(data=Cons_Disc, aes(x=Date, y=Pct_Change, color='green')) + 
  geom_line(data=Cons_Staples, aes(x=Date, y=Pct_Change, color='red'))+
  geom_line(data=Energy, aes(x=Date, y=Pct_Change, color='blue'))+
  geom_line(data=Financial, aes(x=Date, y=Pct_Change, color='purple'))+
  geom_line(data=Healthcare, aes(x=Date, y=Pct_Change, color='orange'))+
  geom_line(data=Industrials, aes(x=Date, y=Pct_Change, color='maroon'))+
  geom_line(data=Materials, aes(x=Date, y=Pct_Change, color='black'))+
  geom_line(data=Tech, aes(x=Date, y=Pct_Change, color='yellow'))+
  geom_line(data=Utilities, aes(x=Date, y=Pct_Change, color='grey'))+
  labs(y="Cumulative Returns(%)",title="SP500 Sector Returns (1999-2018)")+
  scale_color_discrete(name="Indices",labels=c("Materials","Energy","Cons_Disc", "Utilities",
                                               "Industrials","Healthcare","Financial","Cons_Staples",
                                               "Tech"))+
  theme(plot.title = element_text(size = 22, face = "bold"),
        axis.text=element_text(size=16),
        legend.title=element_text(size=16), 
        legend.text=element_text(size=16))
                                        
Sector_Returns


#Melt Rates DF and Plot----

melted = melt(Rates_Yield, id.vars=c("DATE"))
melted 

Rates_Plot = ggplot() +
  geom_line(data=melted,aes(x=DATE,y=value,color=variable))+
  labs(x="Date",y="Yield %",title="1999-2018 US Treasury Yields")

Rates_Plot

#Combine Rates+Sectors----
Rates_Sectors = Rates_Change %>%
  mutate(EnergyReturn=Energy$Total.Monthly.Return) %>% 
  mutate(ConsDiscReturn=Cons_Disc$Total.Monthly.Return) %>% 
  mutate(ConsStaplesReturn=Cons_Staples$Total.Monthly.Return) %>% 
  mutate(FinancialReturn=Financial$Total.Monthly.Return) %>% 
  mutate(HealthCareReturn=Healthcare$Total.Monthly.Return) %>% 
  mutate(IndustrialsReturn=Industrials$Total.Monthly.Return) %>% 
  mutate(MaterialsReturn=Materials$Total.Monthly.Return) %>% 
  mutate(TechReturn=Tech$Total.Monthly.Return) %>% 
  mutate(UtilitiesReturn=Utilities$Total.Monthly.Return)


Rates_Sectors_Melted = melt(Rates_Sectors,id.vars=c("DATE"))
Rates_Sectors_Melted

Rates_Sectors_Melted = Rates_Sectors_Melted %>% 
  rename(Percentage_Change=value) %>% 
  rename(Date=DATE)

Rates_Sectors_2 = Rates_Sectors %>% 
  mutate(DATE=NULL)

colnames(Rates_Sectors)=c("Date","Two_Yr","Five_Yr","Ten_Yr","Thirty_Yr","Energy","C_Discretion",
                          "C_Staples","Financial","Healthcare","Industrials",
                          "Materials","Tech","Utilities")
colnames(Rates_Sectors_2)=c("Two_Yr","Five_Yr","Ten_Yr","Thirty_Yr","Energy","C_Discretion",
                           "C_Staples","Financial","Healthcare","Industrials",
                           "Materials","Tech","Utilities")

#Corr Plots ----

Corr_Matrix = cor(Rates_Sectors_2,use="complete.obs")

Corr_Plot = corrplot(Corr_Matrix,
                     mar=c(0,0,2,0))

       
Corr_Plot2 = ggcorrplot(Corr_Plot, method = "circle",hc.order = TRUE,type='full',title="UST vs. Equity Sectors",title)
Corr_Plot2

#date choices

date_choices = seq(as.Date("1999-01-01"), as.Date("2019-01-01"), by="1 month")
date_choices[length(date_choices)] = as.Date("2018-12-01")

#Product List

Product_List = c("Two_Yr","Five_Yr","Ten_Yr","Thirty_Yr","EnergyReturn","ConsDiscReturn","ConsStaplesReturn","FinancialReturn",
"HealthcareReturn","IndustrialsReturn","MaterialsReturn","TechReturn","UtilitiesReturn")




