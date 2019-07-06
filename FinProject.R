# Global variables can go heres
setwd("/Users/pandiyaraajv/Desktop/R/")
crop_cost<-read.csv("./datasetagri/datagovin.csv")

crop_cost<-data.frame(crop_cost)
names(crop_cost)

names(crop_cost) <- gsub("\\.", "", names(crop_cost))
names(crop_cost)

library(lattice)
library(dplyr)
library(gridExtra)
library(tidyr)
library(RColorBrewer)
library(ggplot2)

yield<-crop_cost %>% select(Crop,YieldQuintalHectare)%>% group_by(Crop)%>% summarise(yield=sum(YieldQuintalHectare))
yield <-data.frame(yield)
yield

# Define the UI
ui <- fluidPage(
  navbarPage("Agiculture data point from 2k1-2k14!",
             tabPanel("Total Yield",mainPanel(
               plotOutput("plot_bar",  height = "650px" , width = "150%")
             )
             ),
             tabPanel("Cost of Production(Quintal)",mainPanel(
               plotOutput("plot_hist",  height = "650px" , width = "150%")
             )
             ),
             tabPanel("Cost of Cultivation(Hectare)",
                      sidebarPanel(
                        #radioButtons("plotvalue", "Plot Value",
                        checkboxGroupInput("plotvalue", "Select atleast one crop:",selected = "PADDY",
                                           c("PADDY"="PADDY", "GRAM"="GRAM" , "MOONG" = "MOONG","ARHAR"="ARHAR","COTTON"="COTTON","GROUNDNUT"="GROUNDNUT","MAIZE"="MAIZE","WHEAT"="WHEAT","RAPESEED AND MUSTARD"="RAPESEED AND MUSTARD","SUGARCANE"="SUGARCANE")
                        )
                        
                        #ARHAR COTTON GRAM GROUNDNUT MAIZE MOONG PADDY RAPESEED AND MUSTARD WHEAT SUGARCANE
                        
                      ),mainPanel(
                        plotOutput("plot_bw",  height = "650" , width = "100%")
                      )
             ),
             tabPanel("Major & Major",mainPanel(
               plotOutput("plot_bar_major",  height = "650px" , width = "150%")
             )
             ),
             tabPanel("Summary",
                      verbatimTextOutput("summary")
             )
  )
)

# Define the server code
server <- function(input, output, session) {
  output$plot_bar <- renderPlot({
    p <-ggplot(yield, aes(x=Crop, y=yield)) +
      geom_bar(stat="identity", fill="steelblue") 
    p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$plot_hist <- renderPlot({
    histogram(~CostofProductionQuintalC2|Crop ,data=crop_cost,col="brown")
  })
  
  output$plot_bw <- renderPlot({
    bwplot(CostofCultivationHectareC2 ~ State |Crop, data = crop_cost,
           varwidth = TRUE,
           ylab = "Cost of Cultivation/Hectare",
           xlab="Crop wrt State",
           col="brown",cex.label=1,fill="red",main="State level cost of Crop cultivation per Hectare",
           scales=list (x=list(cex=0.5, rot=90)),
           #layout=c(4,4),
           subset=Crop %in% c(input$plotvalue))
  })
  
  output$plot_bar_major <- renderPlot({
    cc<-crop_cost %>% select(Crop,State,YieldQuintalHectare)%>% filter(Crop=="SUGARCANE")%>% mutate(Total= sum(YieldQuintalHectare),percent=(YieldQuintalHectare/Total)*100)
    
    g1<-barchart(percent ~ State,data=cc,fill="darkgreen",col="darkgreen",main="SUGARCANE Yield")
    
    cot<-crop_cost %>% select(Crop,State,YieldQuintalHectare)%>% filter(Crop=="COTTON")%>% mutate(Total= sum(YieldQuintalHectare),percent=(YieldQuintalHectare/Total)*100)
    
    g2<-barchart(percent ~ State,data=cot,fill="grey",col="grey",main="COTTON Yield")
    
    pad<-crop_cost %>% select(Crop,State,YieldQuintalHectare)%>% filter(Crop=="PADDY")%>% mutate(Total= sum(YieldQuintalHectare),percent=(YieldQuintalHectare/Total)*100)
    g3<-barchart(percent ~ State,data=pad,fill="gold",col="gold",main="PADDY Yield")
    wh<-crop_cost %>% select(Crop,State,YieldQuintalHectare)%>% filter(Crop=="WHEAT")%>% mutate(Total= sum(YieldQuintalHectare),percent=(YieldQuintalHectare/Total)*100)
    g4<-barchart(percent ~ State,data=wh,fill="brown",col="brown",main="WHEAT Yield")
    grid.arrange(g1,g2,g3,g4,nrow=2,ncol=2)
  })
  
  output$summary <- renderPrint({
    summary(yield)
  })
  
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)
