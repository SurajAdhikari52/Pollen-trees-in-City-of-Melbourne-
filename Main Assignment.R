
#title: 'Assignment 4: Exploration project'
#author: "Suraj"
#date: "31/05/2021"
#output: html_document

library(ggplot2)
library(dplyr)
library(leaflet)
library(shiny)
library(htmltools)
library(readxl)
library(sf)
library(rgdal)
library(sp)
library(data.table)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)


#Reading the Files
my_data <- read_excel("Pollen trees.xlsx")
patients <- read_excel("Pollen Allergic Count.xlsx")

#Data for line graph
 plot_2 <- patients %>%   group_by(BIRTH_YEAR,GENDER_FACTOR) %>% 
   summarise(Avg_age = mean(ALLERGIC_RHINITIS_START))
 
 #Data for Bar Chart
 plot_x <- my_data %>%   group_by(Located_in,Year_Planted) %>% 
   summarise(num =n())
 plot_4 <- na.omit(plot_x)
 
 # Data for Pie chart
 plot_y <- my_data %>%   group_by(Year_Planted,Genus) %>% 
   summarise(tree =n())
 plot_5 <- na.omit(plot_y)

button <- "#DivCompClear, #FinderClear, #EnterTimes{
background: DodgerBlue;font-size: 15px;}"

shinyApp(
  ui <- fluidPage(theme = shinytheme("slate"),
                  
    navbarPage("Pollen Allergy", theme = shinytheme("slate"),
               tabPanel("Trees Finder", fluid = TRUE, icon = icon("globe-americas"),
                        tags$style(button),
                        
    sidebarLayout(
      sidebarPanel( 
        titlePanel("Pollen Tress in city of Melbourne"),hr(),
        fluidRow(column(10,
                        #Select Trees
                        selectInput(inputId = "treeID", label = "Select type of tree",
                                    choices = my_data$Genus,
                                    hr())
        ))),
      mainPanel(    titlePanel("Map showing Location of Trees"),
        tabsetPanel(type = "tabs",
                    tabPanel( "Plot",
        fluidRow(column(6,
                        # leaflet output
                        leafletOutput(outputId = 'MapID', width = "200%") ) )  ),
        tabPanel("Summary", helpText("The map Shows the location of the trees in the city of Melbourne council.
                                     The radius of the Circle shows the age of the tree."))) )
      
      )),
    #patient tab
    tabPanel("Pollen patients", fluid = TRUE,
             tags$style(button),
             
             sidebarLayout(
               sidebarPanel( 
                 titlePanel("Pollen Patients over the Years"),hr(),
                 fluidRow(column(10,
                                 # Select which Gender(s) to plot
                                 checkboxGroupInput(inputId = "GenderFinder",
                                                    label = "Select Gender(s):",
                                                    choices = unique(patients$GENDER_FACTOR),
                                                    selected = patients$GENDER_FACTOR))
                 )),
               mainPanel(   titlePanel("Average age of the male and female patients over the period"),
                 tabsetPanel(type = "tabs",
                             tabPanel( "Graph",
                                       fluidRow(column(6,
                                                       # leaflet output
                                                       plotOutput("plot2") ) )  
                                       ),
                             tabPanel("Summary", helpText("The graph shows how the pollen affecting Age group of the patients goes on decresing over the year."))) )
               
             )),
    #Diversity of Trees
    tabPanel("Diversity of Trees", fluid = TRUE,
             tags$style(button),
             
             sidebarLayout(
               sidebarPanel( 
                 titlePanel("Diversity of trees over the Period"),hr(),
                 fluidRow(column(10,
                                 #Select Years
                                 sliderInput("Years", "Years:",
                                             min = 1997, max = 2021,
                                             value = 2017, step = 1))
                 ),hr() ,helpText("Summary:") ,hr(),
                 helpText("The Bar chart shows count of pollen trees in the park and In the street. It is also observed that the trees planted in the park are more than that of Street.
                          The pie chart shows the distribustion of trees according to there Genus ")),
               mainPanel(  titlePanel("Number of trees in the Parks Vs Street"), 
                 fluidRow(column(4,
                           # barchart output
                               plotOutput("plot3") ),
                          column(4,offset = 2,
                                 # piechart output
                                 plotOutput("plot4") )))
               
             ))
    
    )
    )
  ,
  server = shinyServer(function(input, output) {
    #Map 
    output$MapID <- renderLeaflet({
      leaflet(data = subset(my_data, my_data$Genus == input$treeID)) %>% addTiles() %>%
        addCircles(lng = ~Longitude,lat = ~Latitude,color=~ColorPal(Genus),
                   radius = ~Age)
      })
    #line graph
     output$plot2<-renderPlot({
      ggplot(plot_2<-subset(plot_2,GENDER_FACTOR == input$GenderFinder ),aes(x=BIRTH_YEAR, y= Avg_age)) + 
       geom_line(aes(color = GENDER_FACTOR)) +
    labs(x= "BIRTH YEAR", y="AGE")},height = 400,width = 600)
    
     #Bar graph
     output$plot3<-renderPlot({
       ggplot(plot_4<-subset(plot_4,Year_Planted == input$Years ),aes(x=Located_in, y= num )) + 
         geom_bar(stat="identity",  fill="steelblue") +
         theme_dark()+
         geom_text(aes(label = num), vjust = -0.5, size = 5)+
         labs(x= "Location", y="Number of trees")},height = 400,width = 400)
     
     #pie Chart
     output$plot4<-renderPlot({
       plot_5<-subset(plot_5,Year_Planted == input$Years )
       slices <- plot_5$tree
       lbls <- plot_5$Genus
       pie(slices,labels=lbls,
           main="Pie Chart of Countries ")},height = 400,width = 400)
      
  }
  )
)