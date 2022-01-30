library(plotly)
library(shiny)
library(markdown)

df_corr=read.csv("corr_export.csv")
df_import=read.csv("import.csv")
df_export=read.csv("export.csv")
df_export_breakdown=read.csv("malaysiaexports.csv")

# Define UI for application that draws a histogram
ui <- navbarPage("External Trade Conditions",
             
      tabPanel("Correlation Analysis",
 
      # Correlation Analysis
        sidebarLayout(
          sidebarPanel(
           selectInput("Country_corr", "Select Country", choices = df_corr$Country)
          ),
    
        mainPanel(
        plotlyOutput("corr_chart")
          )
          )
      ),

      tabPanel("Monthly Growth Import",  
               sidebarLayout(
                 sidebarPanel(
                   selectInput("Year_import", "Select Year", choices = df_import$Year)
                 ),
                 
                 mainPanel(
                   plotlyOutput("import_chart")
                 )
               )
      ),
      
      tabPanel("Monthly Growth Export",  
               sidebarLayout(
                 sidebarPanel(
                   selectInput("Year_export", "Select Year", choices = df_export$Year)
                 ),
                 
                 mainPanel(
                   plotlyOutput("export_chart")
                 )
               )
      ),
      
      tabPanel("Export Breakdown",  
               sidebarLayout(
                 sidebarPanel(
                   selectInput("Date_breakdown", "Select Date", choices = df_export_breakdown$Date)
                 ),
                 
                 mainPanel(
                   plotlyOutput("breakdown_chart")
                 )
               ) 
      )
      
      
#Ending for ui      
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #Correlation Analysis Server
    output$corr_chart <- renderPlotly({
      
      sub_df_corr <- subset(df_corr, Country == input$Country_corr)
      p <- plot_ly(       
        x = list(18.801702640,
                7.343895486,
                 -0.848451896,
                 -1.130084166,
                24.959604831,
                -2.377054416,
                10.225647624,
                -6.358515527,
                -24.721519859,
                -25.836025385,
                8.103297266,
                3.249797518,
                -0.931114416,
                13.597979457,
                0.362379725,
                4.662168254,
                10.888916947,
                6.331886976,
                17.375678276,
                30.872061082,
                62.669116158,
                46.993590943,
                27.237566748,
                5.004835561,
                18.367135245,
                24.679974775,
                25.484263890
      ),
        y = sub_df_corr$Percentage,
        type = "scatter",
        mode = "markers",
        marker = list()
    )   %>% 
        layout(title = "Correlation between Exports of Malaysia vs Different Countries ", 
               xaxis =list(title="MALAYSIA (RM)", range = c(-40,80)),
               yaxis = list( range =c(-100,200),title=input$Country_corr))
  })
    #Import Server
    output$import_chart <- renderPlotly({
      
      sub_df_import <- subset(df_import, Year == input$Year_import)
      p <- plot_ly(       
        x = sub_df_import$Country,
        y = sub_df_import$Percentage,
        type = "bar",          
      )   %>% 
        layout(title = "Monthly Growth Import", 
               xaxis =list(title="Country"),
               yaxis = list(title=" Percentage change of preceding year (%)"))
    }) 
    
    #Export Server
    output$export_chart <- renderPlotly({
      
      sub_df_export <- subset(df_export, Year == input$Year_export)
      p <- plot_ly(       
        x = sub_df_export$Country,
        y = sub_df_export$Percentage,
        type = "bar" ,          
      )   %>% 
        layout(title = "Monthly Growth Export", 
               xaxis =list(title="Country"),
               yaxis = list(title=" Percentage change of preceding year (%)"))
    })
    
    #Breakdown Server
    output$breakdown_chart <- renderPlotly({
      
      sub_df_breakdown <- subset(df_export_breakdown, Date == input$Date_breakdown)
      p <- plot_ly(       
        x = sub_df_breakdown$Amount,
        y = sub_df_breakdown$Category,
        type = "bar" , 
        orientation = "h",
        color = ~sub_df_breakdown$Type,
      )   %>% 
        layout(
          title = "Exports Breakdown", 
          xaxis =list(title="Amount"),
          yaxis = list(title="Category")
        )
    })
    
    
    
    
    
#ending for server    
}

# Run the application 
shinyApp(ui = ui, server = server)
