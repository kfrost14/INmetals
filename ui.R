
pageWithoutHeader <- function (controls, header, plots) 
{
  bootstrapPage(div(class="container-fluid", 
                    div(class = "row-fluid", 
                        controls, div(class = "row-fluid", header, div(class="row-fluid", plots)))))
}
# tags$style(type="text/css",
#            ".shiny-output-error { visibility: hidden; }",
#            ".shiny-output-error:before { visibility: hidden; }"
# )
#          tablePanel <- function (...) 
#         {
#           div(class = "span3 tablePanel", ...)
#         }
        
        titlePanel <- function (title) 
        {
          div(class = "span7 titlePanel", style = "padding-left: 10px;", h3(title))
        }
        
        controlPanel <- function (...) 
        {
          div(class = "span3 controlPanel", tags$form(class = "well", ...))
        }
        
        plotPanel <- function(...)
        {
          div(class = "span7 plotPanel",...)
        }
        
        addResourcePath('metalsapp', getwd())                          
                          
shinyUI(pageWithoutHeader(
  controlPanel( 

    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="/metalsapp/metals.css"),
      tags$title("Metals Data Explorer")
    )
    ,
    
   
#  shinyUI(pageWithSidebar(
# #   
#    headerPanel(h3("Indiana Metals Data"),windowTitle="Metals Data Explorer"),
#    sidebarPanel(
  bsProgressBar("pb1"),

    conditionalPanel(condition="input.contab==1",
    
                     uiOutput("availparam") ,    
         
                     selectInput(inputId = "chart_type",
                                 label = "Chart type",
                                 choices = c("Boxplot",
                                             "Trend",
                                             "Barplot",
                                             "Histogram"),selected="Trend"),
                      

                      #   uiOutput ("choosedata"),
                     uiOutput("availcounty"),
                     
                    
                     sliderInput(inputId="year",
                               label= "Year", min=2000, max=2013,
                              value=c(2000,2013), format="####",ticks=TRUE),
                     
                     downloadButton('downloadData', 'Download'),
                     
                    # plotOutput("side_plot",width="85%")
                     plotOutput("sideplot1",width="100%")
                     ),
    
  
      conditionalPanel(condition="input.contab==2",
                  selectInput(inputId ="county",
                             label = "Choose a county:",
                               choices = sort(unique(wswd_names$x)), selected="Lake"),
                       
                       sliderInput(inputId="Year",
                                   label= "Year", min=2006, max=2013,
                                   value=c(2006,2013), format="####",ticks=TRUE),
                       
                       sliderInput(inputId="Month",
                                   label= "Month", min=1, max=12,
                                   value=c(1,12), format="##",ticks=TRUE)
          
                       
                      , plotOutput("sideplot2",width="100%")
                  ),
    
      conditionalPanel(condition="input.contab==3"
                       ,
                       
                       selectInput("param", "Choose a parameter:", 
                                  choices=c(sort(unique(xact_param))),
                                  selected="Manganese"),
#                        dateInput("Date", label="Choose a date:", value = 2012-10-25, min = 2012-10-25,
#                                  max = 2013-04-07, format = "yyyy-mm-dd", startview = "month",
#                                  weekstart = 0, language = "en"),
                       
                       h6("Map of Gary Xact Monitor and Surrounding Area"),
                       
#                        showOutput('myChart3','leaflet')
                       htmlOutput("sideplot3")
                     ),
    
    conditionalPanel(condition="input.contab==4"
                      ,
                    
                    
                     uiOutput("availcomp"),
                     uiOutput("availemisscounty"),
                     uiOutput("inRadio")
                     
                     
                     
                    # plotOutput("sideplot4",width="100%")
  )

    ,
    conditionalPanel(condition="input.contab==5"
                     ,
          h6("this is info about the tool")

                     
    )
    )
  ,              
 titlePanel("Indiana Metals Data"),
     
 
plotPanel(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
# mainPanel(
    
  #  progressInit(),
    tabsetPanel(
    tabPanel("Statewide Metals Data", value=1,       
             
             plotOutput("main_plot",height="auto")), 
    
    
    tabPanel("PM 2.5", value=2,
             
             plotOutput("main_plot2",height="auto")),
    
    tabPanel("Xact Monitoring",value=3,
             
#              showOutput("mychart_ts","nvd3")),
             plotOutput("main_plot3",height="auto")),
    
    tabPanel("Emissions",value=4
             ,
             h5("Metals Emissions by Facility"),
             
            # plotOutput("sideplot4",width="100%"),
             showOutput('myChart2','leaflet')
             ),
    tabPanel("About",value=5,
             h6("this is info about the tool"))
    ,
             
             
                id="contab")
  )
  )
        )



