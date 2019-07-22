library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinyDND)
library(shinyWidgets)
library(dplyr)
library(mosaic)
library(plotly)
library(ggplot2)
library(EDAWR)
library(plot3D)
library(ggmap)
#library(datasets)
library(shinyAce)
library(shinycssloaders)
library(rlocker)

source("helpers.R")

header = dashboardHeader(title = 'Data Visualization',
                         tags$li(class = "dropdown",
                                 tags$a(href = "https://shinyapps.science.psu.edu/",
                                        icon("home"))),
                         tags$li(class = "dropdown",
                                 actionLink("info", icon("info"), class = "myClass")))

sidebar = dashboardSidebar(
  sidebarMenu(id = 'tabs',
              menuItem('Overview', tabName = 'overview', icon = icon("dashboard")),
              menuItem('Simple Data Visualization', tabName = 'VisualOne', 
                       icon = icon('wpexplorer')),
              menuItem('Advanced Data Visualization', tabName = 'exp4', icon = icon('wpexplorer'))
  )
)

body = dashboardBody(
  tags$head( 
    tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css")
  ),
  tags$style(type = "text/css", ".content-wrapper,.right-side {background-color: white;}"),
  
  useShinyjs(),
  tabItems(
    tabItem(tabName = 'overview',
            tags$a(href='http://stat.psu.edu/', tags$img(src = 'psu_icon.jpg', align = "left", width = 180)),
            br(),
            br(),
            br(),
            h3(strong('About:')),
            h4('This app illustrates R code for data visualization.'),
            br(),
            
            h3(strong('Instructions:')),
            h4(tags$li("Simple data visualization section introduces
                       how to create some common use plots with ggplot and Rplot with exercise at the end.")),
            h4(tags$li("Advanced Data Visualization section
                      introduces 3D plots, line plots, contour plots, and heat maps.")),
            br(),
            div(style = 'text-align: center', 
                bsButton(inputId = 'go2', label = 'Explore', 
                         icon = icon('bolt'), size = 'large', class='circle grow')),
            br(),
            h3(strong('Acknowledgements:')),
            h4('This application was coded and developed by Anna (Yinqi) Zhang in 2018 and Yiyun Gong in 2019. 
               Special Thanks to Grace (Yubaihe) Zhou for being incredibly helpful with programming issues.'),
            h4('Packages used: EDAWR, ggmap, mosaic, plotly, ggplot2, plot3D, shinyAce.')
    ),
    
    ############ Data Visualization Introduction #######
    ######Characterizing one single Variable######
    
    tabItem(tabName = 'VisualOne',
            # div(style="display: inline-block;vertical-align:top;",
            #     tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 19))
            # ),
            # div(style="display: inline-block;vertical-align:top;",
            #     circleButton("info0",icon = icon("info"), status = "myClass",size = "xs")
            # ),
            
            tabsetPanel(type = 'tabs',
                        ###### One Variable ######
                        tabPanel('Single Variable',
                                 h3(strong('One Variable Visualization')),
                                 # br(),
                                 h4('This section illustrates R code for data 
                                    visulization includes plot() and ggplot() with one Variable'),
                                 
                                 br(),
                                 sidebarLayout(
                                   sidebarPanel(
                                     id="sidebar",
                                     tags$head(tags$style(
                                       HTML('#sidebar{
                                                background-color: #b8f28c;
                                            }')
                                     )),
                                     ####select between plot and ggplot
                                     selectInput(inputId="plotType", label="select Plot Type",
                                                 choices = c('plot', 'ggplot'),
                                                 selected = 'plot'),
                                     
                                     ####select datasets
                                     selectInput(inputId="dataset", label="Select Dataset:", 
                                                 choices= c('cars', 'trees'), 
                                                 selected = 'cars'),
                                     
                                     ####variable options for 'car' dataset
                                     conditionalPanel(
                                       condition = "input.dataset == 'cars'",
                                       selectInput(inputId="carsVariable", label="Select Variables",
                                                   choices = c("speed", "dist"),
                                                   selected = 'speed')
                                     ),
                                     
                                     ####variable option for 'trees' dataset
                                     conditionalPanel(
                                       condition = "input.dataset == 'trees'",
                                       selectInput(inputId="treesVariable", label="Select Variables",
                                                   choices = c("Girth", "Height", "Volume"),
                                                   selected = 'Girth')
                                     ),
                                     #tags$img(src="DataView.pdf")
                                     #includeHTML("ViewData.nb.html")
                                     #tags$a(tags$img(src="pdficon.png"), href="DataView.pdf", download="Viewdata.pdf")
                                     # br(),
                                     #downloadLink("downloadData", "Preview of Data"),
                                     
                                       checkboxInput("previewData", "Preview of Datasets")
                                       
                                   ),
                                   
                                   mainPanel(
                                     conditionalPanel(
                                       condition="input.previewData==1",
                                       fluidRow(
                                         column(2, p(strong("Dataset cars"))),
                                         column(4, tableOutput("Previewcar")),
                                         column(2, p(strong("Dataset trees"))),
                                         column(4, tableOutput("Previewtree"))
                                       )
                                     ),
                                     fluidRow(
                                       column(6,plotOutput(outputId="oneDensity", width="100%",height="300px")%>% withSpinner(color="#1E7B14")),  
                                       column(6,plotOutput(outputId="onehist", width="100%",height="300px")%>% withSpinner(color="#1E7B14"))
                                     ),
                                     fluidRow(
                                       column(width = 6, textOutput(outputId="DensityoneCode")),
                                       column(width = 6, textOutput(outputId="HistogramoneCode"))
                                     ),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     tags$head(tags$style("#qqCode, #BarCode, #DensityoneCode, #HistogramoneCode,
                                                          #twoscattercode, #logTransformationCode, #twobarcode, #twoboxcode
                                                          {color: #005485}"
                                     )),
                                     
                                     
                                     fluidRow(
                                       column(6,plotOutput(outputId="onebar", width="100%",height="300px")%>% withSpinner(color="#1E7B14")),  
                                       column(6,plotOutput(outputId="oneqq", width="100%",height="300px")%>% withSpinner(color="#1E7B14"))
                                     ),
                                     fluidRow(
                                       column(width = 6, textOutput(outputId="BarCode")),
                                       column(width = 6, textOutput(outputId="qqCode"))
                                     ),
                                     br(),
                                     br()
                                     # div(style = 'text-align: center', 
                                     #     bsButton(inputId = 'next2', label = 'Next Section', 
                                     #              icon = icon('angle-double-right'), size = 'small', 
                                     #              class='circle grow'))
                                   )
                                 )
                                 ),
                        
                        ###### Two Variable ######
                        tabPanel(title='Two Variables', value='panel2',
                                 h3(strong('Two Variables Visualization')),
                                 # br(),
                                 h4('This section illustrates R code for data 
                                    visulization uses ggplot() with Two Variables'),
                                 
                                 br(),
                                 sidebarLayout(
                                   sidebarPanel(
                                     id="sidebar",
                                     tags$head(tags$style(
                                       HTML('#sidebar{
                                                background-color: #b8f28c;
                                            }')
                                     )),
                                     ####select continuous variable 1
                                     selectInput(inputId="continuous1", 
                                                 label="Select First Continuous Variable as X:",
                                                 choices= c('Sepal.Length', 
                                                            'Sepal.Width'),
                                                 selected = 'Sepal.Length'),
                                     
                                     selectInput(inputId="continuous2", 
                                                 label="Select Second Continuous Variable as Y:",
                                                 choices= c('Petal.Length', 
                                                            'Petal.Width'),
                                                 selected = 'Petal.Length'),
                                     
                                     selectInput(inputId="CategoryVar", 
                                                 label="Select Categorical Variable:",
                                                 choices= 'Species',
                                                 selected = 'Species'),
                                     
                                     checkboxInput("previewDataTwo", "Preview of Datasets")
                                     
                                     
                                   ),
                                   
                                   mainPanel(
                                     conditionalPanel(
                                       condition="input.previewDataTwo==1",
                                       fluidRow(
                                         column(2, p(strong("Dataset iris"))),
                                         column(5, tableOutput("Previewiris"))
                                       )
                                       #tableOutput("Previewiris")
                                       #p("First four rows of dataset iris")
                                     ),
                                     fluidRow(
                                       column(6,plotOutput(outputId="twoscatter")%>% withSpinner(color="#1E7B14")),
                                       column(6,plotOutput(outputId="logTransformation")%>% withSpinner(color="#1E7B14"))
                                     ),
                                     br(),
                                     fluidRow(
                                       column(6,textOutput(outputId="twoscattercode")),
                                       column(6,textOutput(outputId="logTransformationCode"))
                                     ),
                                     br(),
                                     fluidRow(
                                       column(6,plotOutput(outputId="twobar")%>% withSpinner(color="#1E7B14")),
                                       column(6,plotOutput(outputId="twobox")%>% withSpinner(color="#1E7B14"))
                                     ),
                                     br(),
                                     fluidRow(
                                       column(6,textOutput(outputId="twobarcode")),
                                       column(6,textOutput(outputId="twoboxcode"))
                                     )
                                   )
                                 )
                                 ),
                        
                        tabPanel(title='Exercises', value='panel2',
                                 #uiOutput("urltest"))
                                 #includeMarkdown("test.Rmd")
                                 #system.file("knitr", package="shinyAce")
                                 
                                 fluidRow(
                                   column(6,
                                          verticalLayout(
                                            h2("Instructions"),
                                            wellPanel(
                                              style = "background-color: #9ff28c",
                                            tags$div(tags$ul(
                                              tags$li("You can try the following questions"),
                                              tags$li("Test your code with the following R script
                                             box with the RMarkDown output on the right side"),
                                              tags$li("In each turn, 10 questions will be randomly draw from the question bank."),
                                              tags$li("Uncomment the sample code to start to explore.")
                                              ),
                                              style = "background-color: #9ff28c")),
                                            h2("Exercises"),
                                            uiOutput('progress'),
                                            wellPanel(style = "background-color: #b8f28c",
                                                      uiOutput("question")%>% withSpinner(color="#1E7B14"),
                                                      uiOutput("options"),
                                                      br(),
                                                      selectInput("answer", "pick an answer from below", c("","A", "B", "C")),
                                                      uiOutput("mark"),
                                                      tags$style(type='text/css', '#question{font-size: 15px;
                                                                 background-color: #b8f28c;color: black;}',
                                                                 '.well { padding: 10px; margin-bottom: 15px; max-width: 1000px; }')
                                                      
                                            ),
                                          fluidPage(
                                            tags$head(
                                              tags$style(HTML('#submit{background-color:#5a992b; color:white}')),
                                              tags$style(HTML('#eval{background-color:#5a992b; color:white}')),
                                              tags$style(HTML('#nextq{background-color:#5a992b; color:white}'))
                                            ),
                                            fluidRow(
                                              column(12, align="center",
                                                     div(style="display: inline-block", actionButton(inputId = 'submit', label = 'Submit', style="success")),
                                                     div(style="display: inline-block;vertical-align:top; width: 30px;",HTML("<br>")),
                                                     div(style="display: inline-block", bsButton(inputId = "nextq",label = "Next", disabled = TRUE)),
                                                     div(style="display: inline-block;vertical-align:top; width: 30px;",HTML("<br>")),
                                                     div(style="display: inline-block", bsButton(inputId = "reset",label = "Restart", style="danger", disabled = TRUE)))
                                            )),
                                          
                                          
                                          # column(3,
                                          #        actionButton(inputId = 'submit', label = 'Submit', style="success")
                                          # ),
                                          # column(3,
                                          #        bsButton(inputId = "nextq",label = "Next", style='warning', disabled = TRUE)
                                          # ),
                                          # column(3,
                                          #        bsButton(inputId = "reset",label = "Restart", style="danger", )
                                          # )),
                                          br(),
                                          
                                          ##########try rlocker statement#########
                                          tags$samp(
                                            htmlOutput("statements")
                                          ),
                                          ##########end#############
                                          
                                          h2("Try Your Code"),  
                                          aceEditor("rmd", mode="markdown", value='This is some markdown text. It may also have embedded R code
which will be executed. Please also read the output 
message for more hints.

you can add a new code chuck with following two lines
```{r}
```
```{r}
#structure on datasets we used in previous cases
str(cars)
str(trees)
str(iris)
```
It can even include graphical elements.
```{r}
#ggplot with one variable
#ggplot(aes(x=dist), data=cars)+geom_histogram()
```
```{r}
#ggplot with two variable
#ggplot(aes(x=Sepal.Length, y=Petal.Length), data=iris)+
#geom_line()
```
```{r}
#Rplot with one variable
plot(cars$speed)
```
'),
                                          column(6,
                                                 withBusyIndicatorUI(
                                                 actionButton("eval", "Run")))
                                          )),
                                   br(),
                                   column(6,
                                          h2("Knitted Output"),
                                          htmlOutput("knitDoc")
                                         )
                                 )
                        )
            )
    ),
    ######Advanced
    tabItem(tabName = 'exp4',
            tabsetPanel(type = 'tabs',
                        
                        ###### Maps ######
                        tabPanel('Maps',
                                 br(),
                                 box(title = NULL, style = 'background-color: #dce775', width = NULL, height = NULL,
                                     
                                     selectInput(inputId = 'mapsOp', label = 'Make a US/World Map with ggplot2',
                                                 choices = c('US Map - ggplot2', 'US Map - plotly'), selected = 'US Map'),
                                     bsPopover(id = 'mapsOp', title = " ", content = 'mUSMap takes in one dataframe that includes information about different
                                               US states and returns this data or a ggplot object constructed with the data. mWorldMap does the same but it
                                               takes in one dataframe that includes information about different countries.', trigger = 'click'),
                                     
                                     # conditionalPanel('input.mapsOp == "World Map"',
                                     #                  sliderInput(inputId = 'worldMap1', label = 'The Number of Color Scales', min = 1, max = 10,
                                     #                              value = 5, step = 1, ticks = TRUE)
                                     #                  ),
                                     
                                     conditionalPanel('input.mapsOp == "US Map - ggplot2"',
                                                      selectInput(inputId = 'usMap1', label = 'Plot Option', choices = c('borders', 'frame')),
                                                      selectInput(inputId = 'usMap2', label = 'Style Option', choices = c('compact', 'real'))
                                     )
                                     
                                 ),
                                 
                                 box(title = NULL, style = 'background-color: #f0f4c3', width=NULL, height = NULL,
                                
                                     conditionalPanel('input.mapsOp == "US Map - ggplot2"',
                                                      tags$strong('R code: '),
                                                      uiOutput('usMapOut2'),
                                                      br(),
                                                      div(style = "height: 300px; width: 600px",plotOutput('usMapOut1')
                                                      )),
                                     
                                     conditionalPanel('input.mapsOp == "US Map - plotly"',
                                                      tags$strong('R code: '),
                                                      uiOutput('plotlyUScode'),
                                                      br(),
                                                      plotlyOutput('plotlyUSMap')
                                     )
                                 )
                        ),
                        
                        ###### 3D Plots ######
                        tabPanel('3D Plots',
                                 br(),
                                 fluidRow(
                                   column(width = 12,
                                          box(title = NULL, style = 'background-color: #e8eaf6', width = NULL, height = NULL,
                                              selectInput(inputId = 'Exsel', label = '3D Plot Type', 
                                                          choices = c('Normal Simulation via Plotly', '3D Basic Scatter Plot', '3D Texts Plot'),
                                                          # choices = c('Normal Simulation via Plotly', 'Basic Scatter Plot', 'Basic Scatter Plot Colored by Groups', 
                                                          #             '3D Plots with Confidence Intervals', '3D Texts Plot'),
                                                          selected = 'Normal Simulation via Plotly', multiple = FALSE),
                                              
                                              # bsPopover(id = 'Exsel', title = 'Understand the Graph Type', 
                                              #           content = 'Mesh plot generates a wireframe plot while the scatter plot generates a dotted plot.', 
                                              #           placement = 'bottom', trigger = 'hover'),
                                              
                                              #a. Normal Simulation via Plotly
                                              conditionalPanel('input.Exsel == "Normal Simulation via Plotly"',
                                                               sliderInput(inputId = 'Exsel1', label = 'Please Select Your Sample Size', min = 0, max = 100,
                                                                           value = 30, step = 1, ticks = TRUE)
                                              ),
                                              
                                              #b. Basic Scatter Plot
                                              conditionalPanel('input.Exsel == "3D Basic Scatter Plot"',
                                                               selectInput(inputId = 'basicX', label = 'Variable for X-Axis', choices = c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width'),
                                                                           selected = 'Sepal.Length', multiple = FALSE),
                                                               selectInput(inputId = 'basicY', label = 'Variable for Y-Axis', choices = c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width'),
                                                                           selected = 'Sepal.Width', multiple = FALSE),
                                                               selectInput(inputId = 'basicZ', label = 'Variable for Z-Axis', choices = c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width'),
                                                                           selected = 'Petal.Length', multiple = FALSE)
                                              ),
                                              #c.
                                              #d. 3D Plots with Confidence Intervals
                                              conditionalPanel('input.Exsel == "3D Plots with Confidence Intervals"',
                                                               selectInput(inputId = 'CIX', label = 'Variable for X-Axis', choices = c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width'),
                                                                           selected = 'Sepal.Length', multiple = FALSE),
                                                               selectInput(inputId = 'CIY', label = 'Variable for Y-Axis', choices = c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width'),
                                                                           selected = 'Sepal.Width', multiple = FALSE),
                                                               selectInput(inputId = 'CIZ', label = 'Variable for Z-Axis', choices = c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width'),
                                                                           selected = 'Petal.Length', multiple = FALSE)
                                              )
                                          )
                                   )
                                 ),
                                 fluidRow(
                                   column(width = 12,
                                          box(title = NULL, style = 'background-color: #9fa8da', width = NULL, height = NULL,
                                              
                                              #a. Normal Simulation via Plotly
                                              conditionalPanel('input.Exsel == "Normal Simulation via Plotly"',
                                                               tags$strong('R code: '),
                                                               uiOutput('ExCode'),
                                                               br(),
                                                               plotlyOutput('plotly1'),
                                                               br(),
                                                               verbatimTextOutput("hover"),
                                                               verbatimTextOutput("click")
                                              ),
                                              
                                              #b. Basic Scatter Plot
                                              conditionalPanel('input.Exsel == "3D Basic Scatter Plot"',
                                                               tags$strong('R code: '),
                                                               uiOutput('basicRcode'),
                                                               br(),
                                                               tableOutput('bspTable'),
                                                               plotOutput('bspOut1')
                                              ),
                                              
                                              # #c. Basic Scatter Plot Colored by Groups
                                              # conditionalPanel('input.Exsel == "Basic Scatter Plot Colored by Groups"',
                                              #                  tableOutput('bspTableCopy'),
                                              #                  plotOutput('bspOut2')
                                              # ),
                                              
                                              # #d. 3D Plots with Confidence Intervals
                                              # conditionalPanel('input.Exsel == "3D Plots with Confidence Intervals"',
                                              #                  plotOutput('CIOut')
                                              #                  ),
                                              
                                              #e. 3D Texts Plot
                                              conditionalPanel('input.Exsel == "3D Texts Plot"',
                                                               tags$strong('R code: '),
                                                               uiOutput('textRcode'),
                                                               br(),
                                                               tableOutput('textTable'),
                                                               plotOutput('textOut')
                                              )
                                              
                                          )
                                   )
                                 )
                        ),
                        
                        ###### 2D Line Plots ######
                        tabPanel('2D Line Plots',
                                 br(),
                                 fluidRow(
                                   column(width = 12,
                                          box(title = NULL, style = 'background-color: #e8eaf6', width = NULL, height = NULL,
                                              
                                              sliderInput(inputId = 'LPsel1', label = 'Please Set the Maximum of X-Axis', min = 0, max = 200,
                                                          value = 80, step = 1, ticks = TRUE),
                                              
                                              numericInput(inputId = 'LPnum1', label = 'Theoretical Mean of trace 0',
                                                           value = 10, step = 1),
                                              selectInput(inputId = 'LPSEL1', label = 'Please Select Your First Graph Mode',
                                                          choices = c('Lines', 'Markers')),
                                              numericInput(inputId = 'LPnum2', label = 'Theoretical Mean of trace 1',
                                                           value = -10, step = 1),
                                              selectInput(inputId = 'LPSEL2', label = 'Please Select Your Second Graph Mode',
                                                          choices = c('Lines', 'Markers'))
                                          )
                                   )
                                 ),
                                 fluidRow(
                                   column(width = 12,
                                          box(title = NULL, style = 'background-color: #9fa8da', width = NULL, height = NULL,
                                              tags$strong('R code: '),
                                              uiOutput('LPCode'),
                                              br(),
                                              plotlyOutput('plotly2')
                                          )
                                   )
                                 )
                        ),
                        
                        ###### Contour Plots & Heat Maps ######
                        tabPanel('Contour Plots & Heatmaps',
                                 br(),
                                 sidebarLayout(
                                   sidebarPanel(
                                     box(title = NULL, style = 'background-color: #f0f4c3', width = NULL, height = NULL,
                                         div('Heat maps and contour plots are visualization techniques to show data density on a map. They are particularly helpful when you have a lot of data points on the map and are mainly interested in their overall distribution.', style = 'color: blue'),
                                         br(),
                                         selectInput(inputId = 'chSel', label = 'Please Select Your Display Option', choices = c('', 'Contour Plots', 'Heatmaps'), selected = 'Contour Plots'),
                                         
                                         #contour plots
                                         conditionalPanel('input.chSel == "Contour Plots"',
                                                          selectInput(inputId = 'chSel2', label = 'View An Example', choices = c('Volcano', 'Protein-Protein Interaction')),
                                                          conditionalPanel('input.chSel2 == "Volcano"',
                                                                           checkboxInput(inputId = 'contourLabel', label = 'Add Contour Labels', value = FALSE)
                                                          )
                                         ),
                                         
                                         #heat maps
                                         conditionalPanel('input.chSel == "Heatmaps"',
                                                          selectInput(inputId = 'heat1', label = 'View An Example', choices = c('Volcano', 'Cars')),
                                                          conditionalPanel('input.heat1 == "Volcano"',
                                                                           sliderTextInput(inputId = 'heatmapCol', label = 'Please Select Your Colorscale', 
                                                                                           choices = c('purple+green', 'yellow+red', 'pink+purple', 'white+black'), grid = TRUE)
                                                          )
                                                          
                                         )
                                     )
                                   ),
                                   
                                   mainPanel(
                                     box(title = NULL, style = 'background-color: #dce775', width = NULL, height = NULL,
                                         conditionalPanel('input.chSel == "Contour Plots"',
                                                          conditionalPanel('input.chSel2 == "Volcano"',
                                                                           tags$b('In this section, we will use an embedded dataset named Volcano. It is a matrix containing 87 rows and 61 columns.'),
                                                                           br(),
                                                                           br(),
                                                                           tags$strong('R Code: '),
                                                                           uiOutput('CPCode1'), #volcano code
                                                                           br(),
                                                                           plotlyOutput('plotly3') #volcano plot
                                                          ),
                                                          conditionalPanel('input.chSel2 == "Protein-Protein Interaction"',
                                                                           plotOutput('proteinInt')
                                                          )
                                                          
                                         ),
                                         
                                         conditionalPanel('input.chSel == "Heatmaps"',
                                                          conditionalPanel('input.heat1 == "Volcano"',
                                                                           tags$strong('R Code: '),
                                                                           uiOutput('CPCode2'),
                                                                           br(),
                                                                           plotlyOutput('plotly4')
                                                          ),
                                                          conditionalPanel('input.heat1 == "Cars"',
                                                                           plotlyOutput('cars1')
                                                          )
                                         )
                                     )
                                   )
                                 )
                        )
            )
    )
    
  )
)

shinyUI(dashboardPage(skin = 'green', header, sidebar, body))

