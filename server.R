library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinyDND)
library(shinyWidgets)
library(dplyr)
library(EDAWR)
library(mosaic)
library(plot3D)
library(plotly)
library(ggplot2)
library(ggmap)
library(datasets)
library(learnr)
library(knitr)
library(rmarkdown)
library(shinyAce)
library(rlocker)

bank <- read.csv("questionbank.csv")
bank = data.frame(lapply(bank, as.character), stringsAsFactors = FALSE)

source("helpers.R")

shinyServer(function(input, output, session) {

  # Initialize Learning Locker connection
  connection <- rlocker::connect(session, list(
    base_url = "https://learning-locker.stat.vmhost.psu.edu/",
    auth = "Basic ZDQ2OTNhZWZhN2Q0ODRhYTU4OTFmOTlhNWE1YzBkMjQxMjFmMGZiZjo4N2IwYzc3Mjc1MzU3MWZkMzc1ZDliY2YzOTNjMGZiNzcxOThiYWU2",
    agent = rlocker::createAgent()
  ))
  
  # Setup demo app and user.
  currentUser <- 
    connection$agent
  
  if(connection$status != 200){
   warning(paste(connection$status, "\nTry checking your auth token.")) 
  }
  
  ##############end#########
  output$Previewcar<-
    renderTable({
      head(cars, 4)
    }, striped = TRUE, hover=TRUE, bordered = TRUE, spacing = 'xs')
  
  output$Previewtree<-
    renderTable({
      head(trees, 4)
    }, striped = TRUE, hover=TRUE, bordered = TRUE, spacing = 'xs')
  
  output$Previewiris<-
    renderTable({
      head(iris, 4)
    }, striped = TRUE, hover=TRUE, bordered = TRUE, spacing = 'xs')
  
  ###KNITR
  observeEvent(input$eval,{
    withBusyIndicatorServer("eval",{
      output$knitDoc <- renderUI({
        return(isolate(HTML(knit2html(text = input$rmd, fragment.only = TRUE, quiet = FALSE))))
      })
      
      output$output <- renderPrint({
        return(isolate(eval(parse(text=input$code))))
      })  
    })
  })
  
  output$knitDoc <- renderUI({
    input$eval
    return(isolate(HTML(knit2html(text = input$rmd, fragment.only = TRUE, quiet = FALSE))))
  })

  output$output <- renderPrint({
    input$eval
    return(isolate(eval(parse(text=input$code))))
  })
  
  observeEvent(input$info0,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Move the sliders or select from the dropdown menus and view the R code that produces the results.",
      type = NULL
    )
  })
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Move the sliders or select from the dropdown menus and view the R code that produces the results.",
      type = NULL
    )
  })
  observeEvent(input$go2, {
    updateTabItems(session, 'tabs', 'VisualOne')
  })
  
  observeEvent(input$next2, {
    updateTabsetPanel(session, 'VisualOne', selected = 'panel2')
  })
  
  ############ Data Visualization ############
  ###########One Single Variable Plot##############
  output$oneDensity<-
    renderPlot({
      if (input$dataset == 'cars'){
        if (input$carsVariable == 'speed'){
          if(input$plotType == 'plot'){
            plot(density(cars$speed), main = "Density Plot", xlab = input$carsVariable)
          }
          else if(input$plotType == 'ggplot'){
            ggplot(aes(speed), data=cars)+
              geom_density(color="darkblue", fill="lightblue", alpha=0.4)+
              ggtitle('Density Plot')
          }
        }
        else if(input$carsVariable == 'dist'){
          if(input$plotType == 'plot'){
            plot(density(cars$dist), main = "Density Plot", xlab = input$carsVariable)
          }
          else if(input$plotType == 'ggplot'){
            ggplot(aes(dist), data=cars)+
              geom_density(color="darkblue", fill="lightblue", alpha=0.4)+
              ggtitle('Density Plot')
          }
        }
      }
      else if (input$dataset == 'trees'){
        if (input$treesVariable == 'Girth'){
          if(input$plotType == 'plot'){
            plot(density(trees$Girth), main = "Density Plot", xlab = input$carsVariable)
          }
          else if(input$plotType == 'ggplot'){
            ggplot(aes(Girth), data=trees)+
              geom_density(color="darkblue", fill="lightblue", alpha=0.4)+
              ggtitle('Density Plot')
          }
        }
        else if(input$treesVariable == 'Height'){
          if(input$plotType == 'plot'){
            plot(density(trees$Height), main = "Density Plot", xlab = input$carsVariable)
          }
          else if(input$plotType == 'ggplot'){
            ggplot(aes(Height), data=trees)+
              geom_density(color="darkblue", fill="lightblue", alpha=0.4)+
              ggtitle('Density Plot')
          }
          
        }
        else if(input$treesVariable == 'Volume'){
          if(input$plotType == 'plot'){
            plot(density(trees$Volume), main = "Density Plot", xlab = input$carsVariable)
          }
          else if(input$plotType == 'ggplot'){
            ggplot(aes(Volume), data=trees)+
              geom_density(color="darkblue", fill="lightblue", alpha=0.4)+
              ggtitle('Density Plot')
          }
         
        }
      }
    })
  
  output$onehist<-renderPlot({
    if (input$dataset == 'cars'){
      if (input$carsVariable == 'speed'){
        if(input$plotType == 'plot'){
          hist(cars$speed, main = "Histogram", xlab = input$carsVariable)
        }
        else if(input$plotType == 'ggplot'){
          ggplot(aes(speed), data=cars)+
            geom_histogram(color="darkblue", fill="lightblue", alpha=0.4)+
            ggtitle("Histogram")
        }
      }
      else if(input$carsVariable == 'dist'){
        if(input$plotType == 'plot'){
          hist(cars$dist, main = "Histogram", xlab = input$carsVariable)
        }
        else if(input$plotType == 'ggplot'){
          ggplot(aes(dist), data=cars)+
            geom_histogram(color="darkblue", fill="lightblue", alpha=0.4)+
            ggtitle("Histogram")
        }
        
      }
    }
    else if (input$dataset == 'trees'){
      if (input$treesVariable == 'Girth'){
        if(input$plotType == 'plot'){
          hist(trees$Girth, main = "Histogram", xlab = input$carsVariable)
        }
        else if(input$plotType == 'ggplot'){
          ggplot(aes(Girth), data=trees)+
            geom_histogram(color="darkblue", fill="lightblue", alpha=0.4)+
            ggtitle("Histogram")
        }
        
      }
      else if(input$treesVariable == 'Height'){
        if(input$plotType == 'plot'){
          hist(trees$Height, main = "Histogram", xlab = input$carsVariable)
        }
        else if(input$plotType == 'ggplot'){
          ggplot(aes(Height), data=trees)+
            geom_histogram(color="darkblue", fill="lightblue", alpha=0.4)+
            ggtitle("Histogram")
        }
      }
      else if(input$treesVariable == 'Volume'){
        if(input$plotType == 'plot'){
          hist(trees$Volume, main = "Histogram", xlab = input$carsVariable)
        }
        else if(input$plotType == 'ggplot'){
          ggplot(aes(Volume), data=trees)+
            geom_histogram(color="darkblue", fill="lightblue", alpha=0.4)+
            ggtitle("Histogram")
        }
        
      }
    }
  })
  
  output$onebar<-
    renderPlot({
      if (input$dataset == 'cars'){
        if (input$carsVariable == 'speed'){
          if(input$plotType == 'plot'){
            barplot(cars$speed, main = "Bar Plot", xlab = input$carsVariable)
          }
          else if(input$plotType == 'ggplot'){
            ggplot(aes(speed), data=cars)+
              geom_freqpoly(bins = 30)+
              geom_area(stat = "bin", bins = 30,
                        color="darkblue", fill="lightblue", alpha=0.4)+
              ggtitle('Frequency polygon')
          }
        }
        else if(input$carsVariable == 'dist'){
          if(input$plotType == 'plot'){
            barplot(cars$dist, main = "Bar Plot", xlab = input$carsVariable)
          }
          else if(input$plotType == 'ggplot'){
            ggplot(aes(dist), data=cars)+
              geom_freqpoly(bins = 30)+
              geom_area(stat = "bin", bins = 30,
                        color="darkblue", fill="lightblue", alpha=0.4)+
              ggtitle('Frequency polygon')
          }
          
        }
      }
      else if (input$dataset == 'trees'){
        if (input$treesVariable == 'Girth'){
          if(input$plotType == 'plot'){
            barplot(trees$Girth, main = "Bar Plot", xlab = input$carsVariable)
          }
          else if(input$plotType == 'ggplot'){
            ggplot(aes(Girth), data=trees)+
              geom_freqpoly(bins = 30)+
              geom_area(stat = "bin", bins = 30,
                        color="darkblue", fill="lightblue", alpha=0.4)+
              ggtitle('Frequency polygon')
          }
        }
        else if(input$treesVariable == 'Height'){
          if(input$plotType == 'plot'){
            barplot(trees$Height, main = "Bar Plot", xlab = input$carsVariable)
          }
          else if(input$plotType == 'ggplot'){
            ggplot(aes(Height), data=trees)+
              geom_freqpoly(bins = 30)+
              geom_area(stat = "bin", bins = 30,
                        color="darkblue", fill="lightblue", alpha=0.4)+
              ggtitle('Frequency polygon')
          }
          
        }
        else if(input$treesVariable == 'Volume'){
          if(input$plotType == 'plot'){
            barplot(trees$Volume, main = "Bar Plot", xlab = input$carsVariable)
          }
          else if(input$plotType == 'ggplot'){
            ggplot(aes(Volume), data=trees)+
              geom_freqpoly(bins = 30)+
              geom_area(stat = "bin", bins = 30,
                        color="darkblue", fill="lightblue", alpha=0.4)+
              ggtitle('Frequency polygon')
          }
        }
      }
    })
  
  output$oneqq<-
    renderPlot({
      if (input$dataset == 'cars'){
        if (input$carsVariable == 'speed'){
          if (input$plotType == 'plot'){
            qqnorm(cars$speed)
            qqline(cars$speed, col='red')
          }
          else if(input$plotType == 'ggplot'){
            ggplot(aes(sample=speed), data=cars)+
              stat_qq(color="darkblue", fill="lightblue", alpha=0.4)+
              stat_qq_line(color='red')
          }
        }
        else if(input$carsVariable == 'dist'){
          if (input$plotType == 'plot'){
            qqnorm(cars$dist)
            qqline(cars$dist, col='red')
          }
          else if(input$plotType == 'ggplot'){
            ggplot(aes(sample=dist), data=cars)+
              stat_qq(color="darkblue", fill="lightblue", alpha=0.4)+
              stat_qq_line(color='red')
          }
        }
      }
      else if (input$dataset == 'trees'){
        if (input$treesVariable == 'Girth'){
          if (input$plotType == 'plot'){
            qqnorm(trees$Girth)
            qqline(trees$Girth, col='red')
          }
          else if(input$plotType == 'ggplot'){
            ggplot(aes(sample=Girth), data=trees)+
              stat_qq(color="darkblue", fill="lightblue", alpha=0.4)+
              stat_qq_line(color='red')
          }
        }
        else if(input$treesVariable == 'Height'){
          if (input$plotType == 'plot'){
            qqnorm(trees$Height)
            qqline(trees$Height, col='red')
          }
          else if(input$plotType == 'ggplot'){
            ggplot(aes(sample=Height), data=trees)+
              stat_qq(color="darkblue", fill="lightblue", alpha=0.4)+
              stat_qq_line(color='red')
          }
        }
        else if(input$treesVariable == 'Volume'){
          if (input$plotType == 'plot'){
            qqnorm(trees$Volume)
            qqline(trees$Volume, col='red')
          }
          else if(input$plotType == 'ggplot'){
            ggplot(aes(sample=Volume), data=trees)+
              stat_qq(color="darkblue", fill="lightblue", alpha=0.4)+
              stat_qq_line(color='red')
          }
        }
      }
    })
  
  output$DensityoneCode <- renderText({
    if (input$dataset == 'cars'){
      if (input$plotType == 'plot'){
        paste('plot(density(', input$dataset, '$', input$carsVariable, '))', seq='')
      }
      else if (input$plotType == 'ggplot'){
        paste("ggplot(aes(",input$carsVariable,"), data=cars)+
          geom_density(color='darkblue', fill='lightblue', alpha=0.4)+
          ggtitle('Density Plot')", seq='')
      }
    }
    else{
      if (input$plotType == 'plot'){
        paste('plot(density(', input$dataset, '$', input$treesVariable, ')', seq='')
      }
      else if (input$plotType == 'ggplot'){
        paste("ggplot(aes(",input$treesVariable,"), data=trees)+
              geom_density(color='darkblue', fill='lightblue', alpha=0.4)+
              ggtitle('Density Plot')", seq='')
      }
    }
  })
  
  output$HistogramoneCode <- renderText({
    if (input$dataset == 'cars'){
      if(input$plotType == 'plot'){
        paste('hist(', input$dataset, '$', input$carsVariable, ')', seq='')
      }
      else{
        paste("ggplot(aes(",input$carsVariable,"), data=cars)+
          geom_histogram(color='darkblue', fill='lightblue', alpha=0.4)+
          ggtitle('Histogram')", seq='')
      }
    }
    else{
      if(input$plotType == 'plot'){
        paste('hist(', input$dataset, '$', input$treesVariable, ')', seq='')
      }
      else{
        paste("ggplot(aes(",input$treesVariable,"), data=trees)+
              geom_histogram(color='darkblue', fill='lightblue', alpha=0.4)+
              ggtitle('Histogram')", seq='')
      }
    }
  })
  
  output$BarCode <- renderText({
    if (input$dataset == 'cars'){
      if (input$plotType == 'plot'){
        paste('barplot(', input$dataset, '$', input$carsVariable, ')', seq='')
      }
      else{
        paste("ggplot(aes(",input$carsVariable,"), data=cars)+
                geom_freqpoly(bins = 30)+
                geom_area(stat = 'bin', bins = 30,
                          color='darkblue', fill='lightblue', alpha=0.4)+
                ggtitle('Frequency polygon')")
      }
    }
    else{
      if (input$plotType == 'plot'){
        paste('barplot(', input$dataset, '$', input$treesVariable, ')', seq='')
      }
      else{
        paste("ggplot(aes(",input$treesVariable,"), data=trees)+
              geom_freqpoly(bins = 30)+
              geom_area(stat = 'bin', bins = 30,
              color='darkblue', fill='lightblue', alpha=0.4)+
              ggtitle('Frequency polygon')")
      }
    }
  })
  
  output$qqCode <- renderText({
    if (input$dataset == 'cars'){
      if (input$plotType == 'plot'){
        paste0('qqnorm(', input$dataset, '$', input$carsVariable, ')',
               '\n qqline(', input$dataset, '$', input$carsVariable, ')', seq='')
      }
      else{
        paste("ggplot(aes(sample=",input$carsVariable,"), data=cars)+
          stat_qq(color='darkblue', fill='lightblue', alpha=0.4)+
          stat_qq_line(color='red')", seq='')
      }
    }
    else{
      if (input$plotType == 'plot'){
        paste0('qqnorm(', input$dataset, '$', input$treesVariable, ')',
               'qqline(', input$dataset, '$', input$treesVariable, ')', seq='')
      }
      else{
        paste("ggplot(aes(sample=",input$treesVariable,"), data=trees)+
              stat_qq(color='darkblue', fill='lightblue', alpha=0.4)+
              stat_qq_line(color='red')", seq='')
      }
      
    }
  })
  ###########Two Variables########
  output$twoscatter<-renderPlot({
    if(input$continuous1=='Sepal.Length'){
      if(input$continuous2=='Petal.Length'){
        ggplot(aes(Sepal.Length, Petal.Length), data=iris)+
          geom_point(aes(colour = factor(Species)))+
          geom_smooth(aes(colour = factor(Species)), linetype='twodash', size=0.8)+
          ggtitle("Scatter Plot")
      }
      else if(input$continuous2=='Petal.Width'){
        ggplot(aes(Sepal.Length, Petal.Width), data=iris)+
          geom_point(aes(colour = factor(Species)))+
          geom_smooth(aes(colour = factor(Species)), linetype='twodash', size=0.8)+
          ggtitle("Scatter Plot")
      }
    }
    else if(input$continuous1=='Sepal.Width'){
      if(input$continuous2=='Petal.Length'){
        ggplot(aes(Sepal.Width, Petal.Length), data=iris)+
          geom_point(aes(colour = factor(Species)))+
          geom_smooth(aes(colour = factor(Species)), linetype='twodash', size=0.8)+
          ggtitle("Scatter Plot")
      }
      else if(input$continuous2=='Petal.Width'){
        ggplot(aes(Sepal.Width, Petal.Width), data=iris)+
          geom_point(aes(colour = factor(Species)))+
          geom_smooth(aes(colour = factor(Species)), linetype='twodash', size=0.8)+
          ggtitle("Scatter Plot")
      }
    }
  })
  
  
  output$logTransformation<-renderPlot({
    if(input$continuous1=='Sepal.Length'){
      if(input$continuous2=='Petal.Length'){
        ggplot(aes(Sepal.Length, Petal.Length), data=iris)+
          geom_point(aes(colour = factor(Species)))+
          coord_trans(x="log2", y="log2")+
          ggtitle("Log Transformation")
        #sunflowerplot(Sepal.Length~Petal.Length, data=iris, main="SunflowerPlot")
      }
      else if(input$continuous2=='Petal.Width'){
        ggplot(aes(Sepal.Length, Petal.Width), data=iris)+
          geom_point(aes(colour = factor(Species)))+
          coord_trans(x="log2", y="log2")+
          ggtitle("Log Transformation")
        #sunflowerplot(Sepal.Length~Petal.Width, data=iris, main="SunflowerPlot")
      }
    }
    else if(input$continuous1=='Sepal.Width'){
      if(input$continuous2=='Petal.Length'){
        ggplot(aes(Sepal.Width, Petal.Length), data=iris)+
          geom_point(aes(colour = factor(Species)))+
          coord_trans(x="log2", y="log2")+
          ggtitle("Log Transformation")
      }
      else if(input$continuous2=='Petal.Width'){
        ggplot(aes(Sepal.Width, Petal.Width), data=iris)+
          geom_point(aes(colour = factor(Species)))+
          coord_trans(x="log2", y="log2")+
          ggtitle("Log Transformation")
        #sunflowerplot(Sepal.Width~Petal.Width, data=iris, main="SunflowerPlot")
      }
    }
  })
  
  output$twobar<-renderPlot({
    if(input$continuous1=='Sepal.Length'){
      if(input$continuous2=='Petal.Length'){
        ggplot(data=iris, aes(x=Sepal.Length, y=Petal.Length, fill=factor(Species)))+
          geom_bar(stat="identity")+
          ggtitle('Bar Plot')
      }
      else if(input$continuous2=='Petal.Width'){
        ggplot(data=iris, aes(x=Sepal.Length, y=Petal.Width, fill=factor(Species)))+
          geom_bar(stat="identity")+
          ggtitle('Bar Plot')
      }
    }
    else if(input$continuous1=='Sepal.Width'){
      if(input$continuous2=='Petal.Length'){
        ggplot(data=iris, aes(x=Sepal.Width, y=Petal.Length, fill=factor(Species)))+
          geom_bar(stat="identity")+
          ggtitle('Bar Plot')
      }
      else if(input$continuous2=='Petal.Width'){
        ggplot(data=iris, aes(x=Sepal.Width, y=Petal.Width, fill=factor(Species)))+
          geom_bar(stat="identity")+
          ggtitle('Bar Plot')
      }
    }
  })

  
  output$twobox<-renderPlot({
    if(input$continuous1=='Sepal.Length'){
      if(input$continuous2=='Petal.Length'){
        ggplot(data=iris, aes(x=Sepal.Length, y=Petal.Length, color=Species)) +
          geom_boxplot()+
          ggtitle('Boxplot')
      }
      else if(input$continuous2=='Petal.Width'){
        ggplot(data=iris, aes(x=Sepal.Length, y=Petal.Width, color=Species)) +
          geom_boxplot()+
          ggtitle('Boxplot')
      }
    }
    else if(input$continuous1=='Sepal.Width'){
      if(input$continuous2=='Petal.Length'){
        ggplot(data=iris, aes(x=Sepal.Width, y=Petal.Length, color=Species)) +
          geom_boxplot()+
          ggtitle('Boxplot')
      }
      else if(input$continuous2=='Petal.Width'){
        ggplot(data=iris, aes(x=Sepal.Width, y=Petal.Width, color=Species)) +
          geom_boxplot()+
          ggtitle('Boxplot')
      }
    }
  })
  
  output$twoscattercode<-renderText({
    paste("ggplot(aes(",input$continuous1,',', input$continuous2, "), data=iris)+
      geom_point(aes(colour = factor(Species)))+
      geom_smooth(aes(colour = factor(Species)), linetype='twodash', size=0.8)+
      ggtitle('Scatter Plot')", seq='')
  }
  )
  
  output$logTransformationCode<-renderText({
    paste("ggplot(aes(", input$continuous1,',', input$continuous2, "), data=iris)+
      geom_point(aes(colour = factor(Species)))+
      coord_trans(x='log2', y='log2')+
      ggtitle('Log Transformation')", seq='')
  })
  
  output$twobarcode<-renderText({
    paste("ggplot(data=iris, aes(", input$continuous1,',', input$continuous2, 
          "fill=factor(Species)))+
                    geom_bar(stat='identity')+
                    ggtitle('Bar Plot')", seq='')
  })
  
  output$twoboxcode<-renderText({
    paste("ggplot(data=iris, aes(", input$continuous1,',', input$continuous2, "color=Species)) +
      geom_boxplot()+
      ggtitle('Boxplot')", seq='')
  })
  
  
  ###########Exercises Part###################
  # observeEvent(input$submit, {
  #   updateButton(session, "nextq", disabled = FALSE)
  # })
  # 
  # observeEvent(input$submit, {
  #   updateButton(session, "submit", disabled = TRUE)
  # })
  
  observeEvent(input$nextq, {
    # updateButton(session, "submit", disabled = FALSE)
    # updateButton(session, "nextq", disabled = TRUE)
    updateSelectInput(session,"answer", "pick an answer from below", c("","A", "B", "C"))
    output$mark <- renderUI({
      img(src = NULL,width = 30)
    })
  })
  
  
  #### question bank ####
  value <- reactiveValues(index =  1, mistake = 0,correct = 0)
  ans <- as.matrix(bank[1:14,6])
  #ans <- data.frame(ans)
  index_list<-reactiveValues(list=sample(1:14,10,replace=FALSE))
  
  observeEvent(input$nextq,{
    value$answerbox <- value$index
    index_list$list=index_list$list[-1]   
    value$index<-index_list$list[1]
    value$answerbox<-value$index

    updateButton(session, "nextq", disabled = TRUE)
    updateButton(session,"submit", disabled = FALSE)
  })
  
  output$question <- renderUI({
    h4(bank[value$index, 2])
    # radioButtons(inputId = bank[value$index,1], label= bank[value$index, 2], 
    #              choiceNames=c(bank[value$index, 3], bank[value$index, 4], bank[value$index, 5]), 
    #              choiceValues = c("A", "B", "C"))
  })
  
  output$options <- renderUI({
    str1 <- paste("A.", bank[value$index, 3])
    str2 <- paste("B.", bank[value$index, 4])
    str3 <- paste("C.", bank[value$index, 5])
    HTML(paste(str1, str2, str3, sep = '<br/>'))
    })
    
  
  observeEvent(input$answer,{
    req(input$answer, input$answer!='')
    answer<-isolate(input$answer)
    interacted_statement <- rlocker::createStatement(
      list(
        verb = list(
          display = "selected"
        ),
        object = list(
          id = paste0(getCurrentAddress(session), "#", value$index),
          name = paste('Question', value$index),
          description = bank[value$index, 2]
          
        ),
        result = list(
          success = any(answer == ans[value$index,1]),
          response = paste(getResponseText(value$index, answer))
        )
      )
    )
    
    # Store statement in locker and return status
    status <- rlocker::store(session, interacted_statement)
    
    print(interacted_statement) # remove me
    print(status) # remove me
  })
  
  
  getResponseText <- function(index, answer){
    if(answer == 'A'){
      key = 3
    } else if(answer == 'B'){
      key = 4
    } else {
      key = 5
    }
    return(bank[index, key])
  }
  
  observeEvent(input$submit,{
    if(length(index_list$list) == 1){
      updateButton(session, "nextq", disabled = TRUE)
      updateButton(session,"submit", disabled = TRUE)
      updateButton(session, "reset", disabled = FALSE)
    }
    else{
      updateButton(session, "nextq", disabled = FALSE)
      updateButton(session,"submit", disabled = TRUE)
      updateButton(session, "reset", disabled = FALSE)
    }
    
    # output$progress<-renderUI({
    #   paste("You are currently on problem", 11-length(index_list$list), "/10")
    # })
    
    answer<-isolate(input$answer)
    
    statement <- rlocker::createStatement(
      list(
        verb = list(
          display = "answered"
        ),
        object = list(
          id = paste0(getCurrentAddress(session), "#", value$index),
          name = paste('Question', value$index),
          description = bank[value$index, 2]
        ),
        result = list(
          success = any(answer == ans[value$index,1]),
          response = paste(getResponseText(value$index, answer), 
                           as.character(Sys.time()))
        )
      )
    )
    
    # Store statement in locker and return status
    status <- rlocker::store(session, statement)
    
    print(statement) # remove me
    print(status) # remove me
    
    output$mark <- renderUI({
        if (any(answer == ans[value$index,1])){
          img(src = "correct.png",width = 30)
        }
        else{
          ig<-img(src = "incorrect.png",width = 30)
          w<-paste("You picked", answer, ", The correct answer is", ans[value$index, 1])
          HTML(paste(ig, w), sep = ' ')
        }
    })
  })
  
  observeEvent(input$reset,{
    updateButton(session, "submit", disabled = FALSE)
    updateButton(session,"reset",disable =TRUE)
    updateSelectInput(session,"answer", "pick an answer from below", c("","A", "B", "C"))
    index_list$list<-c(index_list$list,sample(1:14,10,replace=FALSE))
    value$index <- 1
    value$answerbox = value$index
    ans <- as.matrix(bank[1:14,6])
    output$mark <- renderUI({
      img(src = NULL,width = 30)
    })
  })
  
  ###### Maps ######
  #a. usMap
  output$usMapOut1 <- renderPlot({
    USArrests2 <- USArrests %>% mutate(state = row.names(.))
    if (input$usMap1 == 'borders' & input$usMap2 == 'compact') {
      mUSMap(USArrests2, key = "state", fill = "UrbanPop", plot = 'borders', style = 'compact')
    }
    else if (input$usMap1 == 'borders' & input$usMap2 == 'real') {
      mUSMap(USArrests2, key = "state", fill = "UrbanPop", plot = 'borders', style = 'real')
    }
    else if (input$usMap1 == 'frame' & input$usMap2 == 'compact') {
      mUSMap(USArrests2, key = "state", fill = "UrbanPop", plot = 'frame', style = 'compact')
    }
    else {
      mUSMap(USArrests2, key = "state", fill = "UrbanPop", plot = 'frame', style = 'real')
    }
  })
  
  output$usMapOut2 <- renderUI ({
    tags$code('mUSMap(USArrests2, key = "state", fill = "UrbanPop", plot = "', input$usMap1, '", style = "', input$usMap2, '")')
  })
  
  #plotly US Map - code
  output$plotlyUScode <- renderUI ({
    tags$code('p <- plot_geo(df, locationmode = "USA-states", sizes = c(1, 250))')
  })
  
  #plotly US Map
  output$plotlyUSMap <- renderPlotly({
    df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_us_cities.csv')
    df$q <- with(df, cut(pop, quantile(pop)))
    levels(df$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
    df$q <- as.ordered(df$q)
    
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showland = TRUE,
      landcolor = toRGB("gray85"),
      subunitwidth = 1,
      countrywidth = 1,
      subunitcolor = toRGB("white"),
      countrycolor = toRGB("white")
    )
    
    p <- plot_geo(df, locationmode = 'USA-states', sizes = c(1, 250)) %>%
      add_markers(
        x = ~lon, y = ~lat, size = ~pop, color = ~q, hoverinfo = "text",
        text = ~paste(df$name, "<br />", df$pop/1e6, " million")
      ) %>%
      layout(title = '2014 US city populations<br>(Click legend to toggle)', geo = g)
    g
    p
  })
  
  
  ###### 3D Plots ######
  #a. Normal Simulation via Plotly
  output$plotly1 <- renderPlotly ({
    plot_ly(x = rnorm(input$Exsel1), y = rnorm(input$Exsel1), z = rnorm(input$Exsel1), 
            type = 'scatter3d', mode = 'markers')
  })
  
  output$ExCode <- renderUI ({
    tags$code('plot_ly(x = rnorm(', input$Exsel1, '), y = rnorm(', input$Exsel1, '), z = rnorm(', input$Exsel1, '), type = "scatter3d", mode = "markers")')
  })
  
  
  output$hover <- renderPrint({
    dataHover <- event_data("plotly_hover")
    if (is.null(dataHover)) {
      "Hover events appear here (unhover to clear)" 
    }
    else {
      dataHover
    }
  })
  
  output$click <- renderPrint({
    dataClick <- event_data("plotly_click")
    if (is.null(dataClick)) {
      "Click events appear here (double-click to clear)"
    }
    else {
      dataClick
    }
  })
  
  # output$ExCode <- renderUI ({
  #   if (input$Exsel == 'Scatter Plot') {
  #     tags$code('plot_ly(x = rnorm(', input$Exsel1, '), y = rnorm(', input$Exsel1, '), z = rnorm(', input$Exsel1, '), type = "scatter3d", mode = "markers")')
  #   }
  #   else if (input$Exsel == 'Line Plot') {
  #     tags$code('plot_ly(x = rnorm(', input$Exsel1, '), y = rnorm(', input$Exsel1, '), z = rnorm(', input$Exsel1, '), type = "scatter3d", mode = "lines")')
  #   }
  #   else {
  #     tags$code('plot_ly(x = rnorm(', input$Exsel1, '), y = rnorm(', input$Exsel1, '), z = rnorm(', input$Exsel1, '), type = "mesh3d", mode = "markers")')
  #   }
  # })
  
  #b. Basic Scatter Plot
  output$basicRcode <- renderUI ({
    tags$code('scatter3D(x, y, z, clab = c("Sepal", "Width (cm)"), xlab = input$basicX, ylab = input$basicY, zlab = input$basicZ)')
  })
  
  output$bspTable <- renderTable ({
    head(iris)
  })
  
  output$bspOut1 <- renderPlot({
    x <- iris[, input$basicX]
    y <- iris[, input$basicY]
    z <- iris[, input$basicZ]
    scatter3D(x, y, z, clab = c("Sepal", "Width (cm)"), xlab = input$basicX, ylab = input$basicY, zlab = input$basicZ)
  })
  
  # #c.
  # output$bspTableCopy <- renderTable ({
  #   head(iris)
  # })
  # 
  # output$bspOut2 <- renderPlot ({
  #   scatter3D(x, y, z, bty = "g", pch = 18, 
  #             col.var = as.integer(iris$Species), 
  #             col = c("#1B9E77", "#D95F02", "#7570B3"),
  #             pch = 18, ticktype = "detailed",
  #             colkey = list(at = c(2, 3, 4), side = 1, 
  #                           addlines = TRUE, length = 0.5, width = 0.5,
  #                           labels = c("setosa", "versicolor", "virginica")) )
  # })
  
  # #d. 3D Plots with Confidence Intervals
  # output$CIOut <- renderPlot ({
  #   x <- iris[, input$CIX]
  #   y <- iris[, input$CIY]
  #   z <- iris[, input$CIZ]
  #   CI <- list(z = matrix(nrow = length(x),
  #                         data = rep(0.1, 2*length(x))))
  #   scatter3D(x, y, z, phi = 0, bty = "g", col = gg.col(100), 
  #             pch = 18, CI = CI)
  # })
  
  #e. 3D Texts Plot
  output$textRcode <- renderUI ({
    tags$code('with(USArrests, text3D(Murder, Assault, Rape, 
              labels = rownames(USArrests), colvar = UrbanPop, 
              col = gg.col(100), theta = 60, phi = 20,
              xlab = "Murder", ylab = "Assault", zlab = "Rape", 
              main = "USA arrests", cex = 0.6, 
              bty = "g", ticktype = "detailed", d = 2,
              clab = c("Urban","Pop"), adj = 0.5, font = 2))')
  })
  
  output$textTable <- renderTable ({
    head(USArrests)
  })
  
  output$textOut <- renderPlot ({
    data(USArrests)
    with(USArrests, text3D(Murder, Assault, Rape, 
                           labels = rownames(USArrests), colvar = UrbanPop, 
                           col = gg.col(100), theta = 60, phi = 20,
                           xlab = "Murder", ylab = "Assault", zlab = "Rape", 
                           main = "USA arrests", cex = 0.6, 
                           bty = "g", ticktype = "detailed", d = 2,
                           clab = c("Urban","Pop"), adj = 0.5, font = 2))
  })
  
  ###### 2D Line Plots ######
  output$plotly2 <- renderPlotly ({
    trace_0 <- rnorm(as.numeric(input$LPsel1), mean = as.numeric(input$LPnum1))
    trace_1 <- rnorm(as.numeric(input$LPsel1), mean = as.numeric(input$LPnum2))
    x = c(1:as.numeric(input$LPsel1))
    data <- data.frame(x, trace_0, trace_1)
    if (input$LPSEL1 == 'Lines' & input$LPSEL2 == 'Lines') {
      plot_ly(data, x = ~x, y = ~trace_0, name = 'trace 0', type = 'scatter', mode = 'lines') %>%
        add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines')
    }
    else if (input$LPSEL1 == 'Markers' & input$LPSEL2 == 'Markers') {
      plot_ly(data, x = ~x, y = ~trace_0, name = 'trace 0', type = 'scatter', mode = 'markers') %>%
        add_trace(y = ~trace_1, name = 'trace 1', mode = 'markers')
    }
    else if (input$LPSEL1 == 'Lines' & input$LPSEL2 == 'Markers') {
      plot_ly(data, x = ~x, y = ~trace_0, name = 'trace 0', type = 'scatter', mode = 'lines') %>%
        add_trace(y = ~trace_1, name = 'trace 1', mode = 'markers')
    }
    else {
      plot_ly(data, x = ~x, y = ~trace_0, name = 'trace 0', type = 'scatter', mode = 'markers') %>%
        add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines')
    }
  })
  
  output$LPCode <- renderUI ({
    tags$code('plot_ly(data, x = ~x, y = ~trace_0, name = "trace 0", type = "scatter", mode = "lines") %>%
              add_trace(y = ~trace_1, name = "trace 1", mode = "markers + lines")')
  })
  
  ###### Contour Plots and Heatmaps ######
  #contour plot
  output$proteinInt <- renderPlot ({
    potentials <- as.matrix(read.table("MULTIPOT_lu.txt", row.names=1, header=TRUE))
    matrix.axes <- function(data) {
      # Do the rows, las=2 for text perpendicular to the axis
      x <- (1:dim(data)[1] - 1) / (dim(data)[1] - 1);
      axis(side=1, at=x, labels=rownames(data), las=2);
      # Do the columns
      x <- (1:dim(data)[2] - 1) / (dim(data)[2] - 1);
      axis(side=2, at=x, labels=colnames(data), las=2);
    }
    filled.contour(potentials, plot.axes=matrix.axes(potentials), main = "Protein-Protein Interaction Potential")
  })
  
  output$plotly3 <- renderPlotly({
    if (input$contourLabel == FALSE) {
      plot_ly(z = volcano, type = "contour", colors = colorRamp(c("purple", "green")))
    }
    else {
      plot_ly(z = volcano, type = "contour", colors = colorRamp(c("purple", "green")),
              contours = list(showlabels = TRUE))
    }
  })
  
  #contour plot r code
  output$CPCode1 <- renderUI ({
    if (input$contourLabel == FALSE) {
      tags$code('plot_ly(z = volcano, type = "contour", colors = colorRamp(c("purple", "green")))')
    }
    else {
      tags$code('plot_ly(z = volcano, type = "contour", colors = colorRamp(c("purple", "green")), contours = list(showlabels = TRUE))')
    }
  })
  
  #heatmap
  output$plotly4 <- renderPlotly({
    if (input$heatmapCol == 'purple+green') {
      plot_ly(z = volcano, type = "heatmap", colors = colorRamp(c("purple", "green")))
    }
    else if (input$heatmapCol == 'yellow+red') {
      plot_ly(z = volcano, type = "heatmap", colors = colorRamp(c("yellow", "red")))
    }
    else if (input$heatmapCol == 'pink+purple') {
      plot_ly(z = volcano, type = "heatmap", colors = colorRamp(c("pink", "purple")))
    }
    else {
      plot_ly(z = volcano, type = "heatmap", colors = colorRamp(c("white", "black")))
    }
  })
  
  #heatmaps r code
  output$CPCode2 <- renderUI ({
    if (input$heatmapCol == 'purple+green') {
      tags$code('plot_ly(z = volcano, type = "heatmap", colors = colorRamp(c("purple", "green")))') 
    }
    else if (input$heatmapCol == 'yellow+red') {
      tags$code('plot_ly(z = volcano, type = "heatmap", colors = colorRamp(c("yellow", "red")))') 
    }
    else if (input$heatmapCol == 'pink+purple') {
      tags$code('plot_ly(z = volcano, type = "heatmap", colors = colorRamp(c("pink", "purple")))') 
    }
    else {
      tags$code('plot_ly(z = volcano, type = "heatmap", colors = colorRamp(c("white", "black")))') 
    }
  })
  
  output$cars1 <- renderPlotly ({
    head(mtcars)
    data = as.matrix(mtcars)
    data=apply(data, 2, function(x){x/mean(x)})
    plot_ly(x=colnames(data), y=rownames(data), z = data, type = "heatmap")
  })
  
  })