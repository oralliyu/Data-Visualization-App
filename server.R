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

shinyServer(function(input, output, session) {
  observeEvent(input$info0,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Move the sliders or select from the dropdown menus and view the R code that produces the results.",
      type = "info"
    )
  })
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Move the sliders or select from the dropdown menus and view the R code that produces the results.",
      type = "info"
    )
  })
  observeEvent(input$go2, {
    updateTabItems(session, 'tabs', 'exp4')
  })
  
  ############ Data Visualization ############
  
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
  
  # #b. worldMap
  # output$worldMapOut1 <- renderPlot ({
  #   gdpData <- gdpData %>% mutate(GDPOption = ntiles(-GDP, input$worldMap1, format = "rank"))
  #   mWorldMap(gdpData, key = "country", fill = "GDPOption")
  # })
  # 
  # output$worldMapCode1 <- renderUI ({
  #   tags$code('gdpData <- gdpData %>% mutate(GDPOption = ntiles(-GDP,', input$worldMap1, ', format = "rank"))')
  # })
  # 
  # output$worldMapCode2 <- renderUI ({
  #   tags$code('mWorldMap(gdpData, key = "country", fill = "GDPOption")')
  # })
  
  ###### 3D Plots ######
  #a. Normal Simulation via Plotly
  output$plotly1 <- renderPlotly ({
    plot_ly(x = rnorm(input$Exsel1), y = rnorm(input$Exsel1), z = rnorm(input$Exsel1), 
            type = 'scatter3d', mode = 'markers')
  })
  
  output$ExCode <- renderUI ({
    tags$code('plot_ly(x = rnorm(', input$Exsel1, '), y = rnorm(', input$Exsel1, '), z = rnorm(', input$Exsel1, '), type = "scatter3d", mode = "markers")')
  })
  
  # output$plotly1 <- renderPlotly ({
  #   if (input$Exsel == 'Scatter Plot') {
  #     plot_ly(x = rnorm(input$Exsel1), y = rnorm(input$Exsel1), z = rnorm(input$Exsel1), 
  #             type = 'scatter3d', mode = 'markers')
  #   }
  #   else if (input$Exsel == 'Line Plot') {
  #     plot_ly(x = rnorm(input$Exsel1), y = rnorm(input$Exsel1), z = rnorm(input$Exsel1), 
  #             type = 'scatter3d', mode = 'lines') 
  #   }
  #   else {
  #     plot_ly(x = rnorm(input$Exsel1), y = rnorm(input$Exsel1), z = rnorm(input$Exsel1), 
  #             type = 'mesh3d', mode = 'markers') 
  #   }
  # })
  
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