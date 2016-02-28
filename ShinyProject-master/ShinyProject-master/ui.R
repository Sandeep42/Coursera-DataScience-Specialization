shinyUI(pageWithSidebar(
  headerPanel('mtcars k-means clustering'),
  sidebarPanel(
    h5("This app loads the mtcars dataset and performs a k-means clustering on it. To get started,
       please select the variables that you want to be seen."),
    
    selectInput('xcol', 'Please select the X Variable', names(mtcars)),
    selectInput('ycol', 'Please select the Y Variable', names(mtcars),
                selected=names(iris)[[2]]),
    numericInput('clusters', 'Cluster count', 3,
                 min = 1, max = 9)
  ),
  mainPanel(
    plotOutput('plot1')
  )
))