library(shiny)
library(ggbiplot)

ui <- navbarPage("HW4 by Wilson Shih",
  tabPanel("PCA", fluidRow(
    tabsetPanel(
      tabPanel("PCA Plot", fluidRow(
        column(4,  sidebarPanel(
          h4("choose what you wnt to see on 'PCA Result : Plot'"),
          selectInput(inputId = "variableX", label = "X variable", choices = c("PC1", "PC2", "PC3", "PC4"), selected = "PC1"),
          selectInput(inputId = "variableY", label = "Y variable", choices = c("PC1", "PC2", "PC3", "PC4"), selected = "PC2"),
          width=12
        )),
        column(8, plotOutput("plotPCA"))
      )),
      tabPanel("PCA Table", fluidRow(
        column(5, h3("PCA Result Summery:"), verbatimTextOutput("summaryPCA"), h3("Rotation Table"), tableOutput("rotation")),
        column(7, h3("x Table"), dataTableOutput("resultPCA")),
      ))
    )
  )),
  tabPanel("CA", fluidRow(
    column(4, sidebarPanel(sliderInput("caK","CA(use kmean)", value=3, min=3, max=10, step=1), width=12)),
    column(8, tabsetPanel(
      tabPanel("CA Plot", fluidRow(column(8, plotOutput("plotCA")))),
      tabPanel("CA Summary", fluidRow(column(8, h4("Scree"), tableOutput("scree"), h4("Rows"), tableOutput("rows"),h4("columns"), tableOutput("columns"))))
    ))
  )),
  tabPanel("Row Data", fluidRow(
    column(4, verbatimTextOutput("summaryRowData")),
    column(8, dataTableOutput("rowData"))
  ))

)

server <- function(input, output) {
  data(iris)
  # log transform 
  log.ir <- log(iris[, 1:4])
  ir.species <- iris[, 5]

  # apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
  ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
  ir_pca_result <- predict(ir.pca, newdata=log.ir)
  ir_pca_summary <- summary(ir.pca)
  iris_sum_output <- capture.output(summary(iris))
  ir_pca_summary_importance <- capture.output(ir_pca_summary$importance)
  
  # set pca x & y variable
  observe({
    variable_x <- switch(input$variableX, "PC1"=1, "PC2"=2, "PC3"=3, "PC4"=4)
    variable_y <- switch(input$variableY, "PC1"=1, "PC2"=2, "PC3"=3, "PC4"=4)
    g <- ggbiplot(ir.pca, choices = c(variable_x, variable_y),obs.scale = 1, var.scale = 1, groups = ir.species, circle=TRUE, ellipse=FALSE)
    g <- g + scale_color_discrete(name = '')
    g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
    output$plotPCA <- renderPlot({g}, width = "auto", height = "auto")
  })
  
  observe({
    kmeans_center <- input$caK
    model <- kmeans(iris[, 1:4], centers = kmeans_center)
    table(iris$Species, model$cluster)
    iris.ca <- ca(table(iris$Species, model$cluster), nd = 2)
    ca_summary <- summary(iris.ca)
    output$plotCA <- renderPlot({plot(iris.ca)})
    # output$summaryCA <- renderPrint({ca_summary$scree})
    output$scree <- renderTable({ca_summary$scree})
    output$rows <- renderTable({ca_summary$rows})
    output$columns <- renderTable({ca_summary$columns})
  })
  # g <- ggbiplot(ir.pca, choices = c(variable_x, variable_y),obs.scale = 1, var.scale = 1, groups = ir.species, circle=TRUE, ellipse=FALSE)
  # g <- g + scale_color_discrete(name = '')
  # g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
  #  g <- getPCA(input=input)
  # output$plotPCA <- renderPlot({g}, width = "auto", height = "auto")


  output$summaryPCA <- renderPrint({
    # paste(ir_pca_summary$importance, collapse = '\n')
    ir_pca_summary_importance
    # summary(ir.pca)
  })

  output$rotation <- renderTable({
    ir.pca$rotation
  })

  output$resultPCA <- renderDataTable({
    ir_pca_result
  })
  
  output$summaryRowData <- renderPrint({
    
    iris_sum_output
  })

  output$rowData <- renderDataTable({
    iris
  })


}

# 运行Shiny应用程序
shinyApp(ui, server)