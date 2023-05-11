library(shiny)
library(ggbiplot)
library(ca)
library(corrplot)
library("FactoMineR")
library("factoextra")

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
                   tabsetPanel(
                     tabPanel("CA(Kmean)", fluidRow(
                       column(4, sidebarPanel(sliderInput("caK","CA(use kmean)", value=3, min=3, max=10, step=1), width=12),
                              h4("Scree"), tableOutput("scree"), br(" "), br(" "),h4("Rows"), tableOutput("rows"),h4("columns"), tableOutput("columns")
                       ),
                       column(8, plotOutput("plotCA"))
                     )),
                     tabPanel("CA(FactoMiner package)", fluidRow(
                       column(3,  sidebarPanel(
                         h4("choose how many input you wnt to see on CA"),
                         sliderInput("ca2", "", value=100, min=1, max=150, step=1)),
                         width=12)
                     ),
                     column(9, plotOutput("plotCA2"), verbatimTextOutput("summaryCA2"))
                     )
                   )
                 )),
                 tabPanel("Row Data", fluidRow(
                   column(5, h4('Summary'), verbatimTextOutput("summaryRowData"), h4('Correlations'), plotOutput("plotCorr")),
                   column(7, dataTableOutput("rowData"))
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
    g <- g + theme(legend.direction = 'horizontal', legend.position = 'top', plot.title = element_text(size=20), axis.title = element_text(size=24), legend.title = element_text(size=16), legend.text = element_text(size=16) )
    output$plotPCA <- renderPlot({g}, width = 800, height =600)
  })
  
  observe({
    kmeans_center <- input$caK
    model <- kmeans(iris[, 1:4], centers = kmeans_center)
    table(iris$Species, model$cluster)
    iris.ca <- ca(table(iris$Species, model$cluster), nd = 2)
    ca_summary <- summary(iris.ca)
    output$plotCA <- renderPlot({plot(iris.ca, arrows = c(FALSE, TRUE))})
    # output$summaryCA <- renderPrint({ca_summary$scree})
    output$scree <- renderTable({ca_summary$scree})
    output$rows <- renderTable({ca_summary$rows})
    output$columns <- renderTable({ca_summary$columns})
  })
  
  observe({
    # output$summaryCA2 <- renderPrint(input$ca2)
    iris.ca <- CA(iris[1:input$ca2, 1:4], graph=FALSE)
    # fviz_screeplot(iris.ca, addlabels = TRUE, ylim = c(0, 50))
    # fviz_ca_biplot(res.ca, repel = TRUE)
    output$plotCA2 <- renderPlot({fviz_ca_biplot(iris.ca, repel = TRUE)})
    output$summaryCA2 <- renderPrint(summary(iris.ca))
  })
  
  output$summaryPCA <- renderPrint({a
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
  
  output$plotCorr <- renderPlot({
    corrplot(cor(iris[, 1:4]), type='full', method='color', tl.col='black', tl.srt=46, tl.cex=0.8)
  })
  
}

shinyApp(ui, server)