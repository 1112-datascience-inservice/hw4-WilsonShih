# # PCA
# getPCA <- function(input){
#   data(iris)
#   # log transform 
#   log.ir <- log(iris[, 1:4])
#   ir.species <- iris[, 5]

#   # apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
#   ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
#   print(ir.pca)
#   summary(ir.pca)
#   library(ggbiplot)
#   g <- ggbiplot(ir.pca, choice=c(1,4), obs.scale = 1, var.scale = 1, groups = ir.species, ellipse=TRUE)
#   g <- g + scale_color_discrete(name = '')
#   g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
#   print(g)
#   return(g)
# }

# Shiny
library(shiny)
library(DT)

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("HW4 by Wilson Shih!"),
  # fluidRow(
    
  #   column(3,
  #          h3("PCA"),
  #          h5("X aias"),
  #          submitButton("PC1"),
  #          submitButton("PC2"),
  #          submitButton("PC3"),
  #          h5("Y aias"),
  #          submitButton("PC1"),
  #          submitButton("PC2"),
  #          submitButton("PC3")),
    
  # ),
  tags$div(
    tags$div("X aias"),
    tags$button("PC1"), 
    tags$button("PC2"), 
    tags$button("PC3"), 
  ),
  tags$div(
    tags$div("Y aias"),
    tags$button("PC1"), 
    tags$button("PC2"), 
    tags$button("PC3"), 
  ),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of bins ----
      # sliderInput(inputId = "bins",
      #             label = "Number of bins:",
      #             min = 1,
      #             max = 50,
      #             value = 30)

    ),

    # Main panel for displaying outputs ----
    mainPanel(
      # plotOutput(outputId = "distPlot")
      tabsetPanel(type = "tabs",
                  tabPanel("PCA", plotOutput("pca")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Result Data", dataTableOutput("resultData")),
                  tabPanel("Input Data", dataTableOutput("data"))
      )
    )
    
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  data(iris)
  # log transform 
  log.ir <- log(iris[, 1:4])
  ir.species <- iris[, 5]

  # apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
  ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
  ir_pca_result <- predict(ir.pca, newdata=log.ir)
  summary(ir.pca)
  library(ggbiplot)
  g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species, ellipse=TRUE)
  g <- g + scale_color_discrete(name = '')
  g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
  # g <- getPCA(input=input)
  output$pca <- renderPlot({
    g
  })

  output$summary <- renderPrint({
    summary(iris)
  })
  output$resultData <- renderDataTable({
    ir_pca_result
  })
  output$data <- renderDataTable({
    log.ir
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
