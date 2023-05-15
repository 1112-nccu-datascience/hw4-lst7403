library(shiny)
library(ggplot2)
library(reshape2)
library(ggbiplot)
library(ca)
library("factoextra")

PCA_selection <- c("PC1" = 1, "PC2" = 2, "PC3" = 3, "PC4" = 4)

data(iris)
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]
ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)

rotation <- melt(ir.pca$rotation)

row.names(iris) <- 1:nrow(iris)
ca.ir <- ca(iris[, 1:4])

# UI
ui = fluidPage(
  fluidRow(
    navbarPage(title = "陳輝愛資料科學",
      tabPanel("basic dataset",
                  fluidRow(
                    h1("Iris Summary", align = "center", style="color:#E060FF")
                  ),
                  fluidRow(
                    column(12,
                      dataTableOutput('iris_summary')
                    )
                  ),
                  fluidRow(
                    h1("Iris Data", align = "center", style="color:#E060FF")
                  ),
                  fluidRow(
                    column(2,
                      img(src = "iris.png", width="100%", align="center")
                    ),
                    column(10,
                      dataTableOutput('iris_data')
                    )
                  )
      ),
      tabPanel("PCA",
        navlistPanel(widths = c(2,10),
          tabPanel("PCA Plot",
                  fluidRow(
                    column(6, align="center",
                      radioButtons("PCA_x", "X-axis", inline = T,
                                   PCA_selection)
                    ),
                    column(6, align="center",
                       radioButtons("PCA_y", "Y-axis", inline = T,
                                    PCA_selection)
                    )
                  ),
                  fluidRow(
                    plotOutput("PCA_plot")
                  )
          ),
          tabPanel("extended result",
                   fluidRow(
                     column(12, align="center",
                            fluidRow(
                              h1("PCA summary")
                            ),
                            fluidRow(
                              tableOutput("PCA_summary")
                            )
                     )
                   ),
                   fluidRow(
                     column(2, align="center",
                            fluidRow(
                              h1("sdev")
                            ),
                            fluidRow(
                              tableOutput('PCA_sdev')
                            )
                     ),
                     column(2, align="center",
                            fluidRow(
                              h1("center")
                            ),
                            fluidRow(
                              tableOutput('PCA_center')
                            )
                     ),
                     column(2, align="center",
                            fluidRow(
                              h1("scale")
                            ),
                            fluidRow(
                              tableOutput('PCA_scale')
                            )
                     ),
                     column(6, align="center",
                            fluidRow(
                              h1("rotation")
                            ),
                            fluidRow(
                              plotOutput("PCA_rotation")
                            )
                     )
                   )
      
          ),
          tabPanel("PCA data",
                   fluidRow(
                     column(12,
                       dataTableOutput('PCA_data')      
                     )
                   )
          ),
          tabPanel("input data(log)",
                   fluidRow(
                     column(12, align="center",
                        dataTableOutput('PCA_input_data')      
                     )
                   )
          )
        )
      ),
      tabPanel("CA",
        navlistPanel(widths = c(2,10),
          tabPanel("CA Plot",
                  column(12, align="center",
                    # fluidRow(
                    #    sliderInput("CA_slider",
                    #                "number of data",
                    #                10,
                    #                150,
                    #                150)
                    # ),
                    fluidRow(
                      plotOutput("CA_plot")
                    ),
                    fluidRow(
                      dataTableOutput('eig_table')
                    ),
                    fluidRow(
                      plotOutput("eig_plot")
                    )
                  )
          )
          # tabPanel("CA row result",
          #          column(12, align="center",
          #             fluidRow(
          #               plotOutput("row_inertia")
          #             ),
          #             fluidRow(
          #               plotOutput("row_cos")
          #             ),
          #          )
          # ),
          # tabPanel("PCA column result",
          # 
          # )
        )
      )
    )
  )
)

#Server
server = function(input, output, session) {
  # basic dataset tab
  output$iris_summary <- renderDataTable(summary(iris), options = list(
    dom='t', ordering=F)
  )
  
  output$iris_data <- renderDataTable(iris, options = list(
    pageLength=10)
  )
  
  # PCA tab
    # PCA Plot
  observeEvent(input$PCA_x, {
    updateRadioButtons(session, "PCA_y", inline = T,
                       choices = PCA_selection[PCA_selection != input$PCA_x]
    )
  })
  
  output$PCA_plot <- renderPlot({
    g <- ggbiplot(ir.pca, choices = c(as.integer(input$PCA_x), as.integer(input$PCA_y)), obs.scale = 1, var.scale = 1, groups = ir.species, ellipse = TRUE) +
                  scale_color_discrete(name = '') +
                  theme(legend.direction = 'horizontal', legend.position = 'bottom')
    print(g)
  })
  
    # PCA data
  output$PCA_data <- renderDataTable(ir.pca$x, options = list(
    pageLength=10, dom='ltip'))
  
    # input data(log)
  output$PCA_input_data <- renderDataTable(log.ir, options = list(
    pageLength=10, dom='ltip'))
  
    # extended result
  output$PCA_summary <- renderTable(summary(ir.pca)$importance, rownames=T)
  output$PCA_sdev <- renderTable(ir.pca$sdev)
  output$PCA_center <- renderTable(ir.pca$center)
  output$PCA_scale <- renderTable(ir.pca$scale)
  output$PCA_rotation <- renderPlot({
    g <- ggplot(rotation, aes(x = Var1, y = Var2, fill = value)) + 
      geom_tile() +
      geom_text(aes(label = round(value,2)), color = "white", size = 4)+
      coord_fixed() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.background = element_blank())
    print(g)
  })
  # CA tab
    # CA Plot
  output$CA_plot <- renderPlot({
    g <- plot.ca(ca.ir, col = c("black", "red"))
    print(g)
  })
  output$eig_table <- renderDataTable(
    get_eigenvalue(ca.ir), options = list(dom='t')
  )
  output$eig_plot <- renderPlot({
    a <- fviz_screeplot(ca.ir, addlabels = TRUE, ylim = c(0, 100))
    print(a)
  })
    # CA row
  row <- get_ca_row(ca.ir)
  output$row_inertia <- renderDataTable(row$inertia, options = list(dom='t'))
  output$row_cos <- renderDataTable(row$cos2*1000, options = list(dom='t'))
}


shinyApp(ui, server)