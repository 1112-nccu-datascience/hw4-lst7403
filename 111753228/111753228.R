library(shiny)
library(ggplot2)
library(reshape2)
library(ggbiplot)
library(ca)
library("factoextra")

PCA_selection <- c("PC1" = 1, "PC2" = 2, "PC3" = 3, "PC4" = 4)

data(iris)
row.names(iris) <- 1:nrow(iris)
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]
ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)

rotation <- melt(ir.pca$rotation)


# ca.ir <- ca(iris[, 1:4])

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
                    fluidRow(
                       sliderInput("CA_slider1",
                                   "number of data",
                                   min = 6,
                                   max = 150,
                                   value = 150)
                    ),
                    fluidRow(
                      plotOutput("CA_plot")
                    ),
                    fluidRow(
                      tableOutput('eig_table')
                    ),
                    fluidRow(
                      plotOutput("eig_plot")
                    )
                  )
          ),
          tabPanel("CA row result",
                   column(12, align="center",
                      fluidRow(
                        sliderInput("CA_slider2",
                                    "number of data",
                                    min = 6,
                                    max = 150,
                                    value = 150)
                      ),
                      fluidRow(
                        tableOutput("CA_rows")
                      )
                   )
          ),
          tabPanel("CA column result",
                   column(12, align="center",
                          fluidRow(
                            sliderInput("CA_slider3",
                                        "number of data",
                                        min = 6,
                                        max = 150,
                                        value = 150)
                          ),
                          fluidRow(
                            tableOutput("CA_cols")
                          )
                   )
          )
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
    g <- plot.ca(ca(iris[1:input$CA_slider1,1:4]), col = c("black", "red"))
    print(g)
  })
  output$eig_table <- renderTable(
    get_eigenvalue(ca(iris[1:input$CA_slider1,1:4])), options = list(dom='t')
  )
  output$eig_plot <- renderPlot({
    a <- fviz_screeplot(ca(iris[1:input$CA_slider1,1:4]), addlabels = TRUE, ylim = c(0, 100))
    print(a)
  })
  
    # CA row result
  output$CA_rows <- renderTable({
    Mass <- ca(iris[1:input$CA_slider2, 1:4])$rowmass
    ChiDist <- ca(iris[1:input$CA_slider2, 1:4])$rowdist
    Inertia <- ca(iris[1:input$CA_slider2, 1:4])$rowinertia
    Dim.1 <- ca(iris[1:input$CA_slider2, 1:4])$rowcoord[,1]
    Dim.2 <- ca(iris[1:input$CA_slider2, 1:4])$rowcoord[,2]
    df <- data.frame(Mass, ChiDist, Inertia, Dim.1, Dim.2)
    print (df)
  }, rownames = TRUE
  )
  
    # CA column result
  output$CA_cols <- renderTable({
    Mass <- ca(iris[1:input$CA_slider3,1:4])$colmass
    ChiDist <- ca(iris[1:input$CA_slider3,1:4])$coldist
    Inertia <- ca(iris[1:input$CA_slider3,1:4])$colinertia
    Dim.1 <- ca(iris[1:input$CA_slider3,1:4])$colcoord[,1]
    Dim.2 <- ca(iris[1:input$CA_slider3,1:4])$colcoord[,2]
    df <- data.frame(Mass, ChiDist, Inertia, Dim.1, Dim.2)
    print (df)
  }, rownames = TRUE
  )
  
  observeEvent(input$CA_slider1, {
    updateSliderInput(
      session, "CA_slider2", value = input$CA_slider1
    )
    updateSliderInput(
      session, "CA_slider3", value = input$CA_slider1
    )
  })
  observeEvent(input$CA_slider2, {
    updateSliderInput(
      session, "CA_slider1", value = input$CA_slider2
    )
    updateSliderInput(
      session, "CA_slider3", value = input$CA_slider2
    )
  })
  observeEvent(input$CA_slider3, {
    updateSliderInput(
      session, "CA_slider1", value = input$CA_slider3
    )
    updateSliderInput(
      session, "CA_slider2", value = input$CA_slider3
    )
  })
  
}


shinyApp(ui, server)