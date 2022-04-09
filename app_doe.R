library(tidyverse)
library(openxlsx)
library(shiny)
library(DT)

a <- b <- c <- d <- e <- c(-1, 1)

# Define UI for application 
ui <- fluidPage(
  
  # Application title
  titlePanel("Generación de datos para experimento factorial (caso individual)"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      "Cambie los siguientes valores", 
      sliderInput("replicas",
                  "Cantidad de réplicas:",
                  min = 2,
                  max = 4,
                  value = 2, 
                  step = 1),
      
      downloadButton("descargar", "Descargar datos en .xlsx"),
      
    ),
    
    
    # Show a plot
    mainPanel(
      
      DT::DTOutput("table")
      
    )
  )
)


server <- function(input, output) {
  
  datos <- reactive({
    
    N <- 32 * input$replicas
    
    mat <- tidyr::expand_grid(a,b,c,d, e) %>% 
      dplyr::mutate(ab = a*b, 
                    ac = a*c,
                    ad = a*d,
                    ae = a*e,
                    bc = b*c,
                    bd = b*d,
                    be = b*e,
                    cd = c*e,
                    de = d*e,
                    abc = a*b*c,
                    abd = a*b*d,
                    abe = a*b*e,
                    acd = a*c*d,
                    ade = a*d*e,
                    bcd = b*c*d,
                    bde = b*c*e,
                    abcd = a*b*c*d,
                    abde = a*b*d*e)
    
    mat <- mat[rep(seq_len(nrow(mat)), input$replicas), ]
    
    color <- (17.105 - 1.713 * mat$a + 2.179 * mat$b + 0.127 * mat$c + 0.390*mat$d + 
      0.357 * mat$e + 0.147 * mat$ab + 0.034 * mat$ac + 0.659 * mat$ad + 
      0.111 * mat$ae - 0.323 * mat$bc + 0.313 * mat$bd + 0.176 * mat$be + 
      0.239 * mat$cd + 0.433 * mat$de + 0.183 * mat$abc - 0.133 * mat$abd + 
      0.194 * mat$abe + 0.340 * mat$acd + 0.593 * mat$ade - 0.465 * mat$bcd + 
      0.278 * mat$bde - 0.336 * mat$abcd + 0.616 * mat$abde + 
        rnorm(N, 0, 1.5)) %>% 
      round(3)
    
    mat <- mat %>% 
      dplyr::select(a:e) %>% 
      cbind(color)
    
    names(mat) <- c("A", "B", "C", "D", "E", "Respuesta")
    
    mat[sample(nrow(mat)), ]
    
  })
  
  # mostrar tabla
  
  output$table <- DT::renderDT(datos(), 
                               options = list(scrollX = TRUE,
                                              pageLength = 50),
                               rownames = FALSE)
  
  
  output$descargar <- downloadHandler(
    
    
    filename = function(){
      
      paste("datos", ".xlsx", sep = "")
      
    },
    
    content = function(file){
      openxlsx::write.xlsx(datos(), file)
    }
    
  )
    
  }


# Run the application 
shinyApp(ui = ui, server = server)