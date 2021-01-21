library(shiny)

ui <- fluidPage(
  
  fluidRow(
    column(2, offset = 4, 
           HTML("<a href = '#'>Descargar Reporte</a>")
           ), 
    column(2, 
           HTML("<a href = '#'>Consultar Código</a>")
    )
    
  ),
  
  tags$head(tags$script(src = "message-handler.js"))
  
)

server <- function(input, output, session) {
  observeEvent(input$do, {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Thank you for clicking')
  })
}

shinyApp(ui, server)