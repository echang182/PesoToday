# ----| Inicio |-----

library(shiny)

Lugares<- sort(c("Dia","Carrefour","COTO","Carni-Frute/ria La Lucila",
                 "Mercado de San Telmo","Chinos","Barrio Chino"))

# ----| UI |----

ui <- fluidPage(title = "Argentina Cost of Life",
    sidebarLayout(
        # ----| SidePanel |----
        sidebarPanel = sidebarPanel(
            dateInput(inputId = "Fecha",label = "Introducir fecha:",value = Sys.Date()),
            textInput(inputId = "NombreP",label = "Nombre del producto"),
            fluidRow(
                column(width = 6,
                       textInput(inputId = "MarcaP",label = "Marca:")
                ),
                column(width = 6,
                       selectInput(inputId = "LugarP",label = "Lugar:",
                                   choices = Lugares,selected = "Dia")       
                )
            ),
            fluidRow(
                column(width = 3,
                    numericInput(inputId = "CantidadP",label = "Cantidad:",min = 0,value = 0)
                ),
                column(width = 4,
                    selectInput(inputId = "UnidadP",label = "Unidad:",
                                choices = c("Gr","Kg","Lt","Unidades"))
                ),
                column(width = 5,
                    numericInput(inputId = "PrecioP",label = "Precio:",min = 0,step = 0.01,
                                 value = 0)
                )
            ),
            actionButton(inputId = "AgregarP",label = "Agregar")
        ),
        # ----| MainPanel |----
        mainPanel = mainPanel(
            tabsetPanel(
                tabPanel(title = "Diario",
                    tableOutput(outputId = "DiarioT")
                ),
                tabPanel(title = "Mensual",
                    tableOutput(outputId = "MensualT")
                )
            )
        )
    )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)