# ----| Inicio |-----

library(shiny)
load("Datos.RData")

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
                    fluidRow(
                      column(width = 6,
                             selectInput(inputId = "Categoria1",label = "Categoria:",
                                         choices = c("Alquiler","Alimentos","Higiene","Transporte",
                                                     "Celulares","Gustos","Ahorro","Hogar","Miscelaneos"),
                                         selected = "Alimentos")
                      ),
                      column(width = 6,
                             uiOutput(outputId = "Categoria2"))
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
                      ),
                      tabPanel(title = "Productos",
                               tableOutput(outputId = "ProductosT"))
                    )
                  )
                )
)

# ---| Server |----

server <- function(input, output, session) {
  
  output$Categoria2<- renderUI({
    aux<- switch (input$Categoria1,
                  "Alquiler" = c("Mensualidad","Deposito"),
                  "Alimentos" = "",
                  "Higiene" = c("Cuidado Personal","Lavanderia","Limpieza"),
                  "Transporte" = "",
                  "Celulares" = c("Claro","Movistar"),
                  "Gustos" = c("Comida","Entretenimiento"),
                  "Ahorro" = "",
                  "Hogar" = "",
                  "Miscelaneos" = c("Salud","Regalos","Papeleria","Documentacion","Inversion")
    )
    if(length(aux) == 1){
      return()
    }
    else {
      return(selectInput(inputId = "Categoria2",label = "Sub-Categoria",
                         choices = aux))
    }
  })
  
  Fecha<- reactive({as.Date(input$Fecha)})
  
  observeEvent(input$AgregarP,{
    # ----| Diario |---- 
    p<- which(Diario$Fecha == Fecha())
    if(length(p) == 1){
      NFilasDiario<- nrow(Diario)
      Diario$Gastado[p]<<- Diario$Gastado[p] + input$PrecioP
      Diario$Promedio[p:NFilasDiario]<<- cumsum(Diario$Gastado)[p:NFilasDiario]/p:NFilasDiario
      Diario$Proyeccion[p:NFilasDiario]<<- Diario$Promedio[p:NFilasDiario]*30
    }
    else {
      NFilasDiario<- nrow(Diario)
      DiferenciaFecha<- Fecha() - Diario$Fecha[NFilasDiario]
      if(DiferenciaFecha == 1){
        # No hay fechas sin registros
        Promedio<- (sum(Diario$Gastado)+input$PrecioP)/(NFilasDiario + 1)
        FilaDiario<- data.frame(Fecha=Fecha(),
                                Gastado=input$PrecioP,
                                Promedio=Promedio,
                                Proyeccion=Promedio*30)
        FilaDiario$Fecha<- as.Date(as.character(FilaDiario$Fecha))
        Diario<<- rbind(Diario,FilaDiario)
      }
      else {
        # Si hay fechas sin registros
        FechasSinRegistro<- (Diario$Fecha[NFilasDiario] + 1):(Fecha() - 1)
        NFechasSinRegistros<- length(FechasSinRegistro)
        GastoAcumulado<- Diario$Promedio[NFilasDiario]
        Promedio<- c(rep(GastoAcumulado,NFechasSinRegistros),GastoAcumulado + input$PrecioP) / (NFilasDiario + 1):(NFilasDiario + 1 + NFechasSinRegistros)
        FilasDiario<- data.frame(Fecha=c(FechasSinRegistro,Fecha()),
                                 Gastado=c(rep(0,NFechasSinRegistros),input$PrecioP),
                                 Promedio=Promedio,
                                 Proyeccion=Promedio*30)
        FilasDiario$Fecha<- as.Date(as.character(FilasDiario$Fecha))
        Diario<<- rbind(Diario,FilasDiario)
      }
    }
    # ----| Mensual |---- 
    p<- which(Mensual$Categoria == input$Categoria1)
    Mensual$Acumulado[p]<<- Mensual$Acumulado[p] + input$PrecioP
    Mensual$Diferencia[p]<<- Mensual$Limite[p] - Mensual$Acumulado[p] 
    
    # ----| Productos |----

    if(input$Categoria1 %in% c("Alquiler","Higiene","Celulares","Gustos","Miscelaneos")){
        FilaProducto<- data.frame(Fecha=Fecha(),
                                  Nombre=input$NombreP,
                                  Marca=input$MarcaP,
                                  Lugar=input$LugarP,
                                  Cantidad=input$CantidadP,
                                  Unidad=input$UnidadP,
                                  Precio=input$PrecioP,
                                  Categoria=input$Categoria1,
                                  SubCategoria=input$Categoria2)
    }
    else {
        FilaProducto<- data.frame(Fecha=Fecha(),
                                  Nombre=input$NombreP,
                                  Marca=input$MarcaP,
                                  Lugar=input$LugarP,
                                  Cantidad=input$CantidadP,
                                  Unidad=input$UnidadP,
                                  Precio=input$PrecioP,
                                  Categoria=input$Categoria1,
                                  SubCategoria="")
    }
    Productos$Fecha<- as.Date(as.character(Productos$Fecha))
    Productos<<- rbind(Productos,FilaProducto)
    
    # ----| Actualizado de tablas |----
    
    output$DiarioT<- renderTable({
      Diario
    })
    
    output$MensualT<- renderTable({
      Mensual
    })
    
    output$ProductosT<- renderTable({
      Productos
    })
    
    save(Productos,Diario,Mensual,file = "Datos.RData")
  })
  
  output$DiarioT<- renderTable({
      Diario
  })
  
  output$MensualT<- renderTable({
      Mensual
  })
  
  output$ProductosT<- renderTable({
      Productos
  })
  
}

shinyApp(ui, server)