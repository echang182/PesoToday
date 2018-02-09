# ----| Inicio |-----

library(shiny)
library(mailR)
load("Datos.RData")

# ----| UI |----

ui <- fluidPage(title = "Argentina Cost of Life",
                sidebarLayout(
                  # ----| SidePanel |----
                  sidebarPanel = sidebarPanel(
                    tabsetPanel(
                        # ----| Egresos |----
                        tabPanel(title = "Egresos",
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
                                                        choices = c("Gr","Kg","Lt","Ml","Unidades"))
                                     ),
                                     column(width = 5,
                                            numericInput(inputId = "PrecioP",label = "Precio:",min = 0,step = 0.01,
                                                         value = 0)
                                     )
                                 ),
                                 fluidRow(
                                     column(width = 6,
                                            selectInput(inputId = "Categoria1",label = "Categoria:",
                                                        choices = Categorias, selected = "Alimentos")
                                     ),
                                     column(width = 6,
                                            uiOutput(outputId = "Categoria2"))
                                 ),
                                 actionButton(inputId = "AgregarP",label = "Agregar")
                        ),
                        # ----| Ingresos |----
                        tabPanel(title = "Ingresos",
                            dateInput(inputId = "FechaIng",label = "Fecha:",value = Sys.Date()),
                            numericInput(inputId = "MontoIng",label = "Monto:",min = 0,
                                         value = 0),
                            selectInput(inputId = "CategoriaIng",label = "Categoria:",
                                        choices = c("Sueldo","Ventas","Donaciones","Otros"),
                                        selected = "Sueldo"),
                            uiOutput("SubCategoriaIng"),
                            actionButton(inputId = "AgregarIng",label = "Agregar")
                        ),
                        # ----| Opciones |----
                        tabPanel(title = "Opciones",
                            textInput(inputId = "correo",label = "Introducir correo:",
                                      placeholder = "example@correo.com"),
                            actionButton(inputId = "Enviar",label = "Enviar")
                        )
                    )
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
                               tableOutput(outputId = "ProductosT")
                      ),
                      tabPanel(title = "Balance",
                          tableOutput(outputId = "BalanceT")
                      )
                    )
                  )
                )
)

# ---| Server |----

server <- function(input, output, session) {
  
  # ----| Egresos |----  
  
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
        FechasSinRegistro<- as.Date((Diario$Fecha[NFilasDiario] + 1):(Fecha() - 1),
                                    origin = '1970-01-01')
        NFechasSinRegistros<- length(FechasSinRegistro)
        print(FechasSinRegistro)
        GastoAcumulado<- Diario$Promedio[NFilasDiario]
        Promedio<- c(rep(GastoAcumulado,NFechasSinRegistros),GastoAcumulado + input$PrecioP) / (NFilasDiario + 1):(NFilasDiario + 1 + NFechasSinRegistros)
        FilasDiario<- data.frame(Fecha=c(FechasSinRegistro,Fecha()),
                                 Gastado=c(rep(0,NFechasSinRegistros),input$PrecioP),
                                 Promedio=Promedio,
                                 Proyeccion=Promedio*30)
        print(FilasDiario)
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
    FilaProducto$Fecha<- as.Date(as.character(FilaProducto$Fecha))
    Productos<<- rbind(Productos,FilaProducto)
    
    # ----| Balance |----
    
    p<- which(Balance$Fecha == Fecha())
    if(length(p)==0){
        N<- nrow(Balance)
        DiferenciaFechas<- Balance$Fecha[N] - Fecha()
        if(DiferenciaFechas == 1){
            aux<- data.frame(
                Fecha= FechaIng(),
                Ingreso=0,
                Egreso=input$PrecioP,
                Saldo=Balance$Saldo[nrow(Balance)] - input$PrecioP
            )
            Balance<<- rbind(Balance,aux)
        }
        else {
            NFilasBalance<- nrow(Balance)
            FechasSinRegistro<- as.Date((Balance$Fecha[NFilasBalance] + 1):(Fecha() - 1),
                                        origin = '1970-01-01')
            NFechasSinRegistros<- length(FechasSinRegistro)
            print(FechasSinRegistro)
            FilasBalance<- data.frame(Fecha=c(FechasSinRegistro,Fecha()),
                                      Ingreso=rep(0,NFechasSinRegistros + 1),
                                      Egreso=c(rep(0,NFechasSinRegistros),input$PrecioP),
                                      Saldo=0)
            FilasBalance$Fecha<- as.Date(as.character(FilasBalance$Fecha))
            Balance<<- rbind(Balance,FilasBalance)
            for(i in (NFilasBalance + 1):nrow(Balance)){
                Balance$Saldo[i]<<- Balance$Saldo[i-1] - Balance$Egreso[i]
            }
            print(FilasDiario)
        }
    }
    else {
        Balance$Egreso[p]<<- Balance$Egreso[p] + input$PrecioP
        for(i in p:nrow(Balance)){
            Balance$Saldo[i]<<- Balance$Saldo[i-1] + Balance$Ingreso[i] - Balance$Egreso[i]
        }
    }
    # ----| Actualizado de tablas |----
    
    output$DiarioT<- renderTable({
        Diario$Fecha<- as.character(Diario$Fecha)
        Diario
    })
    
    output$MensualT<- renderTable({
        Mensual
    })
    
    output$ProductosT<- renderTable({
        Productos$Fecha<- as.character(Productos$Fecha)
        Productos
    })
    
    output$BalanceT<- renderTable({
        Balance$Fecha<- as.character(Balance$Fecha)
        Balance
    })
    
    save.image(file = "Datos.RData")
  })
  
  output$DiarioT<- renderTable({
      Diario$Fecha<- as.character(Diario$Fecha)
      Diario
  })
  
  output$MensualT<- renderTable({
      Mensual
  })
  
  output$ProductosT<- renderTable({
      Productos$Fecha<- as.character(Productos$Fecha)
      Productos
  })
  
  output$BalanceT<- renderTable({
      Balance$Fecha<- as.character(Balance$Fecha)
      Balance
  })
  
  # ----| Ingresos |-----
  
  output$SubCategoriaIng<- renderUI({
      if(input$CategoriaIng == "Sueldo"){
          return(selectInput(inputId = "SubCategoriaIng",label = "Sub-Categoria:",
                             choices = c("Sueldo Eloy","Sueldo Yuli")))
      }
      else{
          return()
      }
  })
  
  FechaIng<- reactive({as.Date(input$FechaIng)})
  
  observeEvent(input$AgregarIng,{
      # ----| Ingresos |----
      
      if(input$CategoriaIng == "Sueldos"){
          aux<- data.frame(
              Fecha=FechaIng(),
              Categoria=input$CategoriaIng,
              SubCategoria=input$SubCategoriaIng,
              Monto=input$MontoIng
          )
      }
      else {
          aux<- data.frame(
              Fecha=FechaIng(),
              Categoria=input$CategoriaIng,
              SubCategoria="",
              Monto=input$MontoIng
          )
      }
      Ingreso<<- rbind(Ingreso,aux)
      
      # ----| Balance |----
      
      p<- which(Balance$Fecha == FechaIng())
      if(length(p)==0){
          N<- nrow(Balance)
          DiferenciaFechas<- Balance$Fecha[N] - FechaIng()
          if(DiferenciaFechas == 1){
              aux<- data.frame(
                  Fecha= FechaIng(),
                  Ingreso=input$MontoIng,
                  Egreso=0,
                  Saldo=Balance$Saldo[nrow(Balance)] + input$MontoIng
              )
              Balance<<- rbind(Balance,aux)
          }
          else {
              NFilasBalance<- nrow(Balance)
              FechasSinRegistro<- as.Date((Balance$Fecha[NFilasBalance] + 1):(FechaIng() - 1),
                                          origin = '1970-01-01')
              NFechasSinRegistros<- length(FechasSinRegistro)
              print(FechasSinRegistro)
              FilasBalance<- data.frame(Fecha=c(FechasSinRegistro,FechaIng()),
                                       Ingreso=c(rep(0,NFechasSinRegistros),input$MontoIng),
                                       Egreso=rep(0,NFechasSinRegistros + 1),
                                       Saldo=0)
              FilasBalance$Fecha<- as.Date(as.character(FilasBalance$Fecha))
              Balance<<- rbind(Balance,FilasBalance)
              for(i in (NFilasBalance + 1):nrow(Balance)){
                  Balance$Saldo[i]<<- Balance$Saldo[i-1] + Balance$Ingreso[i]
              }
          }
      }
      else {
          Balance$Ingreso[p]<<- Balance$Ingreso[p] + input$MontoIng
          for(i in p:nrow(Balance)){
              Balance$Saldo[i]<<- Balance$Saldo[i-1] + Balance$Ingreso[i] - Balance$Egreso[i]
          }
      }
      
      # ----| Actualizar las tablas |----
      
      output$DiarioT<- renderTable({
          Diario$Fecha<- as.character(Diario$Fecha)
          Diario
      })
      
      output$MensualT<- renderTable({
          Mensual
      })
      
      output$ProductosT<- renderTable({
          Productos$Fecha<- as.character(Productos$Fecha)
          Productos
      })
      
      output$BalanceT<- renderTable({
          Balance$Fecha<- as.character(Balance$Fecha)
          Balance
      })
      
      save.image(file = "Datos.RData")
  })
  
  # ----| Opciones |-----
  
  observeEvent(input$Enviar,{
      send.mail(from = "epsilon.datalabs@gmail.com",to = input$correo,
                subject = "RespaldoDatos",
                body = paste("Respado generado el:",Sys.time()),authenticate = TRUE,
                smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "epsilon.datalabs", passwd = "$c6219_s51", ssl = TRUE),
                attach.files = "Datos.RData",
                send = TRUE)
  })
}

shinyApp(ui, server)