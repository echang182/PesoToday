Mensual<- data.frame(
    Categoria=c("Alquiler","Alimentos","Higiene","Transporte",
                "Celulares","Gustos","Ahorro","Hogar","Miscelaneos"),
    Acumulado=rep(0,9),
    Limite=c(8000,4000,2400,900,550,1650,12000,2500,0),
    Diferencia=c(8000,4000,2400,900,550,1650,12000,2500,0)
)

Ingreso<- data.frame(
    Fecha=character(),
    Categoria=character(),
    SubCategoria=character(),
    Monto=numeric()
)

Ingreso$Fecha<- as.Date(as.character(Ingreso$Fecha))
Ingreso$Categoria<- as.character(Ingreso$Categoria)
Ingreso$SubCategoria<- as.character(Ingreso$SubCategoria)

Balance<- data.frame(
    Fecha=Sys.Date()-4,
    Ingreso=0,
    Egreso=0,
    Saldo=0
)


