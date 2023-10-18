################ ------- TFM DANIELA LAMA - DoE ------- ################
rm(list=ls())
######## -------- INSTALACION DE PAQUETES -------- ########

##### variable con lista de paquetes deseados 
paqs<-c("tidyverse","shiny","DT","rhandsontable")

##### variable de paquetes a instalar: son los deseados que NO est?n en los resultados de la columna 1 de la libreria. Si no especificaramos [,1], aparecer?a m?s informaci?n de los paquetes con library()$results. 
paqs2Install<-paqs[!(paqs%in%library()$results[,1])]

##### variable de paquetes a cargar:son los deseados que NO est?n cargados en la libería de R. Especificar all.available=TRUE para ver los paquetes cargados. 
paqs2Load<-paqs[!(paqs%in%.packages())]

### --- PREGUNTA: los cargados son los m ismos instalados... por tanto... no es redundante??? 

##### para aquellos paquetes en paquetes a instalar, instalar desde el repositorio. tydiverse no est? disponible como type="binary".
for(pckg in paqs2Install) {install.packages(pckg,repos="https://cloud.r-project.org/", quiet=TRUE,type="source")}

### --- PREGUNTA: verificar type="binary" o type="source" en help, y quiet=TRUE. 

##### para aquellos paquetes en paquetes a cargar, 
for(pckg in paqs2Load) {library(pckg,character.only=TRUE)}


######## -------- COEFICIENTES PAPER UAE (2019) -------- #########

b0 <-(56.4)
b1 <-(1.334)
b2 <-(-0.718)
b3 <-(-0.336)
b4 <-(-42.1)
b5 <-(-10.44)
b6 <-(-0.07)
b11 <-(-0.0129)
b12 <-(0.0108)
b13 <-(0.00202)
b14 <-(-0.32)
b15 <-(0.0701)
b16 <-(-0.0064)
b22 <-(-0.00032)
b23 <-(-0.00158)
b24<-(0.102)
b25<-(0.046)
b26<-(-0.0189)
b33 <-(0.00447)
b34<-(-0.851)
b35<-(-0.0229)
b36<-(0.0187)
b44<-(30)
b45<-(-4.23)
b46<-(5.36)
b55<-(0.473)
b56<-(0.135)
b66<-(-0.0878)

######## -------- AMBIENTE GLOBAL --------- ########

#####  Variable que contiene todas las letras del abcedario
abcedario = LETTERS [1:26]

#####  Fnx para cargar otro archivo R: en este caso las matrices de Taguchi
source("C:\Users\dlama\OneDrive\Escritorio\Shiny ultims")

#####  Fnx para crear checkbox: vector vacio o lleno (numero): cuando recibe un "", devuelve una columna nueva vacia para conservar la grilla de la matriz triangular de interacciones
crear_check = function(paso,inputId,value,label){
  if (paso == "") {
    resultado = column(1," ")
  }
  else {
    contador_nombres <<- c(contador_nombres,inputId)
    resultado = checkboxInput(inputId = inputId,value = value,label = label)
  }
  return(resultado)
}
##### ---------



#####  Fnx para crear filas de la matriz triangular de interacciones

crear_fila = function(nombre,combinaciones,inicio,inicio_fill,fin,input_text,label='texto'){
  
  ### ---- Vector que se va a llenar dependiendo de las columnas y filas ---- ###
  Vector_llenado = c(rep("",(inicio_fill)),seq(inicio_fill,fin))[-length(c(rep("",(inicio_fill)),seq(inicio_fill,fin)))] 
  
  resultado = fluidPage(column(1,nombre),fluidPage(do.call(splitLayout, lapply(Vector_llenado,function(y){p(crear_check(paso=y,inputId = paste0(input_text,'_',abcedario[as.numeric(y)+2]),value = 0,label = label))}))))
  return(resultado)
}
##### ---------



#####  Fnx para poner nombres de columnas a la matriz triangular de interacciones

crear_factores = function(factores,input_text,label){
  
  ### ----  Vector (vacio en inicio) que almacena todos los nombres de las columnas ---- ###
  contador_nombres <<- c()
  
  ### ---- Tamaño de columnas y filas de la matriz triangular ---- ###
  columnas = factores
  filas = factores-1
  
  ### ---- Nombre de las columnas ---- ###
  nombre_col = fluidPage(column(1,""),fluidPage(do.call(splitLayout, lapply(c(abcedario[seq(2,filas+1)]),function(x){tagList(p(x))}))))
  
  ### ---- Crear cada columna ---- ###
  resultado = do.call(tagList, lapply(c(seq(1,columnas)),function(x){tagList(crear_fila(nombre=abcedario[x],combinaciones=combinaciones,inicio=0,inicio_fill=(x-1),fin=filas,input_text=paste0(input_text,'_',abcedario[x]),label = label))}))
  
  ### ---- Consolida el nombre de columnas con los checkboxes ---- ###
  resultado = tagList(nombre_col,resultado)
  return(resultado)
}
##### ---------


######## -------- USER INTERFASE SHINY --------- ########

app_ui<-fluidPage(
  titlePanel("Bienvenido a AppDOE"),
  sidebarLayout(
    
    #####  Panel izquierdo
    sidebarPanel(
      width=3,
      titlePanel(h3("Seleccione el caso de preferencia")),
      verticalLayout(
        fileInput(inputId="cargar_archivo",label="Cargar caso"),
        radioButtons(inputId="caso_x",label="Empezar un caso nuevo",choices=c("Caso libre","Caso propuesto","Caso simulado")),
        wellPanel(style= "background: lightblue",
                  textOutput("descrip_caso")
        ),
        wellPanel(
          tags$head(
            tags$style(HTML('#intdatos{background-color:lightblue}'))
          ),
          div(style="display:inline-block;width:32%;text-align: left;",
              actionButton(inputId="ayuda",label="Ayuda")),
          div(style="display:inline-block;width:32%;text-align: center;",
              actionButton(inputId="autor",label="Autor")),
          div(style="display:inline-block;width:32%;text-align: right;",
              actionButton(inputId="intdatos",label="Empezar"))
        )
      )
    ),
    ##### ---------
    
    
    #####  Panel derecho
    mainPanel(
      width=9,
      #h es el tamaño de fuente
      tabsetPanel(id="tabs1",type="tabs",
                  tabPanel(title="Guía",wellPanel(h4(""))),
                  tabPanel(title="Dominio",
                           wellPanel(h5(
                             "En esta sección tendrá que introducir el número de factores y el número de niveles de interés. Una vez introducidos estos datos, tendrá que pulsar sobre el botón 'Generar diseño' para que el programa sugiera la matriz de Taguchi más adecuada para los grados de libertad requeridos. Si está de acuerdo con la matriz sugerida, continúe pulsando el botón 'continuar'")),
                           
                           splitLayout(
                             numericInput(inputId="num_factores",label="Número de factores",value=2,min=2,step=1,max=26),
                             tags$style(HTML('#irinterac{background-color:lightblue}')),
                             actionButton(inputId="irinterac",label="Continuar")),
                           
                           fluidRow(
                             column(5,div(style = "height:400px;background-color: lightgrey;", DT::dataTableOutput("tabla_factores",width=250))),
                             column(7,div(style = "height:400px;background-color: lightblue;", DT::dataTableOutput("tabla_niveles",width=250)))),
                           
                           actionButton(inputId="borrar_datos",label="Reset"),
                           
                           actionButton(inputId="actualizar",label="Actualizar tabla de factores"),
                           
                           fluidRow(
                             actionButton(inputId="calcmatriz",label="Generar diseño"),
                             textOutput("diseño_sugerido"))),
                  
                  tabPanel(title="Interacciones",
                           # numericInput(inputId="num_factores_2",label="Numero de factores",value=2,min=2,max=26,step=1),
                           uiOutput("factores_generales"),
                           tags$style(HTML('#irmatriz{background-color:lightblue}')),
                           div(style="display:inline-block;width:32%;text-align: right;"),
                           actionButton(inputId="button",label="Mostrar Interacciones"),
                           DT::DTOutput("tabla_resultados_optimo"),
                           actionButton(inputId="irmatriz",label="Continuar")),
                  
                  tabPanel(title="Matriz",
                           tags$style(HTML('#iranalisis{background-color:lightblue}')),
                           div(style="display:inline-block;width:32%;text-align: right;"),
                           actionButton(inputId="iranalisis",label="Continuar")),
                  
                  tabPanel(title="Análisis")
      )
      ##### ---------
    )))



######## -------- SERVER SHINY --------- ########

app_server<-function(input,output,session){
  
  ##### Fnx shinyInput
  shinyInput <- function(FUN,id,num,...) {
    inputs <- character(num)
    for (i in seq_len(num)) {
      inputs[i] <- as.character(FUN(paste0(id,i),label=NULL,...))
    }
    return(inputs)
  }
  ##### ---------
  
  
  ##### Descripcion texto de los casos al seleccionar alguno en el panel izquierdo
  observeEvent(input$caso_x,{
    output$descrip_caso<-renderText({
      if(input$caso_x=="Caso libre")
        paste("Descripción caso libre")
      else
        if(input$caso_x=="Caso propuesto")
          paste("Descripcion caso paper")
      else
        if(input$caso_x=="Caso simulado")
          paste("Descripcion caso simulacion")
      else
        return()
    })}) 
  ##### ---------
  
  
  
  ##### Cuadros de dialogo (ayuda, autor) del panel izquierdo
  
  observeEvent(input$ayuda,{
    showModal(modalDialog(
      # title="AYUDA",renderText("Aquí no sé cómo hacer espacios entre líneas")
      title="AYUDA",HTML("This is the first line.<br> 
      This should be the second.")
    ))
  })
  
  observeEvent(input$autor,{
    showModal(modalDialog(
      title="Información del autor",renderText("Misma historia que en ayuda")
    ))
  })
  ##### ---------
  
  
  
  ##### Ir a la siguiente pestaña con botones "Continuar"
  observeEvent(input$intdatos,{
    updateTabsetPanel(session,"tabs1",selected="Dominio")
  })
  
  observeEvent(input$irinterac,{
    updateTabsetPanel(session,"tabs1",selected="Interacciones")
  })
  
  observeEvent(input$irmatriz,{
    updateTabsetPanel(session,"tabs1",selected="Matriz")
  })
  
  observeEvent(input$iranalisis,{
    updateTabsetPanel(session,"tabs1",selected="Análisis")
  })
  ##### ---------
  
  
  ##### Tabla de factores en pestaña "Dominio"
  tabla_dom=reactive({
    nombre_columna_fac<-c("Nombre Factor","Nivel")
    
    for (i in input$num_factores){
        d1=data.frame(matrix(nrow=input$num_factores,ncol=2,dimnames=list(LETTERS[1:i])))
    }

    d1<<-as.data.frame(d1) #OJO: no poner d1=as.data.frame(d1)
    
    colnames(d1)<-nombre_columna_fac
    return(d1)
  })
  
  output$tabla_factores<-DT::renderDataTable(tabla_dom(),editable=list(target = 'cell', disable = list(columns = 0)))
  ##### ---------
  
  
  
  ##### Tabla de niveles reactiva a la tabla de factores en pestaña "Dominio"
  observeEvent(input$num_factores,{
    
    d3 <<- data.frame(matrix(nrow=input$num_factores,ncol=1))
    colnames(d3)<<-"texto"
    n <- 0
    d1=data.frame(matrix(nrow=input$num_factores,ncol=n))
    output$tabla_niveles<-renderDataTable(d1,editable=list(target = 'cell', disable = list(columns = 0)))
  })
  
  n <- 0
  
  observeEvent(input$tabla_factores_cell_edit,{
    
    if(as.integer(input$tabla_factores_cell_edit[2]$col)==2){
      if(n<as.integer(input$tabla_factores_cell_edit[3]$value)){
        n <<- as.integer(input$tabla_factores_cell_edit[3]$value)
      }
    }
    d1=data.frame(matrix(nrow=input$num_factores,ncol=n))
    if(as.integer(input$tabla_factores_cell_edit[2]$col)==1){
      
      d3$texto[as.integer(input$tabla_factores_cell_edit[1]$row)] <<- input$tabla_factores_cell_edit[3]$value
      # text_row <<- append(text_row,input$tabla_factores_cell_edit[3]$value)
    } 
    
    print(d3)
    
    d1=data.frame(matrix(nrow=input$num_factores,ncol=n))
    for (i in 1:nrow(d3)){
      if(!is.na(d3$texto[i])){
        row.names(d1)[i]<- d3$texto[i]
      }
    }
    # row.names(d1)<-d3$texto
    
    output$tabla_niveles<-renderDataTable(d1,editable=list(target = 'cell', disable = list(columns = 0)))
  })
  ##### ---------
  
  
  
  ##### Tabla de interacciones seleccionadas en pestaña "Matriz"
  observeEvent(input$button, {
    
    ### ----- Tabla de interacciones seleccionadas
    Lista_operacion = unlist(lapply(contador_nombres, function(x){input[[x]]}))
    Seleccionadas = data.frame('Factor'=contador_nombres,'Valor'=Lista_operacion)
    
    ### ----- Filtrar solo los que se seleccionaron ------ ######
    Filtradas = Seleccionadas[Seleccionadas['Valor'] == TRUE,]
    Factores_seleccionados = sum(Lista_operacion) 
    
    ### ----- Arreglar los nombres ---- ####
    Filtradas$Factor= gsub(x = gsub(x = Filtradas[['Factor']],replacement = '',pattern = 'columna_'),replacement = ' con ',pattern = '_')
    
    output$tabla_resultados_optimo = renderDT(
      datatable(Filtradas,
                selection = list(target = "cell"),extensions = 'Buttons',options = list(scrollX = TRUE,scrollY = TRUE, pageLength = 15,autoWidth = TRUE,searching = TRUE, paging = TRUE,ordering = FALSE,dom = 'Bfrtip',buttons = list(
                  list(extend = 'copy',text = "Copiar",title = "Nombre"),
                  list(extend = 'excel',text = "Excel",sheetName = "Agrupacion",
                       messageBottom = paste0("Fecha: ", Sys.Date()),
                       title = "Agrupacion"),
                  list(extend = 'pdf',text = "PDF",title = 'Agrupacion'))))
    )})
  ##### ---------
  
  
  ##### Reactividad de la tabla de interacciones en pestaña "Matriz" 
  # dataInput <- reactive({
  #   crear_factores(factores = input$num_factores_2,input_text = 'columna',label = '')
  # })
  # output$factores_generales <- renderUI(dataInput())
  
  observeEvent(input$num_factores,{
    
    dataInput <- reactive({
        crear_factores(factores = input$num_factores,input_text = 'columna',label = '')
      })
      output$factores_generales <- renderUI(dataInput())
  })
  
  ##### ---------
  
  
  
  ##### Boton para resetear (inicializar) datos introducidos
  observeEvent(input$borrar_datos,{
    updateNumericInput(session,"num_factores",value=2)
    # updateNumericInput(session,"num_interac",value=0)
  })
  ##### ---------
  
  
  ##### Boton para sugerir diseño experimental (falta por configurar calculo de g.l dentro del boton)
  observeEvent(input$calcmatriz,{
    output$diseño_sugerido<-renderText("Diseño sugerido, debe depender de alguna variable de calc")
  })
  ##### ---------
} 



######## -------- RUN APP SHINY --------- ########
shinyApp(ui=app_ui,server=app_server)