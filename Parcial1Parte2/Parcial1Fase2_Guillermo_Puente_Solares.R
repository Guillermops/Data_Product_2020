library(shiny)
library(ggplot2)
library(dplyr)
library(ggiraph)
library(readr)
library(lubridate)
library(rsconnect)

setwd("C:/Users/GUILLERMO/Desktop/U/Octavo Semestre/Data Product/SHINY/Parcial1Parte2/Parcial1Parte2")
data <- read_csv("data.csv")
data <- data %>% 
  filter(NA_Sales >= 1,
         EU_Sales >= 1,
         JP_Sales >= 1,
         NA_Sales <= 40)



ui <- shinyUI(fluidPage(
  
  
  titlePanel("Parcial 1 Parte 2"),
  
  tabsetPanel(tabPanel("Graficas",
                       girafeOutput("clickv"),
                       girafeOutput("a"),
                       girafeOutput("b")
                       
  ),
  
  tabPanel("Tabla",
           fluidRow(column(12,DT::dataTableOutput('t1'),
                           verbatimTextOutput("output")))),
  
  tabPanel("Input",
           sidebarLayout(
             sidebarPanel(
               selectInput("plataforma", "Rango de antiguedad de la plataforma: ", choices = c("ANT" = "Antiguo", "MOD" = "Moderno", "PMOD" = "Posmoderno"),
                           multiple = TRUE, selected = c("Posmoderno")),
               selectInput("tipo", "Genero del juego", choices = c("Action" = "Accion", "Sports" = "Deportes", "Misc" = "Miscelaneo", "Strategy" = "Estrategia"),
                           multiple = TRUE, selected = c("Accion")),
               sliderInput("nasales", "Ventas en Norte America", min = 1, max = 30, value = c(0,30), step = 1),
               selectInput("anho", "Seleccione el Anho", choices = c("1985" = 1985, "2008" = 2008, "2009" = 2009, "1996" = 1996, "1989" = 1989, "2006" = 2006, "2005" = 2005,
                                                                     "1999" = 1999, "2007" = 2007, "1990" = 1990, "1988" = 1988, "2002" = 2002, "2010" = 2010, "1998" = 1998,
                                                                     "2013" = 2013, "2011" = 2011, "2014" = 2014, "1992" = 1992, "1993" = 1993, "2004" = 2004, "2012" = 2012,
                                                                     "1994" = 1994, "2001" = 2001, "2000" = 2000, "2015" = 2015, "1997" = 1997, "2003" = 2003), multiple = TRUE, selected = c("2015")),
               actionButton('reset2','Reset')),
             mainPanel(plotOutput("GraficaVentas")))),
  
  
  tabPanel("Subir Archivos",
           sidebarPanel(
             fileInput("subir_archivo","Seleccionar archivo original o cualquier otro")),
           mainPanel(
             DT::dataTableOutput("Contenido")),
  ),
  
  tabPanel("Grafica por ventas en Norte America",
           sidebarPanel(
             selectInput(inputId = "Color", label = "Seleccione el Color",
                         choices = c("Red" = "Red", "Blue" = "Blue"),
                         selected = "Blue", multiple = FALSE),
             radioButtons(inputId = "orilla1", label = "Seleccione el color",
                          choices = c("Black"="#000000","White"="#ffffff")),
             selectInput(inputId = "areaventa", label = "Seleccione el area de Ventas",
                         choices = c("Genero de Juego" = "Genero de Juego",
                                     "Rango de ventas por desarrollador" = "Rango de ventas por desarrollador",
                                     "Plataforma" = "Platforma"),
                         selected = "Genero de Juego", multiple = FALSE),
             sliderInput(inputId = "rango", label = "Rango de Ventas NA",
                         min = 1, max = 30, value = c(1, 30)),
             actionButton('reset3','Reset')
           ),
           
           mainPanel(
             
             plotOutput(outputId = "grafNA")
             
           )))))


server <- shinyServer(function(input, output, session) {
  
  output$clickv <- renderggiraph({
    
    p <- ggplot(data, aes(x=NA_Sales, y=EU_Sales, color=Platform))
    q <- p + geom_point_interactive(aes(tooltip = Genre, data_id = Genre), size =5)
    
    girafe(ggobj = q,
           options = list(
             opts_selection(type = "multiple",
                            only_shiny = FALSE,
                            css = "stroke:green;fill:green"),
             opts_hover(css = "fill:gray;stroke:gray")))
    
  })
  
  output$a <- renderggiraph({
    
    r <- ggplot(data, aes(x=JP_Sales, y=EU_Sales, color=Platform))
    s <- r + geom_point_interactive(aes(tooltip = Genre, data_id = Genre), size =5)
    
    girafe(ggobj = s,
           options = list(
             opts_selection(type = "multiple",
                            only_shiny = FALSE,
                            css = "stroke:green;fill:green"),
             opts_hover(css = "fill:gray;stroke:gray")))
  })
  
  output$b <- renderggiraph({
    
    v <- ggplot(data, aes(x=NA_Sales, y=JP_Sales, color=Platform))
    w <- v + geom_point_interactive(aes(tooltip = Genre, data_id = Genre), size =5)
    
    girafe(ggobj = w,
           options = list(
             opts_selection(type = "multiple",
                            only_shiny = FALSE,
                            css = "stroke:green;fill:green"),
             opts_hover(css = "fill:gray;stroke:gray")))
  })
  
  output$t1 <- DT::renderDataTable({
    data %>% DT::datatable(rownames = TRUE,
                           filter = "top",
                           extensions = "Buttons",
                           options = list(pageLength = 15,
                                          lengthMenu = c(5, 10, 15),
                                          dom = "Bfrtip",
                                          Buttons = c("csv")
                           )
                           
    )
  })
  
  
  subir_archivo <- reactive({
    contenido <- input$subir_archivo
    if( is.null(contenido) ){
      return(NULL)
    }
    Documento <- read_csv(contenido$datapath)
    return(Documento)
  })
  
  output$Contenido <- DT::renderDataTable({
    DT::datatable(subir_archivo(),
                  rownames = TRUE,
                  filter = "top",
                  extensions = "Buttons",
                  options = list(pageLength = 15,
                                 lengthMenu = c(5, 10, 15),
                                 dom = "Bfrtip",
                                 Buttons = c("csv")
                  )
                  
    )
    
    
  })
  
  observeEvent(input$reset2, {
    updateSliderInput(session,'nasales', value = c(0,30))
    updateSelectInput(session,'plataforma', choices = "Posmoderno")
    updateSelectInput(session,'tipo', choices = c("Accion"))
    updateSelectInput(session, "anho", choices = c("2015"))
  })
  
  output$GraficaVentas <- renderPlot({
    ventas <- data %>% 
      group_by(data$Year, data$Platform) %>% 
      summarise(Cantidad = n(), .groups = "drop") %>% 
      ggplot(venta, aes(venta$`data$Platform`, Cantidad, fill = venta$`data$Platform`)) +
      labs(title = "Juegos Vendidos en Norte America por genero de Juego")
  })
  
  observeEvent(input$reset3, {
    updateSliderInput(session,'rango', value = c(0,30))
    updateSelectInput(session,'color', choices = "Blue")
    updateSelectInput(session,'areaventa', choices = c("Genre"))
  })
  
  output$grafNA <- renderPlot({
    
    if(input$Color == "Red"){
      sColor = "#ff3300"
    }else if(input$Color == "Blue"){
      sColor = "#3399ff"
    }else if(input$Color == "Green"){
      sColor = "#66ff33"
    }
    
    p2 <- data %>%
      filter(NA_Sales >= input$rango[1] & NA_Sales <= input$rango[2]) %>% 
      ggplot()
    if(input$areaventa == "Genero de Juego"){
      p2 <- p2 + geom_bar(aes(x = Genre),bins = input$bin,col=input$orilla1,fill=sColor)
    }else if(input$areaventa == "Plataforma"){
      p2 <- p2 + geom_bar(aes(x = Platform),bins = input$bin,col=input$orilla1,fill=sColor)
    }else if(input$areaventa == "Rango de ventas por desarrollador"){
      p2 <- p2 + geom_bar(aes(x = Publisher_Range),bins = input$bin,col=input$orilla1,fill=sColor)
    }
    p2 <- p2 +  theme_bw()+
      theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
            axis.text = element_text(size=14,color="BLACK",face="bold"))+
      labs(x="Genero/Plataforma/Desarrollador",y="Ventas por Region",title=paste("Ventas anuales",input$areaventa,sep = " "))
    
    p2
  })
  
})

shinyApp(ui = ui, server = server)



