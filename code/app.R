# Paquets necessaris
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(xtable)
library(tidyverse)

# Es carreguen les dades des d'altres scripts
source("plotly.R")
source("visNetwork.R")


ui <- tagList(
  navbarPage("Anàlisi de dades mediambientals mitjançant teoria de xarxes",
             fluidPage(
               p("Aquesta aplicació permet representar el conjunt de dades d'enviaments transfronterers de residus, originalment titulada ", 
                 em("Transboundary Shipments of Waste"), ", extretes del portal",
                 a("Eurostat", href = "https://ec.europa.eu/eurostat/web/waste/data", target = "_blank"), 
                 ".", br(), "Es mostren dues opcions de visualització. La primera es troba en el panell", strong("Mapamundi"), 
                 ", on s'hi troba una representació de les dades plasmades sobre el globus terraqüi, així com també una taula dinàmica en base a les dades seleccionades.
                 La segona opció es troba en el panell", strong("Graf"), ", i dóna una representació visual sobre els grafs, deixant triar quin tipus d'estructura es desitja.
                 Ambdós panells es controlen mitjançant el menú de l'esquerra, que serveix per a filtrar per categoria de les dades (importacions o exportacions dels residus),
                 any i país o països de procedència (en cas que s'hagi triat veure les importacions) o destinació (en cas que s'hagi seleccionat les exportacions). 
                 Tanmateix, pel segon panell no s'hi representen les exportacions de residus (per a més informació, veure secció 2.4.4 de la memòria), però sí que es diferencien els
                 residus perillosos (enllaços de color vermellós) dels que no ho són (enllaços de color verdós). A més a més, com més gruixut sigui aquest enllaç, 
                 més quantitat de residus es transporten per a aquell moviment.",
                 style = "text-align:justify;color:black;background-color:#d1f1f1;padding:15px;border-radius:10px"
               ),
               theme = shinytheme("cosmo"),
               titlePanel(""),
               sidebarLayout(
                 sidebarPanel(
                   conditionalPanel(
                     "input.tab == 1",
                     prettyRadioButtons("import.export", 
                                        label = "Categoria",
                                        choices = c("Importacions", "Exportacions"),
                                        shape = "round",
                                        animation = "pulse",
                                        plain = TRUE,
                                        inline = TRUE,
                                        outline = TRUE,
                                        selected = "Importacions",
                                        icon = icon("check", style = "color: #00CED1")
                     )
                   ),
                   conditionalPanel(
                     "input.tab == 2",
                     prettyRadioButtons("import.export", 
                                        label = "Categoria",
                                        choices = "Importacions",
                                        shape = "round",
                                        animation = "pulse",
                                        plain = TRUE,
                                        inline = TRUE,
                                        outline = TRUE,
                                        selected = "Importacions",
                                        icon = icon("check", style = "color: #00CED1")
                     )
                   ),
                   setSliderColor("Transparent", 1),
                   chooseSliderSkin("Flat", color = "DarkTurquoise"),
                   sliderInput("year",
                               label = "Any", 
                               min = min(waste$Year), 
                               max = max(waste$Year), 
                               value = ceiling((max(waste$Year) + min(waste$Year))/2), 
                               sep = "", 
                               step = 1,
                               ticks = TRUE, 
                               animate = TRUE
                   ),
                   selectizeInput("country1",
                                  label = NULL,
                                  choices = NULL, 
                                  multiple = TRUE,
                                  options = list(plugins = list("remove_button"),
                                                 placeholder = "Selecciona un o més països")
                   ),
                   conditionalPanel(
                     "input.tab == 2",
                     uiOutput("disposal.and.recovery.code"),
                     conditionalPanel(
                       'input.country1 != "" ',
                       prettyRadioButtons(inputId = "layout",
                                          label = "Mode de visualització", 
                                          choices = c("Arbre", "Circular", "Estrella", "Fruchterman i Reingol", "Aleatori"),
                                          shape = "round",
                                          animation = "pulse",
                                          plain = TRUE,
                                          inline = FALSE,
                                          outline = TRUE,
                                          selected = "Arbre",
                                          icon = icon("check", style = "color: #00CED1")
                       )
                     )
                   ),
                   width = 2),
                 mainPanel(
                   tabsetPanel(id = "tab",
                               tabPanel(title = "Mapamundi", id = "mapamundi", value = 1,
                                        splitLayout(
                                          plotlyOutput("plot"),
                                          conditionalPanel(
                                            'input.country1 != "" ',
                                            div(dataTableOutput("table"), style = "font-size:80%"),
                                            prettyRadioButtons("downloadType", 
                                                               label = "Selecciona un tipus de fitxer", 
                                                               choices = c("CSV" = ".csv", "XLSX" = ".xlsx", "TeX" = ".tex"),
                                                               shape = "round",
                                                               animation = "pulse",
                                                               plain = TRUE,
                                                               inline = TRUE,
                                                               outline = TRUE,
                                                               icon = icon("check", style = "color: #00CED1")
                                            ),
                                            downloadButton("downloadTable", "Descarregar taula")
                                          ),
                                          cellWidths = c("55%", "45%"),
                                          cellArgs = list(style = "height: 800px")
                                        )
                               ),
                               
                               tabPanel(title = "Graf", id = "network", value = 2,
                                        br(),
                                        span(textOutput("no_network"), style = "color:red"),
                                        visNetworkOutput("network", height = "1000px")
                               )
                   ),
                   width = 10
                 )
               )
             )
  ),
)




server <- function(input, output, session){
  ##########
  # Plotly #
  ##########
  
  observe({
    updateSelectizeInput(session,
                         "country1",
                         label = ifelse(input$import.export == "Importacions", "Des de", "Fins a"),
                         choices = c("Tots", sort(levels(world_coord$country))), 
                         server = TRUE)
  })
  
  mapData <- reactive({
    map_plotly(var = ifelse(input$import.export == "Importacions", 1, 2), any = input$year, pais = input$country1)
  })
  
  output$plot <- renderPlotly({
    mapData()$plot
  })
  
  output$table <- DT::renderDataTable({
    mapData()$taula
  },
  options = list(
    language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Catalan.json"),
    pageLength = 15,
    autowidth = TRUE,
    scrollX = TRUE,
    rownames = FALSE
  )
  )
  
  output$downloadTable <- downloadHandler(
    filename <- function(ext){
      sprintf("taula-%s-%s-%s%s", tolower(input$import.export), tolower(input$country1), input$year, input$downloadType)
    },
    
    content <- function(filename){
      if(input$downloadType == ".csv"){
        write.csv(mapData()$taula, filename, row.names = FALSE)
      } 
      
      else if(input$downloadType == ".xlsx"){
        write.xlsx(as.data.frame(mapData()$taula), filename, row.names = FALSE)
      }
      
      else if(input$downloadType == ".tex"){
        bold <- function(x) {paste('{\\textbf{',x,'}}', sep ='')}
        print(xtable(mapData()$taula, 
                     caption = sprintf("%s des de %s", input$import.export, input$country1), 
                     label =  sprintf("tab:%s-%s-%s", tolower(input$import.export), tolower(input$country1), input$year),
                     math.style.exponents = "UTF-8"),
              sanitize.colnames.function = bold,
              include.rownames = FALSE,
              booktabs = TRUE,
              floating = TRUE,
              file = filename)
      }
    }
  )
  
  ###########
  # Network #
  ###########
  
  netData <- reactive({
    if(!is.null(input$country1))
      network(any = input$year, pais = input$country1) 
    else
      NULL
  })
  
  
  
  output$network <- renderVisNetwork({
    if(!is.null(netData()) & !is.null(input$country1)){
      
      graf <- visNetwork(nodes = netData()$vis.nodes, edges = netData()$vis.links) %>% 
        #visLegend() %>% 
        visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE, hideColor = "#f7f7f7"),
                   nodesIdSelection = list(enabled = TRUE, main = "Selecciona un país")) %>%
        
        visExport(type = "png",
                  label = paste0("Exporta com a png"))
      
      if(input$layout == "Arbre")
        visIgraphLayout(graf, layout = "layout_as_tree", randomSeed = 123)
      
      else if(input$layout == "Circular")
        visIgraphLayout(graf, layout = "layout_in_circle", randomSeed = 123)
      
      else if(input$layout == "Estrella")
        visIgraphLayout(graf, layout = "layout_as_star", randomSeed = 123)
      
      else if(input$layout == "Fruchterman i Reingol")
        visIgraphLayout(graf, layout = "layout_with_fr", randomSeed = 123)
      
      else if(input$layout == "Aleatori")
        graf %>% visLayout(randomSeed = 123)
      
      else
        NULL
    }
    
    else
      NULL
  })
  
  output$no_network <- renderText({
    if(is.null(netData()$vis.nodes) & !is.null(input$country1))
      sprintf("No hi ha registres en el dataset d'importacions per al país %s i l'any %d.", input$country1, input$year)
    else
      NULL
  })
  
}

shinyApp(ui, server)

