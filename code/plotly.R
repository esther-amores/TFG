# Paquets necessaris
library(plotly)

# Carreguem les dades des d'altres scripts
load("preprocessing.RData")

# Projecció del mapa
m <- list(
  l = 0,
  r = 0,
  b = 0,
  t = 0,
  pad = 4
)

geo <- list(
  showframe = TRUE,
  showcoastlines = TRUE, 
  showocean = TRUE,
  showland = TRUE,
  showcountries = TRUE,
  projection = list(
    type = 'orthographic',
    rotation = list(lon = -9, lat = 40, roll = 0)
  ),
  plotolution = "110",
  landcolor = toRGB("gray95"),
  countrycolor = toRGB("gray80"),
  countrycolor = '#d1d1d1',
  oceancolor = '#c9d2e0'
)



select_data <- function(var, any, pais){
  if(var == 1){
    dades <- import %>% 
      filter(if(any(pais == "Tots")) Year == any else Year == any & From %in% pais) %>% 
      select(To) %>% 
      distinct()
    return(list("dades" = dades))
  }
  if(var == 2){
    dades <- export %>% 
      filter(if(any(pais == "Tots")) Year == any else Year == any & To %in% pais) %>% 
      select(From) %>%
      distinct()
    return(list("dades" = dades))
  }
}



map_plotly <- function(var, any, pais){
  
  ################
  # Importacions #
  ################
  
  if(var == 1){
    
    sum_tonnes <- import %>% 
      filter(if(any(pais == "Tots")) Year == any else Year == any & From %in% pais) %>% 
      group_by(To) %>% 
      summarise(sum = sum(Quantity.in.tonnes_to),
                sum_cap = sum(Quantity.in.kg.per.capita_to)) %>% 
      arrange(To)
    
    plot <- plot_geo() %>% 
      # From
      add_markers(
        data = import %>%
          filter(if(any(pais == "Tots")) Year == any else Year == any & From %in% pais) %>% 
          group_by(From) %>% 
          select(From, start_long, start_lat) %>% 
          distinct(),
        x = ~ start_long, 
        y = ~ start_lat, 
        text = ~ paste(sprintf("<b>%s</b>", From),
                       sprintf("<em>Quantitat total:</em> %.2f tones", sum(sum_tonnes$sum)),
                       sep = "<br />"),
        size = 5,
        color = I("red"),
        hoverinfo = "text", 
        alpha = 0.5,
        name = ifelse(var == 1, "Pa\u00EFsos importadors", "Pa\u00EFsos exportadors")
      ) %>%
      # To
      add_markers(
        data = import %>% 
          filter(if(any(pais == "Tots")) Year == any else Year == any & From %in% pais) %>%
          group_by(To) %>% 
          select(To, end_long, end_lat, Pop_to) %>% 
          distinct() %>% 
          arrange(To),
        x = ~ end_long, 
        y = ~ end_lat, 
        text = ~ paste(sprintf("<b>%s</b>", To), 
                       sprintf("<em>Poblaci\U00F3:</em> %s habitants", format(Pop_to, big.mark = ",")), 
                       sprintf("<em>Quantitat:</em> %.2f tones", sum_tonnes$sum),
                       sprintf("<em>Quantitat per capita:</em> %.4f kg", sum_tonnes$sum_cap),
                       sep = "<br />"),
        size = 5,
        color = I("blue"),
        hoverinfo = "text", 
        alpha = 0.5,
        name = ifelse(var == 1, "Pa\u00EFsos exportadors", "Pa\u00EFsos importadors")
      ) %>%
      # Segments
      add_segments(
        data = import %>% 
          filter(if(any(pais == "Tots")) Year == any else Year == any & From %in% pais), 
        x = ~ start_long, xend = ~ end_long,
        y = ~ start_lat, yend = ~ end_lat,
        size = I(1.5), 
        hoverinfo = "text", 
        alpha = 0.5,
        hoverinfo = "none",
        name = ifelse(var == 1, "Importacions", "Exportacions")
      ) %>%
      layout(geo = geo, showlegend = TRUE, legend = list(title = list(text="<b>Llegenda</b>"), orientation = "h"), autosize = FALSE, width = 600, height = 600, margin = m) 
    
    taula <- import %>% 
      filter(if(any(pais == "Tots")) Year == any else Year == any & From %in% pais) %>% 
      group_by(To) %>% 
      select(To, Pop_to, Quantity.in.tonnes_to, Quantity.in.kg.per.capita_to) %>% 
      summarise(sum = sum(Quantity.in.tonnes_to),
                sum_cap = sum(Quantity.in.kg.per.capita_to),
                pop = mean(Pop_to)) %>% 
      arrange(desc(sum)) %>% 
      rename("Pa\u00EDs exportador" = To, "Poblaci\U00F3" = pop, "Quantitat en tones" = sum, "Quantitat per capita" = sum_cap)
    
    return(list("plot" = plot, "taula" = taula))
  }
  
  
  ################
  # Exportacions #
  ################
  
  if(var == 2){
    sum_tonnes <- export %>% 
      filter(if(any(pais == "Tots")) Year == any else Year == any & To %in% pais) %>% 
      group_by(From) %>% 
      summarise(sum = sum(Quantity.in.tonnes_from),
                sum_cap = sum(Quantity.in.kg.per.capita_from)) %>% 
      arrange(From)
    
    plot <- plot_geo() %>% 
      # From
      add_markers(
        data = export %>% 
          filter(if(any(pais == "Tots")) Year == any else Year == any & To %in% pais) %>%
          group_by(From) %>% 
          select(From, start_long, start_lat, Pop_from) %>%
          distinct(),
        x = ~ start_long, 
        y = ~ start_lat, 
        text = ~ paste(sprintf("<b>%s</b>", From), 
                       sprintf("<em>Poblaci\U00F3:</em> %s habitants", format(Pop_from, big.mark = ",")), 
                       sprintf("<em>Quantitat:</em> %.2f tones", sum_tonnes$sum),
                       sprintf("<em>Quantitat per capita:</em> %.4f kg", sum_tonnes$sum_cap),
                       sep = "<br />"),
        size = 5,
        color = I("red"),
        hoverinfo = "text", 
        alpha = 0.5,
        name = ifelse(var == 1, "Pa\u00EFsos exportadors", "Pa\u00EFsos importadors")
      ) %>%
      # To
      add_markers(
        data = export %>% 
          filter(if(any(pais == "Tots")) Year == any else Year == any & To %in% pais) %>% 
          group_by(To) %>%
          select(To, end_long, end_lat) %>%
          distinct(),
        x = ~ end_long, 
        y = ~ end_lat, 
        text = ~ paste(sprintf("<b>%s</b>", To),
                       sprintf("<em>Quantitat total:</em> %.2f tones", sum(sum_tonnes$sum)),
                       sep = "<br />"),        
        size = 5, 
        color = I("blue"),
        hoverinfo = "text", 
        alpha = 0.5,
        name = ifelse(var == 1, "Pa\u00EFsos importadors", "Pa\u00EFsos exportadors")
      ) %>%
      # Segments
      add_segments(
        data = export %>% 
          filter(if(any(pais == "Tots")) Year == any else Year == any & To %in% pais),
        x = ~ start_long, xend = ~ end_long,
        y = ~ start_lat, yend = ~ end_lat,
        size = I(1.5),
        hoverinfo = "none", 
        alpha = 0.5,
        name = ifelse(var == 1, "Importacions", "Exportacions")
      ) %>%
      layout(geo = geo, showlegend = TRUE, legend = list(title = list(text="<b>Llegenda</b>"), orientation = "h"), autosize = FALSE, width = 600, height = 600, margin = m) 
    
    taula <- export %>% 
      filter(if(any(pais == "Tots")) Year == any else Year == any & To %in% pais) %>% 
      group_by(From) %>% 
      select(From, Pop_from, Quantity.in.tonnes_from, Quantity.in.kg.per.capita_from) %>% 
      summarise(sum = sum(Quantity.in.tonnes_from),
                sum_cap = sum(Quantity.in.kg.per.capita_from),
                pop = mean(Pop_from)) %>% 
      ungroup() %>% 
      arrange(desc(sum)) %>% 
      rename("Pa\u00EDs exportador" = From, "Poblaci\U00F3" = pop, "Quantitat en tones" = sum, "Quantitat per capita" = sum_cap) 
    
    return(list("plot" = plot, "taula" = taula))
  }
}






