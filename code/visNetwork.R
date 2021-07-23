# Paquets necessaris
library(igraph)
library(visNetwork)
library(RColorBrewer)

network <- function(any, pais){ 
  if(prod(dim(import %>% filter(if(any(pais == "Tots")) Year == any else Year == any & From %in% pais))) == 0)
    return(NULL)
  
  else{
    
    links <- import %>%
      filter(if(any(pais == "Tots")) Year == any 
             else Year == any & From %in% pais) %>% 
      select(From, To, Quantity.in.tonnes_to, Quantity.in.kg.per.capita_to, hazardous) %>% 
      rename(weight = Quantity.in.tonnes_to, from = From, to = To) 
    
    nodes <- data.frame(country = union(links$from, links$to)) %>% 
      left_join(y = import %>% filter(Year == any) %>% select(To, Pop_to) %>% distinct(), 
                by = c("country" = "To")) %>% 
      rename(label = country)
    
    vis.nodes <- nodes
    vis.links <- links
    
    #########
    # Nodes #
    #########
    
    vis.nodes$id <- vis.nodes$label
    vis.nodes$shadow <- FALSE
    vis.nodes$borderWidth <- 2
    vis.nodes$color.background <- ifelse(nodes$label %in% pais, "red", "blue") 
    vis.nodes$color.border <- "black"
    vis.nodes$group <- ifelse(nodes$label %in% pais, "Pa\u00EFsos importadors", "Pa\u00EFsos exportadors")
    vis.nodes$margin <- 1
    
    #########
    # Links #
    #########
    
    vis.links$id <- 1:nrow(vis.links)
    vis.links$width <- links$weight/10000
    vis.links$color <- ifelse(links$hazardous == 1, "#802621", "#1b852e")
    vis.links$arrows <- "from"
    vis.links$smooth <- FALSE
    vis.links$shadow <- FALSE

    
    return(list("vis.links" = vis.links, "vis.nodes" = vis.nodes))
    
  }
}
