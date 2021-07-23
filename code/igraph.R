# Paquets necessaris
library(xtable)
library(network)
library(sna)
library(tidyverse)
library(ape)
library(scales)

# Es carreguen les dades des d'altres scripts
source("visNetwork.R")


########################
# Anàlisi descriptiva  #
########################

# Es crea un objecte de classe igraph
dades <- network(2018, "Tots")
g.import <- graph_from_data_frame(d = dades$vis.links, vertices = dades$vis.nodes, directed = TRUE)

class(g.import)
summary(g.import)

# Es pot accedir als seus atributs
E(g.import)$weight 
E(g.import)$hazardous
V(g.import)$Pop_to

# S'assigna un nom
g.import$name <- "Importacions"

# El número de nodes
vcount(g.import)

# El número d'enllaços
ecount(g.import)

# És un multi-graf dirigit, ponderat i està connectat 
is_simple(g.import)
is_directed(g.import)
is_weighted(g.import)
is_connected(g.import)

# El graf està feblement connectat perquè el seu graf subjacent està connectat
is_connected(g.import, mode = "weak")
is_connected(g.import, mode = "strong")

# Nodes adjacents (veïns) a tots els països
neighbors(g.import, 1)
unique(neighbors(g.import, 1))

# Visualització de grafs
png(sprintf("../LaTeX/figures/plot-%s-%s-%s-%s.png", "igraph", "importacions", "tots", "2018"), width = 1000, height = 1000, res = 100)
par(mfrow = c(2,2))
igraph_options(vertex.size = 5, vertex.label = NA, edge.arrow.size = 0.05, cex.main = 3)
plot(g.import, layout = layout_in_circle, main = "Circular")
plot(g.import, layout = layout_with_fr, main = "Fruchterman i Reingol")
plot(g.import, layout = layout_as_tree(g.import, circular = TRUE), main = "Arbre (disp. circular)")
plot(g.import, layout = layout_as_tree, main = "Arbre")
dev.off()

# Grau dels nodes
igraph::degree(g.import)
igraph::degree(g.import, mode = "in")
igraph::degree(g.import, mode = "out")

# Histograma del grau dels nodes
degree.hist <- ggplot(data.frame(degree = igraph::degree(g.import)), aes(x = degree)) +
  geom_histogram(binwidth = 100, fill = "#d5feff", color = "#00CED1", size = 0.3) + 
  scale_x_continuous(breaks = seq(0, max(igraph::degree(g.import)), 250)) +
  labs(x = "Grau dels vèrtexs", y = "Freqüència") +
  theme_bw()

ggsave(sprintf("../LaTeX/figures/plot-%s-%s-%s-%s.png", "degree", "importacions", "tots", "2018"),
       plot = degree.hist, width = 15, height = 10, units = "cm")


# Taula del grau dels nodes
degree.table <- data.frame(degree = igraph::degree(g.import),
                           degree.in = igraph::degree(g.import, mode = "in"),
                           degree.out = igraph::degree(g.import, mode = "out")) %>% 
  arrange(-degree) %>% 
  rownames_to_column()

bold <- function(x) {paste('{\\textbf{',x,'}}', sep ='')}
print(xtable(degree.table %>% rename("$d_v$" = "degree",
                                     "$d^{in}_v$" = "degree.in",
                                     "$d^{out}_v$" = "degree.out"),
             digits = 0), 
      sanitize.colnames.function = bold,
      booktabs = TRUE,
      floating = TRUE,
      file = sprintf("../LaTeX/tables/taula-%s-%s-%s-%s.tex", "degree", "importacions", "tots", "2018"))

# Força dels nodes
strength.hist <- ggplot(data.frame(strength = strength(g.import)), aes(x = strength)) +
  geom_histogram(binwidth = 100000, fill = "#f1ffd5", color = "#c0ea6c", size = 0.3) + 
  scale_x_continuous(breaks = seq(0, max(strength(g.import)), 1000000)) +
  labs(x = "Força dels vèrtexs", y = "Freqüència") +
  theme_bw()

ggsave(sprintf("../LaTeX/figures/plot-%s-%s-%s-%s.png", "strength", "importacions", "tots", "2018"),
       plot = strength.hist, width = 15, height = 10, units = "cm")

# Es simplifica el graf per a poder calcular la mitjana dels graus dels nodes veïns
g.import.simple <- igraph::simplify(g.import, remove.multiple = TRUE, remove.loops = TRUE)
is_simple(g.import.simple)

# Taula del grau dels nodes
degree.table.hazard <- data.frame(degree = igraph::degree(g.import.hazard),
                           degree.in = igraph::degree(g.import.hazard, mode = "in"),
                           degree.out = igraph::degree(g.import.hazard, mode = "out")) %>% 
  arrange(-degree) %>% 
  rownames_to_column()

degree.table.nonhazard <- data.frame(degree = igraph::degree(g.import.nonhazard),
                                  degree.in = igraph::degree(g.import.nonhazard, mode = "in"),
                                  degree.out = igraph::degree(g.import.nonhazard, mode = "out")) %>% 
  arrange(-degree) %>% 
  rownames_to_column()

# Mitjana dels graus dels nodes veïns
a.nn.deg <- knn(g.import.simple, V(g.import.simple))$knn
a.nn.deg.data <- data.frame(knn = a.nn.deg, degree = degree(g.import.simple))

knn.plot <- ggplot(a.nn.deg.data, aes(x = degree, y = knn)) +
  geom_point() +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") + 
  geom_smooth(method = lm, se = FALSE, color = "red3") + 
  labs(x = "Grau dels vèrtexs", y = "Grau en mitjana del veí") +
  theme_classic()

ggsave(sprintf("../LaTeX/figures/plot-%s-%s-%s-%s.png", "knn", "importacions", "tots", "2018"),
       plot = knn.plot, width = 15, height = 10, units = "cm")

# Centralitat dels nodes
A <- as_adjacency_matrix(g.import, sparse = FALSE)
g <- network::as.network.matrix(A)

write.csv(A, file = sprintf("../LaTeX/tables/taula-%s-%s-%s-%s.csv", "adjacencia", "importacions", "tots", "2018"))

print(xtable(A, digits = 2), 
      sanitize.colnames.function = bold,
      include.rownames = FALSE,
      booktabs = TRUE,
      floating = TRUE,
      file = sprintf("../LaTeX/tables/taula-%s-%s-%s-%s.tex", "adjacencia", "importacions", "tots", "2018"))

special <- which(names(igraph::degree(g.import)) == c("Alemanya", "Bèlgica", "França"))
colors <- rep("blue", length(igraph::degree(g.import)))
colors[special] <- "yellow"

png(sprintf("../LaTeX/figures/plot-%s-%s-%s-%s.png", "centralitats", "importacions", "tots", "2018"), width = 1000, height = 1000, res = 75)
par(mfrow = c(2,2))
sna::gplot.target(g, sna::degree(g),
                  main = "Graus", 
                  cex.main = 2,
                  circ.col = "skyblue", 
                  circ.lab = FALSE, 
                  usearrows = FALSE, 
                  vertex.col = colors,
                  edge.col = "darkgray")

sna::gplot.target(g, sna::betweenness(g),
                  main = "Interrelació", 
                  cex.main = 2,
                  circ.col = "skyblue", 
                  circ.lab = FALSE, 
                  usearrows = FALSE, 
                  vertex.col = colors,
                  edge.col = "darkgray")

sna::gplot.target(g, sna::closeness(g),
                  main = "Proximitat", 
                  cex.main = 2,
                  circ.col = "skyblue", 
                  circ.lab = FALSE, 
                  usearrows = FALSE, 
                  vertex.col = colors,
                  edge.col = "darkgray")

sna::gplot.target(g, sna::evcent(g),
                  main = "Vector propi", 
                  cex.main = 2,
                  circ.col = "skyblue", 
                  circ.lab = FALSE, 
                  usearrows = FALSE, 
                  vertex.col = colors,
                  edge.col = "darkgray")
dev.off()


centralitats <- data.frame(nom = V(g.import)$name,
                           grau = sna::degree(g),
                           proximitat = sna::closeness(g),
                           interrelacio = sna::betweenness(g),
                           vep = sna::evcent(g)) %>%
  arrange(desc(grau)) 

print(xtable(centralitats %>% rename("País" = "nom", 
                                     "Grau" = "grau", 
                                     "Proximitat" = "proximitat",
                                     "Interrelació" = "interrelacio",
                                     "Vector propi" = "vep"),
             digits = 2), 
      sanitize.colnames.function = bold,
      include.rownames = FALSE,
      booktabs = TRUE,
      floating = TRUE,
      file = sprintf("../LaTeX/tables/taula-%s-%s-%s-%s.tex", "centralitats", "importacions", "tots", "2018"))


# Caracterització dels enllaços
eb <- edge_betweenness(g.import)
E(g.import)[order(eb, decreasing=T)[1:3]]

# Cliques
table(sapply(cliques(g.import), length))
cliques(g.import)[sapply(cliques(g.import), length) == 11]
table(sapply(max_cliques(g.import), length))
clique_num(g.import)

# Cores
cores <- coreness(g.import)

# S'assigna un color a cada número de cores
cores.d <- as.data.frame(cores) %>% arrange(cores)
valor.previ <- 0
n <- length(unique(cores.d$cores))
color <- colorRampPalette(brewer.pal(8, "Spectral"))(n)
color.tikz <- sprintf("\\color[HTML]{%s}\\bullet", str_sub(color, start = -6))
color.actual <- 0

for(i in 1:nrow(cores.d)){
  if(cores.d$cores[i] != valor.previ){
    color.actual <- color.actual + 1
  }
  cores.d$color[i] <- color[color.actual]
  cores.d$color.tikz[i] <- color.tikz[color.actual]
  valor.previ <- cores.d$cores[i]
}

png(sprintf("../LaTeX/figures/plot-%s-%s-%s-%s.png", "coreness", "importacions", "tots", "2018"), width = 1000, height = 1000, res = 75)
sna::gplot.target(g, cores.d$cores,
                  circ.col = "skyblue", 
                  circ.lwd = 3,
                  circ.lab = FALSE, 
                  usearrows = FALSE, 
                  vertex.col = cores.d$color,
                  edge.col = "darkgray") 

legend("right", legend = unique(cores.d$cores), pch = 19, col = unique(cores.d$color),
       inset = 0, bty = "n", cex = 1.5, title = "k-cores")
dev.off()

print(xtable(cores.d %>% rownames_to_column() %>% select(rowname, cores, color.tikz) %>% rename("País" = "rowname", "$k$-cores" = "cores", "Color" = "color.tikz"),
             digits = 0), 
      sanitize.colnames.function = bold,
      booktabs = TRUE,
      floating = TRUE,
      file = sprintf("../LaTeX/tables/taula-%s-%s-%s-%s.tex", "coreness", "importacions", "tots", "2018"))

# Díades
dyad_census(g.import.simple)

# Densitat global de la xarxa 
ecount(g.import)
vcount(g.import)

density <- function(g){
  num <- abs(ecount(g))
  if(is_directed(g)){
    den <- abs(vcount(g))*(abs(vcount(g))-1)
  }
  else{
    den <- abs(vcount(g))*(abs(vcount(g))-1)/2
  }
  return(num/den)
}

density(g.import)
density(g.import.simple)

# És el mateix que:
edge_density(g.import)
edge_density(g.import.simple)

# Densitat de subgrafs
c(which(V(g.import)$name == c("Alemanya", "Bèlgica", "França")), 13) # el 13 fa referència a Països Baixos

ego_alemanya <- induced_subgraph(g.import.simple, vids = neighborhood(g.import.simple, 1, 1)[[1]])
ego_belgica <- induced_subgraph(g.import.simple, vids = neighborhood(g.import.simple, 6, 6)[[1]])
ego_franca <- induced_subgraph(g.import.simple, vids = neighborhood(g.import.simple, 20, 20)[[1]])
ego_pb <- induced_subgraph(g.import.simple, vids = neighborhood(g.import.simple, 13, 13)[[1]])
edge_density(induced_subgraph(g.import.simple, vids = neighborhood(g.import.simple, 1, 13)[[1]]))

edge_density(ego_alemanya)
edge_density(ego_belgica)
edge_density(ego_franca)
edge_density(ego_pb)

# Transitivitat/coeficient d'agrupació (clustering)
transitivity(g.import)
length(triangles(g.import))

# Reciprocitat
reciprocity(g.import.simple, mode = "default")
reciprocity(g.import.simple, mode = "ratio")

# Connecitivitat
comps <- decompose(g.import)
table(sapply(comps, vcount))
(108/vcount(g.import))*100

# Distància mitjana
mean_distance(g.import)
log(vcount(g.import))

# Càlcul del diàmetre: el valor de la distància més gran
diameter(g.import, weights = NA)

# Transitivitat
transitivity(g.import)

# Clustering
g.import.un <- graph_from_data_frame(d = dades$vis.links, vertices = dades$vis.nodes, directed = FALSE)
g.import.un.simple <- simplify(g.import.un, remove.multiple = TRUE, remove.loops = TRUE)

kc <- cluster_fast_greedy(g.import.un.simple)
length(kc)
sizes(kc)
membership(kc)

memb.table <- data.frame(membership = as.matrix(membership(kc))) %>% 
  arrange(membership) %>% 
  rownames_to_column()

data <- left_join(x = memb.table, y = degree.table %>% select(-degree.in, -degree.out)) %>% 
  left_join(y = cores.d %>% rownames_to_column() %>% select(rowname, cores))

data.table <- data %>% 
  group_by(membership) %>%
  summarise(sum.degrees = sum(degree),
            sum.cores = sum(cores))

print(xtable(data %>% rename("País" = "rowname",
                             "Clúster" = "membership",
                             "$d_v$" = "degree",
                             "Nuclis" = "cores"),
             digits = 0), 
      sanitize.colnames.function = bold,
      include.rownames = FALSE,
      booktabs = TRUE,
      floating = TRUE,
      file = sprintf("../LaTeX/tables/taula-%s-%s-%s-%s.tex", "membership", "importacions", "tots", "2018"))

print(xtable(data.table %>% rename("Clúster" = "membership",
                                   "$\\sum d_v$" = "sum.degrees",
                                   "$\\sum k$" = "sum.cores"),
             digits = 0), 
      sanitize.colnames.function = bold,
      include.rownames = FALSE,
      booktabs = TRUE,
      floating = TRUE,
      file = sprintf("../LaTeX/tables/taula-%s-%s-%s-%s.tex", "membership.summary", "importacions", "tots", "2018"))

png(sprintf("../LaTeX/figures/plot-%s-%s-%s-%s.png", "clustering", "importacions", "tots", "2018"), width = 2000, height = 2000, res = 100)
plot(kc, g.import.un.simple)
dev.off()

png(sprintf("../LaTeX/figures/plot-%s-%s-%s-%s.png", "dendrograma", "importacions", "tots", "2018"), width = 2000, height = 2000, res = 100)
plot_dendrogram(kc)
dev.off()

# Assortativitat
assortativity_degree(g.import.simple)






##########################
# Anàlisi descriptiva II #
##########################

# Conveni general de Basilea: taula
print(xtable(basel_conv %>% 
               mutate_at(.vars = vars(hazardous), 
                         recode, "1" = "Sí", "0" = "No") %>% 
               rename("Codi" = "code", 
                      "Descripció" = "description", 
                      "Perillós" = "hazardous"),
             digits = 0), 
      sanitize.colnames.function = bold,
      include.rownames = FALSE,
      booktabs = TRUE,
      floating = TRUE,
      file = sprintf("../LaTeX/tables/taula-%s.tex", "conveni_basilea"))

# Conveni general de Basilea: plot per a la variable en general
basel_conv.data <- basel_conv %>% 
  group_by(hazardous) %>% 
  mutate(cnt = n(),
         pct = percent(cnt / nrow(.), accuracy = 1)) %>% 
  select(cnt, pct) %>% 
  unique() %>% 
  ungroup() %>% 
  mutate(hazardous = factor(hazardous, levels = c(0, 1), labels = c("No", "Sí")))

basel_conv.plot <- ggplot(basel_conv.data, aes(x = "", y = cnt, fill = factor(hazardous))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("#00CED1", "#057dA4")) +
  geom_text(aes(label = sprintf("%d \n %s", cnt, pct)),
            position = position_stack(vjust = 0.5),
            col="white") +
  theme_void() +
  labs(fill="Perillositat dels residus")

ggsave(sprintf("../LaTeX/figures/plot-%s.png", "conveni_basilea"),
       plot = basel_conv.plot, width = 15, height = 10, units = "cm")

# Conveni general de Basilea: plot per a les importacions
basel_conv.import <- import %>% 
  filter(Year == 2018) %>% 
  group_by(hazardous) %>% 
  mutate(cnt = n(),
         pct = percent(cnt / nrow(.), accuracy = 1)) %>% 
  select(cnt, pct) %>% 
  unique() %>% 
  ungroup() %>% 
  mutate(hazardous = factor(hazardous, levels = c(0, 1), labels = c("No", "Sí")))

basel_conv.import.plot <- ggplot(basel_conv.import, aes(x = "", y = cnt, fill = factor(hazardous))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("#00CED1", "#057dA4")) +
  geom_text(aes(label = sprintf("%d \n %s", cnt, pct)),
            position = position_stack(vjust = 0.5),
            col="white") +
  theme_void() +
  labs(fill="Perillositat dels residus")

ggsave(sprintf("../LaTeX/figures/plot-%s-%s-%s-%s.png", "conveni_basilea", "importacions", "tots", "2018"),
       plot = basel_conv.import.plot, width = 15, height = 10, units = "cm")



# Es crea una funció network nova per a filtrar els datasets segons la perillositat dels residus
network2 <- function(any, pais, perillositat){ 
  if(prod(dim(import %>% filter(if(any(pais == "Tots")) Year == any else Year == any & From %in% pais))) == 0)
    return(NULL)
  
  else{
    
    links <- import %>%
      filter(if(any(pais == "Tots")) Year == any & hazardous == perillositat
             else Year == any & From %in% pais & hazardous == perillositat) %>% 
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

# Es creen 2 objectes de classe igraph
dades.hazard <- network2(2018, "Tots", 1)
dades.nonhazard <- network2(2018, "Tots", 0)

g.import.hazard <- graph_from_data_frame(d = dades.hazard$vis.links, vertices = dades.hazard$vis.nodes, directed = TRUE)
g.import.nonhazard <- graph_from_data_frame(d = dades.nonhazard$vis.links, vertices = dades.nonhazard$vis.nodes, directed = TRUE)

# El número de nodes
vcount(g.import.hazard)
vcount(g.import.nonhazard)

# El número d'enllaços
ecount(g.import.hazard)
ecount(g.import.nonhazard)

# Visualització de grafs
png(sprintf("../LaTeX/figures/plot-%s-%s-%s-%s.png", "igraph", "importacions", "hazard", "2018"), width = 1000, height = 1000, res = 100)
par(mfrow = c(2,2))
igraph_options(vertex.size = 5, vertex.label = NA, edge.arrow.size = 0.05, cex.main = 3)
plot(g.import.hazard, layout = layout_in_circle, main = "Circular")
plot(g.import.hazard, layout = layout_with_fr, main = "Fruchterman i Reingol")
plot(g.import.hazard, layout = layout_as_tree(g.import.hazard, circular = TRUE), main = "Arbre (disp. circular)")
plot(g.import.hazard, layout = layout_as_tree, main = "Arbre")
dev.off()

png(sprintf("../LaTeX/figures/plot-%s-%s-%s-%s.png", "igraph", "importacions", "nonhazard", "2018"), width = 1000, height = 1000, res = 100)
par(mfrow = c(2,2))
igraph_options(vertex.size = 5, vertex.label = NA, edge.arrow.size = 0.05, cex.main = 3)
plot(g.import.nonhazard, layout = layout_in_circle, main = "Circular")
plot(g.import.nonhazard, layout = layout_with_fr, main = "Fruchterman i Reingol")
plot(g.import.nonhazard, layout = layout_as_tree(g.import.nonhazard, circular = TRUE), main = "Arbre (disp. circular)")
plot(g.import.nonhazard, layout = layout_as_tree, main = "Arbre")
dev.off()

# Grau dels nodes
sort(igraph::degree(g.import.hazard), decreasing = TRUE)[1:5]
sort(igraph::degree(g.import.hazard, mode = "in"), decreasing = TRUE)[1:5]
sort(igraph::degree(g.import.hazard, mode = "out"), decreasing = TRUE)[1:5]

sort(igraph::degree(g.import.nonhazard), decreasing = TRUE)[1:5]
sort(igraph::degree(g.import.nonhazard, mode = "in"), decreasing = TRUE)[1:5]
sort(igraph::degree(g.import.nonhazard, mode = "out"), decreasing = TRUE)[1:5]

# Nodes adjacents (veïns) a tots els països
neighbors(g.import.hazard, 1)
unique(neighbors(g.import.hazard, 1))

neighbors(g.import.nonhazard, 1)
unique(neighbors(g.import.nonhazard, 1))

# Es simplifiquen els grafs per a poder calcular la mitjana dels graus dels nodes veïns
g.import.hazard.simple <- igraph::simplify(g.import.hazard, remove.multiple = TRUE, remove.loops = TRUE)
g.import.nonhazard.simple <- igraph::simplify(g.import.nonhazard, remove.multiple = TRUE, remove.loops = TRUE)

is_simple(g.import.hazard.simple)
is_simple(g.import.nonhazard.simple)

# Mitjana dels graus dels nodes veïns
a.nn.deg <- knn(g.import.hazard.simple, V(g.import.hazard.simple))$knn
a.nn.deg.data <- data.frame(knn = a.nn.deg, degree = igraph::degree(g.import.hazard.simple))

knn.plot <- ggplot(a.nn.deg.data, aes(x = degree, y = knn)) +
  geom_point() +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") + 
  geom_smooth(method = lm, se = FALSE, color = "red3") + 
  labs(x = "Grau dels vèrtexs", y = "Grau en mitjana del veí") +
  theme_classic()

ggsave(sprintf("../LaTeX/figures/plot-%s-%s-%s-%s.png", "knn", "importacions", "hazard", "2018"),
       plot = knn.plot, width = 15, height = 10, units = "cm")

##

a.nn.deg <- knn(g.import.nonhazard.simple, V(g.import.nonhazard.simple))$knn
a.nn.deg.data <- data.frame(knn = a.nn.deg, degree = igraph::degree(g.import.nonhazard.simple))

knn.plot <- ggplot(a.nn.deg.data, aes(x = degree, y = knn)) +
  geom_point() +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") + 
  geom_smooth(method = lm, se = FALSE, color = "red3") + 
  labs(x = "Grau dels vèrtexs", y = "Grau en mitjana del veí") +
  theme_classic()

ggsave(sprintf("../LaTeX/figures/plot-%s-%s-%s-%s.png", "knn", "importacions", "nonhazard", "2018"),
       plot = knn.plot, width = 15, height = 10, units = "cm")


# Centralitat dels nodes
A <- as_adjacency_matrix(g.import.hazard, sparse = FALSE)
g.hazard <- network::as.network.matrix(A)

write.csv(A, file = sprintf("../LaTeX/tables/taula-%s-%s-%s-%s.csv", "adjacencia", "importacions", "hazard", "2018"))

special <- which(names(igraph::degree(g.import.hazard)) == c("Alemanya", "Bèlgica", "França"))
colors <- rep("blue", length(igraph::degree(g.import.hazard)))
colors[special] <- "yellow"

png(sprintf("../LaTeX/figures/plot-%s-%s-%s-%s.png", "centralitats", "importacions", "hazard", "2018"), width = 1000, height = 1000, res = 75)
par(mfrow = c(2,2))
sna::gplot.target(g.hazard, sna::degree(g.hazard),
                  main = "Graus", 
                  cex.main = 2,
                  circ.col = "skyblue", 
                  circ.lab = FALSE, 
                  usearrows = FALSE, 
                  vertex.col = colors,
                  edge.col = "darkgray")

sna::gplot.target(g.hazard, sna::betweenness(g.hazard),
                  main = "Interrelació", 
                  cex.main = 2,
                  circ.col = "skyblue", 
                  circ.lab = FALSE, 
                  usearrows = FALSE, 
                  vertex.col = colors,
                  edge.col = "darkgray")

sna::gplot.target(g.hazard, sna::closeness(g.hazard),
                  main = "Proximitat", 
                  cex.main = 2,
                  circ.col = "skyblue", 
                  circ.lab = FALSE, 
                  usearrows = FALSE, 
                  vertex.col = colors,
                  edge.col = "darkgray")

sna::gplot.target(g.hazard, sna::evcent(g.hazard),
                  main = "Vector propi", 
                  cex.main = 2,
                  circ.col = "skyblue", 
                  circ.lab = FALSE, 
                  usearrows = FALSE, 
                  vertex.col = colors,
                  edge.col = "darkgray")
dev.off()

##

A <- as_adjacency_matrix(g.import.nonhazard, sparse = FALSE)
g.nonhazard <- network::as.network.matrix(A)

write.csv(A, file = sprintf("../LaTeX/tables/taula-%s-%s-%s-%s.csv", "adjacencia", "importacions", "nonhazard", "2018"))

special <- which(names(igraph::degree(g.import.nonhazard)) == c("Alemanya", "Bèlgica", "França"))
colors <- rep("blue", length(igraph::degree(g.import.nonhazard)))
colors[special] <- "yellow"

png(sprintf("../LaTeX/figures/plot-%s-%s-%s-%s.png", "centralitats", "importacions", "nonhazard", "2018"), width = 1000, height = 1000, res = 75)
par(mfrow = c(2,2))
sna::gplot.target(g.nonhazard, sna::degree(g.nonhazard),
                  main = "Graus", 
                  cex.main = 2,
                  circ.col = "skyblue", 
                  circ.lab = FALSE, 
                  usearrows = FALSE, 
                  vertex.col = colors,
                  edge.col = "darkgray")

sna::gplot.target(g.nonhazard, sna::betweenness(g.nonhazard),
                  main = "Interrelació", 
                  cex.main = 2,
                  circ.col = "skyblue", 
                  circ.lab = FALSE, 
                  usearrows = FALSE, 
                  vertex.col = colors,
                  edge.col = "darkgray")

sna::gplot.target(g.nonhazard, sna::closeness(g.nonhazard),
                  main = "Proximitat", 
                  cex.main = 2,
                  circ.col = "skyblue", 
                  circ.lab = FALSE, 
                  usearrows = FALSE, 
                  vertex.col = colors,
                  edge.col = "darkgray")

sna::gplot.target(g.nonhazard, sna::evcent(g.nonhazard),
                  main = "Vector propi", 
                  cex.main = 2,
                  circ.col = "skyblue", 
                  circ.lab = FALSE, 
                  usearrows = FALSE, 
                  vertex.col = colors,
                  edge.col = "darkgray")
dev.off()

# Caracterització dels enllaços
eb <- edge_betweenness(g.import.hazard)
E(g.import.hazard)[order(eb, decreasing=T)[1:3]]

eb <- edge_betweenness(g.import.nonhazard)
E(g.import.nonhazard)[order(eb, decreasing=T)[1:3]]

# Cliques
table(sapply(cliques(g.import.hazard), length))
cliques(g.import.hazard)[sapply(cliques(g.import.hazard), length) == 9]
table(sapply(max_cliques(g.import.hazard), length))
clique_num(g.import.hazard)

table(sapply(cliques(g.import.nonhazard), length))
cliques(g.import.nonhazard)[sapply(cliques(g.import.nonhazard), length) == 9]
table(sapply(max_cliques(g.import.nonhazard), length))
clique_num(g.import.nonhazard)

# Cores
cores.hazard <- coreness(g.import.hazard)

# S'assigna un color a cada número de cores
cores.d.hazard <- as.data.frame(cores.hazard) %>% arrange(cores.hazard)
valor.previ <- 0
n <- length(unique(cores.d.hazard$cores))
color <- colorRampPalette(brewer.pal(8, "Spectral"))(n)
color.tikz <- sprintf("\\color[HTML]{%s}\\bullet", str_sub(color, start = -6))
color.actual <- 0

for(i in 1:nrow(cores.d.hazard)){
  if(cores.d.hazard$cores[i] != valor.previ){
    color.actual <- color.actual + 1
  }
  cores.d.hazard$color[i] <- color[color.actual]
  cores.d.hazard$color.tikz[i] <- color.tikz[color.actual]
  valor.previ <- cores.d.hazard$cores[i]
}

png(sprintf("../LaTeX/figures/plot-%s-%s-%s-%s.png", "coreness", "importacions", "hazard", "2018"), width = 1000, height = 1000, res = 75)
sna::gplot.target(g.hazard, cores.d.hazard$cores,
                  circ.col = "skyblue", 
                  circ.lwd = 3,
                  circ.lab = FALSE, 
                  usearrows = FALSE, 
                  vertex.col = cores.d.hazard$color,
                  edge.col = "darkgray") 

legend("right", legend = unique(cores.d.hazard$cores), pch = 19, col = unique(cores.d.hazard$color),
       inset = 0, bty = "n", cex = 1.5, title = "k-cores")
dev.off()

print(xtable(cores.d.hazard %>% rownames_to_column() %>% select(rowname, cores.hazard, color.tikz) %>% rename("País" = "rowname", "$k$-cores" = "cores.hazard", "Color" = "color.tikz"),
             digits = 0), 
      sanitize.colnames.function = bold,
      booktabs = TRUE,
      floating = TRUE,
      file = sprintf("../LaTeX/tables/taula-%s-%s-%s-%s.tex", "coreness", "importacions", "hazard", "2018"))

##

cores.nonhazard <- coreness(g.import.nonhazard)

# S'assigna un color a cada número de cores
cores.d.nonhazard <- as.data.frame(cores.nonhazard) %>% arrange(cores.nonhazard)
valor.previ <- 0
n <- length(unique(cores.d.nonhazard$cores))
color <- colorRampPalette(brewer.pal(8, "Spectral"))(n)
color.tikz <- sprintf("\\color[HTML]{%s}\\bullet", str_sub(color, start = -6))
color.actual <- 0

for(i in 1:nrow(cores.d.nonhazard)){
  if(cores.d.nonhazard$cores[i] != valor.previ){
    color.actual <- color.actual + 1
  }
  cores.d.nonhazard$color[i] <- color[color.actual]
  cores.d.nonhazard$color.tikz[i] <- color.tikz[color.actual]
  valor.previ <- cores.d.nonhazard$cores[i]
}

png(sprintf("../LaTeX/figures/plot-%s-%s-%s-%s.png", "coreness", "importacions", "nonhazard", "2018"), width = 1000, height = 1000, res = 75)
sna::gplot.target(g.nonhazard, cores.d.nonhazard$cores,
                  circ.col = "skyblue", 
                  circ.lwd = 3,
                  circ.lab = FALSE, 
                  usearrows = FALSE, 
                  vertex.col = cores.d.nonhazard$color,
                  edge.col = "darkgray") 

legend("right", legend = unique(cores.d.nonhazard$cores), pch = 19, col = unique(cores.d.nonhazard$color),
       inset = 0, bty = "n", cex = 1.5, title = "k-cores")
dev.off()

print(xtable(cores.d.nonhazard %>% rownames_to_column() %>% select(rowname, cores.nonhazard, color.tikz) %>% rename("País" = "rowname", "$k$-cores" = "cores.nonhazard", "Color" = "color.tikz"),
             digits = 0), 
      sanitize.colnames.function = bold,
      booktabs = TRUE,
      floating = TRUE,
      file = sprintf("../LaTeX/tables/taula-%s-%s-%s-%s.tex", "coreness", "importacions", "nonhazard", "2018"))

# Díades
dyad_census(g.import.hazard.simple)
dyad_census(g.import.nonhazard.simple)

# Densitat
edge_density(g.import.hazard.simple)
edge_density(g.import.nonhazard.simple)

# Transitivitat/coeficient d'agrupació (clustering)
transitivity(g.import.hazard)
transitivity(g.import.nonhazard)

# Reciprocitat
reciprocity(g.import.hazard.simple, mode = "default")
reciprocity(g.import.hazard.simple, mode = "ratio")

reciprocity(g.import.nonhazard.simple, mode = "default")
reciprocity(g.import.nonhazard.simple, mode = "ratio")

# Clustering
g.import.hazard.un <- graph_from_data_frame(d = dades.hazard$vis.links, vertices = dades.hazard$vis.nodes, directed = FALSE)
g.import.hazard.un.simple <- simplify(g.import.hazard.un, remove.multiple = TRUE, remove.loops = TRUE)

kc <- cluster_fast_greedy(g.import.hazard.un.simple)
length(kc)
sizes(kc)
membership(kc)

memb.table <- data.frame(membership = as.matrix(membership(kc))) %>% 
  arrange(membership) %>% 
  rownames_to_column()

data.hazard <- left_join(x = memb.table, y = degree.table.hazard %>% select(-degree.in, -degree.out)) %>% 
  left_join(y = cores.d.hazard %>% rownames_to_column() %>% select(rowname, cores.hazard))

data.table.hazard <- data.hazard %>% 
  group_by(membership) %>%
  summarise(sum.degrees = sum(degree),
            sum.cores = sum(cores.hazard, na.rm = TRUE))

png(sprintf("../LaTeX/figures/plot-%s-%s-%s-%s.png", "clustering", "importacions", "hazard", "2018"), width = 2000, height = 2000, res = 100)
plot(kc, g.import.hazard.un.simple)
dev.off()


##

g.import.nonhazard.un <- graph_from_data_frame(d = dades.nonhazard$vis.links, vertices = dades.nonhazard$vis.nodes, directed = FALSE)
g.import.nonhazard.un.simple <- simplify(g.import.nonhazard.un, remove.multiple = TRUE, remove.loops = TRUE)

kc <- cluster_fast_greedy(g.import.nonhazard.un.simple)
length(kc)
sizes(kc)
membership(kc)

memb.table <- data.frame(membership = as.matrix(membership(kc))) %>% 
  arrange(membership) %>% 
  rownames_to_column()

data.nonhazard <- left_join(x = memb.table, y = degree.table.nonhazard %>% select(-degree.in, -degree.out)) %>% 
  left_join(y = cores.d.nonhazard %>% rownames_to_column() %>% select(rowname, cores.nonhazard))

data.table.nonhazard <- data.nonhazard %>% 
  group_by(membership) %>%
  summarise(sum.degrees = sum(degree),
            sum.cores = sum(cores.nonhazard))

png(sprintf("../LaTeX/figures/plot-%s-%s-%s-%s.png", "clustering", "importacions", "nonhazard", "2018"), width = 2000, height = 2000, res = 100)
plot(kc, g.import.nonhazard.un.simple)
dev.off()


# Assortativitat
assortativity_degree(g.import.hazard.simple)
assortativity_degree(g.import.nonhazard.simple)

