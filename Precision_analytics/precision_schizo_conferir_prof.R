#library -----------------------------------------------------------------

library(igraph)
library(dplyr)

# data --------------------------------------------------------------------


# Carregar o dataframe
schizophrenia_sympt <- read.csv2("/home/and/github/tryr/Precision_analytics/schizophrenia_symp.csv", header = TRUE)

# Identificar todos os intermediários que também são sintomas
intermediates_as_symptoms <- schizophrenia_sympt %>%
  filter(Intermediate %in% Symptom) %>%
  pull(Intermediate) %>%
  unique()

# Filtrar para remover arestas duplas
filtered_schizophrenia_sympt <- schizophrenia_sympt %>%
  filter(!(Intermediate %in% intermediates_as_symptoms & 
             Symptom %in% intermediates_as_symptoms))

# Visualizando o resultado filtrado
View(filtered_schizophrenia_sympt)


# definindo os vértices

edges <- data.frame(from = c(filtered_schizophrenia_sympt$Disorder, filtered_schizophrenia_sympt$Intermediate),
                    to = c(filtered_schizophrenia_sympt$Intermediate, filtered_schizophrenia_sympt$Symptom))

# Definir os tipos de vértices
g <- graph_from_data_frame(edges, directed = FALSE)

# Remover loops e arestas múltiplas
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

# Cores dos nós
V(g)$color <- ifelse(V(g)$name %in% filtered_schizophrenia_sympt$Disorder, "forestgreen", 
                     ifelse(V(g)$name %in% filtered_schizophrenia_sympt$Intermediate, "darkorchid4", 
                            "darkorange"))
plot(g,
     layout = layout_with_dh(g, maxiter = 100, fineiter = 100),  # Usar layout dh
     vertex.label = V(g)$name,  # Adicionar rótulos
     vertex.label.dist = 1,  # Distância dos rótulos
     vertex.label.cex = 0.6,  # Tamanho dos rótulos
     vertex.size = deg*1.1,  # Tamanho dos nós
     edge.arrow.size = 1,  # Tamanho das setas
     main = "Schizophrenia Network",  # Título do grafo
     vertex.label.family = "sans",  # Fonte dos rótulos
     vertex.label.color = "black",  # Cor dos rótulos
     asp=.5)  # Aspect ratio

deg <- degree(g, mode = "out")

# medidas de centralidade
centralidades = data.frame(V(g)$name, 
           degree= 
centr_degree(g, mode = "in", normalized = TRUE)$res, 
closeness = 
centr_clo(g, mode = "all", normalized = TRUE)$res, 
eigen = 
centr_eigen(g, directed = TRUE, normalized = TRUE)$vector, 
betweenness =
centr_betw(g, directed = TRUE, normalized = TRUE)$res)


# Hubs e autoridades

hs <- hub_score(g, weights = NA)$vector
as <- authority_score(g, weights = NA)$vector

par(mfrow=c(1,1))
plot(g,vertex.size=hs*10,vertex.label=NA,vertex.color=hs,edge.color="gray",edge.arrow.size=0.5,main="Hubs")
plot(g,vertex.size=as*10,vertex.label=NA,vertex.color=as,edge.color="gray",edge.arrow.size=0.5,main="Authorities")

# Comunidades

par(mfrow=c(2,2))

com <- cluster_edge_betweenness(g)
com
plot(com, g, vertex.size=5, vertex.label=V(g)$name, edge.color="gray", main="Edge Betweenness Clustering")

clp <- cluster_label_prop(g)
plot(clp, g, vertex.size=5, vertex.label=V(g)$name, edge.color="gray", main="Label Propagation Clustering")

clm <- cluster_walktrap(g)
plot(clm, g, vertex.size=5, vertex.label=V(g)$name, edge.color="gray", main="Walktrap Clustering")

cfg <- cluster_fast_greedy(g)
plot(cfg, g,  vertex.size=5, vertex.label=V(g)$name, edge.color="gray", main="Fast Greedy Clustering")

## teste para melhor visualização

par(mfrow=c(2,2), mar=c(0,0,2,2))

# Define colors for clusters
colors <- rainbow(length(unique(com$membership)))

# Edge Betweenness Clustering
plot(com, g, vertex.size=6, vertex.label=V(g)$name,
     vertex.label.cex=0.6, vertex.color=colors[com$membership],
     edge.color="gray", main="Edge Betweenness Clustering")

# Label Propagation Clustering
plot(clp, g, vertex.size=6, vertex.label=V(g)$name,
     vertex.label.cex=0.6, vertex.color=colors[clp$membership],
     edge.color="gray", main="Label Propagation Clustering")

# Walktrap Clustering
plot(clm, g, vertex.size=6, vertex.label=V(g)$name,
     vertex.label.cex=0.6, vertex.color=colors[clm$membership],
     edge.color="gray", main="Walktrap Clustering")

# Fast Greedy Clustering
plot(cfg, g, ver{tex.size=6, vertex.label=V(g)$name,
     vertex.label.cex=0.6, vertex.color=colors[cfg$membership],
     edge.color="gray", main="Fast Greedy Clustering")



