#library -----------------------------------------------------------------

library(igraph)
library(dplyr)
`%ni%` <- Negate(`%in%`)
# data --------------------------------------------------------------------


# Carregar o dataframe
schizophrenia_sympt <- read.csv2("/home/and/github/tryr/Precision_analytics/schizophrenia_symp.csv", header = TRUE)


interm = schizophrenia_sympt$Intermediate %>% unique
gTS = schizophrenia_sympt %>%
  select(Disorder, Symptom) %>%
  filter(Symptom %ni% interm) %>%
  graph_from_data_frame(directed = F)

# Remover loops e arestas múltiplas
gTS <- simplify(gTS, remove.multiple = TRUE, remove.loops = TRUE)

# Cores dos nós
V(gTS)$color <- ifelse(V(gTS)$name %in% schizophrenia_sympt$Disorder, "forestgreen","darkorange")

plot(gTS,
     layout = layout_with_dh(gTS, maxiter = 100, fineiter = 100),  # Usar layout dh
     vertex.label = V(gTS)$name,  # Adicionar rótulos
     vertex.label.dist = 1,  # Distância dos rótulos
     vertex.label.cex = 0.6,  # Tamanho dos rótulos
     vertex.size = 3,  # Tamanho dos nós
     edge.arrow.size = 1,  # Tamanho das setas
     #main = "Schizophrenia Network",  # Título do grafo
     vertex.label.family = "sans",  # Fonte dos rótulos
     vertex.label.color = "black",  # Cor dos rótulos
     asp=.3)  # Aspect ratio

# Extraindo os sintomas únicos
sintomas <- unique(schizophrenia_sympt$Symptom)

# Calculando as centralidades
centralidades <- data.frame(V(gTS)$name, 
                            degree = centr_degree(gTS, mode = "in", normalized = TRUE)$res, 
                            closeness = centr_clo(gTS, mode = "all", normalized = TRUE)$res, 
                            eigen = centr_eigen(gTS, directed = TRUE, normalized = TRUE)$vector, 
                            betweenness = centr_betw(gTS, directed = TRUE, normalized = TRUE)$res)

colnames(centralidades)[1] <- "names" 

centralidades_sympt <- centralidades[centralidades$name %in% sintomas, ]
