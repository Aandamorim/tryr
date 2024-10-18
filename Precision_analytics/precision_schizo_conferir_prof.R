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
     vertex.size = 3,  # Tamanho dos nós
     edge.arrow.size = 1,  # Tamanho das setas
     main = "Schizophrenia Network",  # Título do grafo
     vertex.label.family = "sans",  # Fonte dos rótulos
     vertex.label.color = "black",  # Cor dos rótulos
     asp=.5)  # Aspect ratio
