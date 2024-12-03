library(igraph)
library(rgl)

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

# Definindo os vértices
edges <- data.frame(from = filtered_schizophrenia_sympt$Disorder,
                    to = filtered_schizophrenia_sympt$Symptom)

# Criar o grafo
g <- graph_from_data_frame(edges, directed = FALSE)

# Remover loops e arestas múltiplas
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

# Cálculo de clusters (por exemplo, usando Edge Betweenness)
com <- cluster_edge_betweenness(g)

# Criar dataframe de nós
nodes <- data.frame(id = V(g)$name, 
                    label = V(g)$name, 
                    group = as.factor(com$membership))  # Certifique-se de que seja um fator

# Criar dataframe de arestas
edges <- get.data.frame(g, what = "edges")

# Definindo cores para os grupos
colors <- rainbow(length(unique(com$membership)))

# Obter layout em 3D
layout_3d <- layout_with_fr(g)
layout_3d <- cbind(layout_3d, z = runif(nrow(layout_3d), min = -1, max = 1))

# Criar gráfico em 3D
plot3d(layout_3d[,1], layout_3d[,2], layout_3d[,3], type = "s", size = 5,
       col = colors[as.numeric(com$membership)], xlab = "X", ylab = "Y", zlab = "Z")

# Adicionar arestas
for (i in seq_len(nrow(edges))) {
  segments3d(c(layout_3d[which(V(g)$name == edges[i,1]),1],
               layout_3d[which(V(g)$name == edges[i,2]),1]),
             c(layout_3d[which(V(g)$name == edges[i,1]),2],
               layout_3d[which(V(g)$name == edges[i,2]),2]),
             c(layout_3d[which(V(g)$name == edges[i,1]),3],
               layout_3d[which(V(g)$name == edges[i,2]),3]), col="gray")
}

rglwidget()

# total

library(igraph)
library(rgl)

# Carregar o dataframe original
schizophrenia_sympt <- read.csv2("/home/and/github/tryr/Precision_analytics/schizophrenia_symp.csv", header = TRUE)

# Criar o dataframe de arestas
edges <- data.frame(from = schizophrenia_sympt$Disorder,
                    to = schizophrenia_sympt$Symptom)

# Criar o grafo
g <- graph_from_data_frame(edges, directed = FALSE)

# Remover loops e arestas múltiplas
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

# Cálculo de clusters (por exemplo, usando Edge Betweenness)
com <- cluster_edge_betweenness(g)

# Criar dataframe de nós
nodes <- data.frame(id = V(g)$name, 
                    label = V(g)$name, 
                    group = as.factor(com$membership))

# Definindo cores para os grupos
colors <- rainbow(length(unique(com$membership)))

# Obter layout em 3D
layout_3d <- layout_with_fr(g)
layout_3d <- cbind(layout_3d, z = runif(nrow(layout_3d), min = -1, max = 1))

# Criar gráfico em 3D
plot3d(layout_3d[,1], layout_3d[,2], layout_3d[,3], type = "s", size = 5,
       col = colors[as.numeric(com$membership)], xlab = "X", ylab = "Y", zlab = "Z")

# Adicionar arestas
for (i in seq_len(nrow(edges))) {
  segments3d(c(layout_3d[which(V(g)$name == edges[i,1]),1],
               layout_3d[which(V(g)$name == edges[i,2]),1]),
             c(layout_3d[which(V(g)$name == edges[i,1]),2],
               layout_3d[which(V(g)$name == edges[i,2]),2]),
             c(layout_3d[which(V(g)$name == edges[i,1]),3],
               layout_3d[which(V(g)$name == edges[i,2]),3]), col="gray")
}

# Adicionar nomes aos nós
text3d(layout_3d[,1], layout_3d[,2], layout_3d[,3], texts = V(g)$name, cex = 0.7)

######################### outro teste 3d

library(igraph)
library(rgl)

# Carregar o dataframe original
schizophrenia_sympt <- read.csv2("/home/and/github/tryr/Precision_analytics/schizophrenia_symp.csv", header = TRUE)

# Criar o dataframe de arestas
edges <- data.frame(from = schizophrenia_sympt$Disorder,
                    to = schizophrenia_sympt$Symptom)

# Criar o grafo
g <- graph_from_data_frame(edges, directed = FALSE)

# Remover loops e arestas múltiplas
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

# Cálculo de clusters (por exemplo, usando Edge Betweenness)
com <- cluster_edge_betweenness(g)

# Criar dataframe de nós
nodes <- data.frame(id = V(g)$name, 
                    label = V(g)$name)

# Definindo cores para os grupos
colors <- rep("gray", vcount(g))  # Inicializa todas as cores como cinza

# Atribuir cores específicas para cada tipo
disorders <- unique(schizophrenia_sympt$Disorder)
intermediates <- unique(schizophrenia_sympt$Intermediate)
symptoms <- unique(schizophrenia_sympt$Symptom)

# Atribuir cores específicas para cada tipo
colors[V(g)$name %in% disorders] <- "forestgreen"  # Disorders
colors[V(g)$name %in% intermediates] <- "darkorchid4"  # Intermediates
colors[V(g)$name %in% symptoms] <- "darkorange"  # Symptoms

# Obter layout em 3D com maior espaçamento
layout_3d <- layout_with_dh(g, niter = 500)  # Aumenta o número de iterações para melhor espaçamento
layout_3d <- cbind(layout_3d, z = runif(nrow(layout_3d), min = -2, max = 2))  # Aumenta a amplitude do eixo Z

# Criar gráfico em 3D
plot3d(layout_3d[,1], layout_3d[,2], layout_3d[,3], type = "s", size = 5,
       col = colors, xlab = "X", ylab = "Y", zlab = "Z")

# Adicionar arestas
for (i in seq_len(nrow(edges))) {
  segments3d(c(layout_3d[which(V(g)$name == edges[i,1]),1],
               layout_3d[which(V(g)$name == edges[i,2]),1]),
             c(layout_3d[which(V(g)$name == edges[i,1]),2],
               layout_3d[which(V(g)$name == edges[i,2]),2]),
             c(layout_3d[which(V(g)$name == edges[i,1]),3],
               layout_3d[which(V(g)$name == edges[i,2]),3]), col="gray")
}

# Adicionar nomes aos nós
text3d(layout_3d[,1], layout_3d[,2], layout_3d[,3], texts = V(g)$name, cex = 0.7)

rglwidget()


