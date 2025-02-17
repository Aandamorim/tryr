# library

library(igraph)
library(dplyr)
library(tidyr)
library(pheatmap)

`%ni%` <- Negate(`%in%`)

# df
schizophrenia_sympt <- read.csv2("/home/and/github/tryr/Precision_analytics/schizophrenia_symp.csv", header = TRUE)

# criando o grafo
interm = schizophrenia_sympt$Intermediate %>% unique
gTS = schizophrenia_sympt %>%
  select(Disorder, Symptom) %>%
  filter(Symptom %ni% interm) %>%
  graph_from_data_frame(directed = F) %>%  
  simplify(remove.multiple = TRUE, remove.loops = TRUE)


# cores
V(gTS)$color <- ifelse(V(gTS)$name %in% schizophrenia_sympt$Disorder, "forestgreen","darkorange")

plot(gTS,
     layout = layout_with_dh(gTS, maxiter = 100, fineiter = 100),
     vertex.label = V(gTS)$name,
     vertex.label.dist = 1,
     vertex.label.cex = 0.6,
     vertex.size = 3,
     edge.arrow.size = 1,
     vertex.label.family = "sans",
     vertex.label.color = "black",
     asp=.3)  # Aspect ratio

# centralidades
centralidades <- data.frame(V(gTS)$name, 
                            degree = centr_degree(gTS, mode = "in", normalized = TRUE)$res, 
                            closeness = centr_clo(gTS, mode = "all", normalized = TRUE)$res, 
                            eigen = centr_eigen(gTS, directed = TRUE, normalized = TRUE)$vector, 
                            betweenness = centr_betw(gTS, directed = TRUE, normalized = TRUE)$res)

# centralidade dos sintomas
centralidades_sympt <- centralidades %>% 
  filter(centralidades$V.gTS..name %in% unique(schizophrenia_sympt$Symptom))

# jaccard index


transtorno_sintomas <- schizophrenia_sympt %>%
  select(Disorder, Symptom) %>%
  unique()

# Função para calcular o índice de Jaccard entre dois conjuntos
calcular_jaccard <- function(set1, set2) {
  intersecao <- length(intersect(set1, set2))
  uniao <- length(union(set1, set2))
  return(ifelse(uniao == 0, 0, intersecao / uniao))
}

transtornos <- unique(transtorno_sintomas$Disorder)
n <- length(transtornos)
transtornos[grep("Catatonia Associated", transtornos)] <- "Catatonia Associated with Another Mental Disorder"


matriz_jaccard <- matrix(0, nrow = n, ncol = n)
rownames(matriz_jaccard) <- transtornos
colnames(matriz_jaccard) <- transtornos

for (i in 1:n) {
  for (j in 1:n) {
    if (i != j) {
      sintomas_i <- transtorno_sintomas$Symptom[transtorno_sintomas$Disorder == transtornos[i]]
      sintomas_j <- transtorno_sintomas$Symptom[transtorno_sintomas$Disorder == transtornos[j]]
      matriz_jaccard[i, j] <- calcular_jaccard(sintomas_i, sintomas_j)
    }
  }
}

print(matriz_jaccard[1:5, 1:5])

pheatmap(matriz_jaccard, 
         main = "Índice de Jaccard entre Transtornos",
         color = colorRampPalette(c("white", "red"))(100),
         angle_col = "45",
         cluster_cols = F,
         cluster_rows = F)

# clareza do jaccard [wip]
diag(matriz_jaccard) <- -2

matriz_jaccard[]

pheatmap(matriz_jaccard, 
         main = "Índice de Jaccard entre Transtornos",
         color = c("white",colorRampPalette(c("white", "blue"))(100)),
         breaks = c(-2, seq(-1, 1, length.out = 100)),
         angle_col = "45",
         cluster_cols = F,
         cluster_rows = F,
         legend_breaks = seq(-1, 1, length.out = 3),
         legend_labels = c("-1", "0", "1"),
         border_color = "grey32"
)

# Distância entre transtornos

matriz_distancia <- distances(gTS, v = transtornos, to = transtornos, mode = 'all')

matriz_distancia[is.infinite(matriz_distancia)] <- -1

max_dist <- max(matriz_distancia[matriz_distancia != -1])

# teste da escala matriz_distancia[2,1] <- 8

matriz_numeros <- matriz_distancia

matriz_numeros[matriz_numeros == -1] <- ""



pheatmap(matriz_distancia, 
         main = "Distância entre Transtornos",
         color = cores <- c("grey", colorRampPalette(c("white", "blue"))(100)),
         breaks = breaks <- c(-1.5, -0.5, seq(0, max_dist, length.out = 100)),
         angle_col = "45",
         cluster_cols = F,
         cluster_rows = F,
         display_numbers = matriz_numeros,
         legend = F)
