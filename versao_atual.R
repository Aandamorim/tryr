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

# heatmap jaccard 
diag(matriz_jaccard) <- -2

## gambiarra pra mostrar os números sem o -2 na diagonal
matriz_num_jac <- matriz_jaccard
matriz_num_jac <-round(matriz_num_jac, 3)
matriz_num_jac[matriz_num_jac == -2] <- ""
matriz_num_jac[matriz_num_jac == 0] <- ""


#####
# 
# teste_superior <- cor(matriz_jaccard)
# 
# teste_superior[lower.tri(teste_superior)] <- NA
# 
# mat_tes_num <- teste_superior
# mat_tes_num <- round(mat_tes_num, 3)
# mat_tes_num[mat_tes_num < 0] <- ""
# mat_tes_num[is.na(mat_tes_num)] <- ""
# 
# pheatmap(teste_superior, 
#          main = "Índice de Jaccard entre Transtornos",
#          color = colorRampPalette(c("white", "blue"))(100),
#          breaks = seq(0, 1, length.out = 100),
#          angle_col = "45",
#          cluster_cols = F,
#          cluster_rows = F,
#          legend_breaks = seq(0, 1, length.out = 3),
#          legend_labels = c( "0","", "1"),
#          display_numbers = mat_tes_num,
#          border_color = "white",
#          na_col = "white"
# )
#####

pheatmap(matriz_jaccard, 
         main = "Índice de Jaccard entre Transtornos",
         color = c("white",colorRampPalette(c("white", "red"))(100)),
         breaks = c(-1, seq(0, 1, length.out = 80)),
         angle_col = "45",
         cluster_cols = F,
         cluster_rows = F,
         legend_breaks = seq(0, 1, length.out = 3),
         legend_labels = c( "0","", "1"),
         #legend = F,
         display_numbers = matriz_num_jac,
         border_color = "grey80",
         fontsize = 8,
         fontsize_row = 8,
         fontsize_col = 8,
         cellwidth = 40,
         cellheight = 40
)

# Separation transtornos
transtornos <- unique(transtorno_sintomas$Disorder)
matriz_distancia <- distances(gTS, v = transtornos, to = transtornos, mode = 'all')

matriz_distancia[is.infinite(matriz_distancia)] <- -1

max_dist <- max(matriz_distancia[matriz_distancia != -1])

matriz_numeros <- matriz_distancia

matriz_numeros[matriz_numeros == -1] <- ""



pheatmap(matriz_distancia, 
         main = "Distância entre Transtornos",
         color = cores <- c("white", colorRampPalette(c("grey", "red"))(100)),
         breaks = breaks <- c(-1.5, -0.5, seq(0, max_dist, length.out = 50)),
         angle_col = "45",
         cluster_cols = F,
         cluster_rows = F,
         display_numbers = matriz_numeros,
         legend = F,
         fontsize = 8,
         fontsize_row = 8,
         fontsize_col = 8,
         cellwidth = 40,
         cellheight = 40
         )

# # Similarity transtornos
# 
# 
# matriz_similaridade <- similarity(gTS, method = "jaccard", 
#                                   vids = transtornos,
#                                   mode = "all")
# 
# matriz_dist_sim <- 1 - matriz_similaridade
# 
# max_dist_sim <- max(matriz_dist_sim)
# rownames(matriz_dist_sim) <- transtornos
# colnames(matriz_dist_sim) <- transtornos
# 
# pheatmap(matriz_dist_sim, 
#          main = "Similaridade entre Transtornos",
#          color = colorRampPalette(c("white", "red"))(100),
#          breaks = seq(0, max_dist_sim, length.out = 100),
#          angle_col = "45",
#          cluster_cols = F,
#          cluster_rows = F,
#          display_numbers = T,
#          legend = T)

## teste similaridade

# Função para calcular a proximidade entre dois transtornos
calcular_proximidade <- function(transtorno1, transtorno2, grafo) {
  sintomas1 <- neighbors(grafo, transtorno1)
  sintomas2 <- neighbors(grafo, transtorno2)
  
  sintomas_compartilhados <- length(intersect(sintomas1, sintomas2))
  total_sintomas_unicos <- length(union(sintomas1, sintomas2))
  
  return(sintomas_compartilhados / total_sintomas_unicos)
}

# Criar matriz de proximidade
matriz_proximidade <- matrix(0, nrow = n, ncol = n)

for (i in 1:n) {
  for (j in 1:n) {
    matriz_proximidade[i, j] <- calcular_proximidade(transtornos[i], transtornos[j], gTS)
  }
}

# Plotar o heatmap
pheatmap(matriz_proximidade, 
         main = "Proximidade entre Transtornos (Baseada em Sintomas Compartilhados)",
         color = colorRampPalette(c("white", "red"))(100),
         breaks = seq(0, 1, length.out = 100),
         angle_col = "45",
         cluster_cols = F,
         cluster_rows = F,
         display_numbers = T,
         legend = T)
