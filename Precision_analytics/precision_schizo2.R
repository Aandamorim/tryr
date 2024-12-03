# library -----------------------------------------------------------------

library(igraph)
library(tidyr)
library(ggplot2)
library(dplyr)



# melhor versão por enquanto ------------------------------------------------------------------
# Carregar os dados
schizophrenia_sympt <- read.csv2("/home/and/github/tryr/Precision_analytics/schizophrenia_symp.csv", header = TRUE)
View(schizophrenia_sympt)
# Remover espaços em branco e normalizar os nomes
schizophrenia_sympt$Disorder <- trimws(schizophrenia_sympt$Disorder)
schizophrenia_sympt$Symptom <- trimws(schizophrenia_sympt$Symptom)
schizophrenia_sympt$Intermediate <- trimws(schizophrenia_sympt$Intermediate)

# Criar um dataframe de arestas
#edges <- data.frame(from = schizophrenia_sympt$Disorder, to = schizophrenia_sympt$Symptom)

edges <- data.frame(from = c(schizophrenia_sympt$Disorder, schizophrenia_sympt$Intermediate),
                    to = c(schizophrenia_sympt$Intermediate, schizophrenia_sympt$Symptom))

# Criar um dataframe de vértices
#vertices <- unique(c(schizophrenia_sympt$Disorder, schizophrenia_sympt$Symptom))

vertices <- unique(c(schizophrenia_sympt$Disorder,
                     schizophrenia_sympt$Symptom,
                     schizophrenia_sympt$Intermediate))

# Criar um dataframe de vértices
#vertices_df <- data.frame(name = vertices)


#grafo <- graph_from_data_frame(d = edges, vertices = vertices_df, directed = TRUE)

# Definir os tipos de vértices
g <- graph_from_data_frame(edges, directed = FALSE)
# Remover loops e arestas múltiplas
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

V(g)$color <- ifelse(V(g)$name %in% schizophrenia_sympt$Disorder, "forestgreen", 
                     ifelse(V(g)$name %in% schizophrenia_sympt$Intermediate, "darkorchid4", 
                            "darkorange"))


# Verificar os vértices do grafo
print(V(grafo)$name)  # Imprimir os nomes dos vértices

# Definir os tipos de vértices
V(grafo)$type <- ifelse(V(grafo)$name %in% schizophrenia_sympt$Disorder, TRUE, FALSE)

V(grafo)$type <- ifelse(V(grafo)$name %in% schizophrenia_sympt$Intermediate, TRUE, FALSE)

# Verificar o vetor de tipos
print(V(grafo)$name)  # Imprimir o vetor de tipos
table(V(grafo)$type)  # Contar quantos TRUE e FALSE

V(g)$name

ol <- layout_as_bipartite(grafo)


ol[9,1] <- 70
# Deslocar os rótulos de "Disorders" para a esquerda e de "Symptoms" para a direita
#ol[V(grafo)$type, 1] <- ol[V(grafo)$type, 1] - 0.5  # Deslocar Disorders para a esquerda
#ol[!V(grafo)$type, 1] <- ol[!V(grafo)$type, 1] + 0.5  # Deslocar Symptoms para a direita



plot(g,
     layout = layout_with_dh(g, maxiter = 100, fineiter = 100),  # Usar layout dh
     #layout = ol[,2:1],  # Usar layout bipartido
     #layout = layout_with_graphopt(g, charge = 0.0001, mass = 30),
     #vertex.label.degree = pi/2, # Orientação dos rótulos
     vertex.label = V(g)$name,  # Adicionar rótulos
     vertex.label.dist = 1,  # Distância dos rótulos
     vertex.label.cex = 0.6,  # Tamanho dos rótulos
     vertex.size = 3,  # Tamanho dos nós
     edge.arrow.size = 1,  # Tamanho das setas
     main = "Schizophrenia Network",  # Título do grafo
     vertex.label.family = "sans",  # Fonte dos rótulos
     vertex.label.color = "black",  # Cor dos rótulos
     #vertex.color = ifelse(V(grafo)$type == TRUE, "orange", "skyblue"), # Cor dos nós
     asp=.5)  # Aspect ratio
legend("bottomleft", legend = c("Disorders", "Symptoms", "Intermediate"),
       fill = c("forestgreen", "darkorchid4", "darkorange"),
       bty = "n", inset = .1, cex = 0.6)
which.max(degree(g))

# adicionar intermediarios check
# gephi  scytoscape
# visnetwork
# https://www.r-graph-gallery.com/247-igraph-plot-network-with-ggraph-and-igraph.html

# teste função detecção

func_detec <- function(df, Col1, Col2){
  suppressWarnings({
    ind1 <- df %>% 
      select(all_of(c(Col1 = Col1, Col2 = Col2))) %>% 
      mutate(Ind = case_when(Col1 %in% Col2 ~ T, .default = F)) %>%
      select(Ind) %>% 
      unlist(use.names = F)
  ind1 <- which(ind1)
  ind2 <- numeric(length(ind1))
  for(i in 1:lenght(ind1)){
    ind2[i] <- which(df[, Col2] == 
                       unlist(df[ind1[i], Col1], use.name = F))
    }
  })
  return(list(ind1, ind2))
}

func_detec(schizophrenia_sympt, "Intermediate", "Symptom")

