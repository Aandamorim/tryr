# library -----------------------------------------------------------------

library(igraph)
library(tidyr)
library(ggplot2)
library(dplyr)


# carregando dados --------------------------------------------------------------------

schizophrenia_sympt <- read.csv2("/home/and/github/tryr/Precision_analytics/schizophrenia_symp.csv")
  
View(schizophrenia_sympt)


# elementos únicos --------------------------------------------------------

disorders_unicos <- unique(schizophrenia_sympt$Disorder)
symptoms_unicos <- unique(schizophrenia_sympt$Symptom)


# ajuste e criação matriz quadrada --------------------------------------------------

max_elementos <- max(length(disorders_unicos), length(symptoms_unicos))
matriz_adj <- matrix(0, nrow = max_elementos, ncol = max_elementos,
                            dimnames = list(1:max_elementos, 1:max_elementos))


# popular matriz -------------------------------------------------------

for (i in 1:nrow(schizophrenia_sympt)) {
  disorder <- match(schizophrenia_sympt[i, "Disorder"], disorders_unicos)
  symptom <- match(schizophrenia_sympt[i, "Symptom"], symptoms_unicos)
  
  # Preenche a matriz com 1 onde há correspondência
  matriz_adj[disorder, symptom] <- 1
}


# grafo -------------------------------------------------------------------

grafo <- graph_from_adjacency_matrix(matriz_adj, 
                                     mode = "directed", 
                                     diag = FALSE,
                                     add.rownames = disorders_unicos,
                                     add.colnames = symptoms_unicos)


# plot --------------------------------------------------------------------

plot(grafo, layout = layout_with_fr,  # Usar layout Fruchterman-Reingold para espaçamento
     vertex.label = V(grafo)$name,  # Adicionar rótulos
     vertex.label.cex = 0.8,  # Tamanho dos rótulos
     vertex.size = 5,  # Tamanho dos nós
     edge.arrow.size = 0.5,  # Tamanho das setas
     main = "Grafo de Relação entre Disorders e Symptoms")





# versão direto do df -----------------------------------------------------

# Passo 1: Carregar os dados
schizophrenia_sympt <- read.csv2("/home/and/github/tryr/Precision_analytics/schizophrenia_symp.csv")

# Passo 2: Criar a matriz de adjacência quadrada
# Obter os nomes únicos de Disorders e Symptoms
disorders_unicos <- unique(schizophrenia_sympt$Disorder)
symptoms_unicos <- unique(schizophrenia_sympt$Symptom)

# Criar um vetor que combina Disorders e Symptoms
todos_nos <- c(disorders_unicos, symptoms_unicos)

# Inicializar a matriz de adjacência quadrada
matriz_adjacencia <- matrix(0, nrow = length(todos_nos), ncol = length(todos_nos),
                            dimnames = list(todos_nos, todos_nos))

# Preencher a matriz com 1 onde há correspondência
for (i in 1:nrow(schizophrenia_sympt)) {
  disorder <- schizophrenia_sympt[i, "Disorder"]
  symptom <- schizophrenia_sympt[i, "Symptom"]
  
  # Encontrar os índices correspondentes
  disorder_index <- match(disorder, todos_nos)
  symptom_index <- match(symptom, todos_nos)
  
  # Preenche a matriz com 1 onde há correspondência
  matriz_adjacencia[disorder_index, symptom_index] <- 1
}

# Criar o grafo a partir da matriz de adjacência
grafo <- graph_from_adjacency_matrix(matriz_adjacencia, mode = "directed", diag = FALSE)

# Definir os tipos de vértices
V(grafo)$type <- c(rep("Disorder", length(disorders_unicos)), rep("Symptom", length(symptoms_unicos)))

# Passo 4: Ajustar a visualização do grafo
plot(grafo,
     layout = layout_as_bipartite,  # Usar layout bipartido
     vertex.label = V(grafo)$name,  # Adicionar rótulos
     vertex.label.cex = 0.7,  # Tamanho dos rótulos
     vertex.size = 5,  # Tamanho dos nós
     edge.arrow.size = 0.5,  # Tamanho das setas
     main = "Grafo Bipartido de Disorders e Symptoms",  # Título do grafo
     vertex.label.family = "sans",  # Fonte dos rótulos
     vertex.label.color = "black")  # Cor dos rótulos


# 4 tentativa -------------------------------------------------------------

# Passo 1: Carregar os dados
schizophrenia_sympt <- read.csv2("/home/and/github/tryr/Precision_analytics/schizophrenia_symp.csv", header = TRUE)

# Passo 2: Remover espaços em branco e normalizar os nomes
schizophrenia_sympt$Disorder <- trimws(schizophrenia_sympt$Disorder)
schizophrenia_sympt$Symptom <- trimws(schizophrenia_sympt$Symptom)

# Passo 3: Verificar se existem valores NA nas colunas "Disorder" e "Symptom"
if (any(is.na(schizophrenia_sympt$Disorder))) {
  stop("Existem valores NA na coluna Disorder.")
}
if (any(is.na(schizophrenia_sympt$Symptom))) {
  stop("Existem valores NA na coluna Symptom.")
}

# Passo 4: Criar a matriz de adjacência quadrada
# Obter os nomes únicos de Disorders e Symptoms
disorders_unicos <- unique(schizophrenia_sympt$Disorder)
symptoms_unicos <- unique(schizophrenia_sympt$Symptom)

# Criar um vetor que combina Disorders e Symptoms
todos_nos <- c(disorders_unicos, symptoms_unicos)

# Inicializar a matriz de adjacência quadrada
matriz_adjacencia <- matrix(0, nrow = length(todos_nos), ncol = length(todos_nos),
                            dimnames = list(todos_nos, todos_nos))

# Preencher a matriz com 1 onde há correspondência
for (i in 1:nrow(schizophrenia_sympt)) {
  disorder <- schizophrenia_sympt[i, "Disorder"]
  symptom <- schizophrenia_sympt[i, "Symptom"]
  
  # Encontrar os índices correspondentes
  disorder_index <- match(disorder, todos_nos)
  symptom_index <- match(symptom, todos_nos)
  
  # Verificar se os índices foram encontrados
  if (is.na(disorder_index) || is.na(symptom_index)) {
    cat("Índices não encontrados para Disorder:", disorder, "ou Symptom:", symptom, "\n")
  } else {
    # Preenche a matriz com 1 onde há correspondência
    matriz_adjacencia[disorder_index, symptom_index] <- 1
  }
}

# Passo 5: Criar o grafo a partir da matriz de adjacência
library(igraph)

# Criar o grafo a partir da matriz de adjacência
grafo <- graph_from_adjacency_matrix(matriz_adjacencia, mode = "directed", diag = FALSE)

# Passo 6: Verificar a estrutura do grafo
print(grafo)

# Passo 7: Definir os tipos de vértices
# Atribuir tipos de vértices: "Disorder" para os primeiros e "Symptom" para os últimos
V(grafo)$type <- c(rep("Disorder", length(disorders_unicos)), rep("Symptom", length(symptoms_unicos)))

# Verificar se há NAs nos tipos de vértices
if (any(is.na(V(grafo)$type))) {
  stop("Existem valores NA nos tipos de vértices.")
}

# Passo 8: Ajustar a visualização do grafo
plot(grafo,
     layout = layout_as_bipartite,  # Usar layout bipartido
     vertex.label = V(grafo)$name,  # Adicionar rótulos
     vertex.label.cex = 0.7,  # Tamanho dos rótulos
     vertex.size = 5,  # Tamanho dos nós
     edge.arrow.size = 0.5,  # Tamanho das setas
     main = "Grafo Bipartido de Disorders e Symptoms",  # Título do grafo
     vertex.label.family = "sans",  # Fonte dos rótulos
     vertex.label.color = "black")  # Cor dos rótulos



# df again ----------------------------------------------------------------

# Passo 1: Carregar os dados
schizophrenia_sympt <- read.csv2("/home/and/github/tryr/Precision_analytics/schizophrenia_symp.csv", header = TRUE)

# Passo 2: Remover espaços em branco e normalizar os nomes
schizophrenia_sympt$Disorder <- trimws(schizophrenia_sympt$Disorder)
schizophrenia_sympt$Symptom <- trimws(schizophrenia_sympt$Symptom)

# Passo 3: Criar um dataframe de arestas
edges <- data.frame(from = schizophrenia_sympt$Disorder, to = schizophrenia_sympt$Symptom)

# Passo 4: Criar um dataframe de vértices
vertices <- unique(c(schizophrenia_sympt$Disorder, schizophrenia_sympt$Symptom))

# Criar um dataframe de vértices
vertices_df <- data.frame(name = vertices)

# Passo 5: Criar o grafo a partir dos dataframes de arestas e vértices
library(igraph)

grafo <- graph_from_data_frame(d = edges, vertices = vertices_df, directed = TRUE)

# Passo 6: Definir os tipos de vértices
# Atribuir tipos de vértices: "Disorder" para os primeiros e "Symptom" para os últimos
V(grafo)$type <- ifelse(V(grafo)$name %in% schizophrenia_sympt$Disorder, "Disorder", "Symptom")

# Passo 7: Ajustar a visualização do grafo
plot(grafo,
     layout = layout_with_dh(grafo, maxiter = 100, fineiter = 100),  # Usar layout Fruchterman-Reingold
     #layout = layout_as_bipartite(grafo, types = ),  # Usar layout bipartido
     #layout = layout_with_graphopt(grafo, charge = 0.03, mass = 10),
     vertex.label = V(grafo)$name,  # Adicionar rótulos
     vertex.label.cex = 0.7,  # Tamanho dos rótulos
     vertex.size = 7,  # Tamanho dos nós
     edge.arrow.size = 0.1,  # Tamanho das setas
     main = "Grafo de Disorders e Symptoms",  # Título do grafo
     vertex.label.family = "sans",  # Fonte dos rótulos
     vertex.label.color = "black",  # Cor dos rótulos
     vertex.color = ifelse(V(grafo)$type == "Disorder", "orange", "skyblue"))  # Cor dos nós



# melhor versão por enquanto ------------------------------------------------------------------
# Carregar os dados
schizophrenia_sympt <- read.csv2("/home/and/github/tryr/Precision_analytics/schizophrenia_symp.csv", header = TRUE)
View(schizophrenia_sympt)
# Remover espaços em branco e normalizar os nomes
schizophrenia_sympt$Disorder <- trimws(schizophrenia_sympt$Disorder)
schizophrenia_sympt$Symptom <- trimws(schizophrenia_sympt$Symptom)
schizophrenia_sympt$Intermediate <- trimws(schizophrenia_sympt$Intermediate)

# Criar um dataframe de arestas
edges <- data.frame(from = schizophrenia_sympt$Disorder, to = schizophrenia_sympt$Symptom)

edges <- data.frame(from = c(schizophrenia_sympt$Disorder, schizophrenia_sympt$Intermediate),
                    to = c(schizophrenia_sympt$Intermediate, schizophrenia_sympt$Symptom))

# Criar um dataframe de vértices
vertices <- unique(c(schizophrenia_sympt$Disorder, schizophrenia_sympt$Symptom))

vertices <- unique(c(schizophrenia_sympt$Disorder,
                     schizophrenia_sympt$Symptom,
                     schizophrenia_sympt$Intermediate))

# Criar um dataframe de vértices
vertices_df <- data.frame(name = vertices)


grafo <- graph_from_data_frame(d = edges, vertices = vertices_df, directed = TRUE)

# Definir os tipos de vértices
g <- graph_from_data_frame(edges, directed = FALSE)
# Remover loops e arestas múltiplas
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

V(g)$color <- ifelse(V(g)$name %in% schizophrenia_sympt$Disorder, "lightblue", 
                     ifelse(V(g)$name %in% schizophrenia_sympt$Intermediate, "lightgreen", 
                            "lightcoral"))


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

# Aumentar a posição de todos os "Symptoms"
for (i in seq_along(ol[V(grafo)$type, 1])) {
  ol[V(grafo)$type, 1][i] <- ol[V(grafo)$type, 1][i] + (i * 0.5)  # Aumentar progressivamente
}

# Aumentar a posição de todos os "Symptoms"
for (i in seq_along(ol[!V(grafo)$type, 1])) {
  ol[!V(grafo)$type, 1][i] <- ol[!V(grafo)$type, 1][i] + (i * 0.5)  # Aumentar progressivamente
}

vertex_label_dist <- ifelse(V(g)$type, 1.5, 1.5)
plot(g,
     layout = layout_with_dh(g, maxiter = 100, fineiter = 100),  # Usar layout Fruchterman-Reingold
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

which.max(degree(g))

# adicionar intermediarios check
# gephi  scytoscape
# visnetwork
