library(dplyr)
library(visNetwork)

# Carregar o dataframe
schizophrenia <- read.csv2("/home/and/github/Disease_Symp_Complete.csv", sep = ',', header = TRUE)

# Identificar todos os intermediários que também são sintomas
intermediates_as_symptoms <- schizophrenia %>%
  filter(Intermediate %in% Symptom) %>%
  pull(Intermediate) %>%
  unique()

# Filtrar para remover arestas duplas
filtered_schizophrenia <- schizophrenia %>%
  filter(!(Intermediate %in% intermediates_as_symptoms & 
             Symptom %in% intermediates_as_symptoms))


# Criar um dataframe de nós
nodes <- data.frame(id = unique(c(filtered_schizophrenia$Disorder, 
                                  filtered_schizophrenia$Intermediate, 
                                  filtered_schizophrenia$Symptom)),
                    label = unique(c(filtered_schizophrenia$Disorder, 
                                     filtered_schizophrenia$Intermediate, 
                                     filtered_schizophrenia$Symptom)))

# Criar um dataframe de arestas
edges <- data.frame(from = c(filtered_schizophrenia$Disorder, filtered_schizophrenia$Intermediate),
                    to = c(filtered_schizophrenia$Intermediate, filtered_schizophrenia$Symptom))

# Remover auto-loops
edges <- edges[edges$from != edges$to, ]

# Ajustando as arestas para evitar duplicatas
edges <- unique(edges)

# Definindo cores com base na classificação
nodes$color <- ifelse(nodes$label %in% filtered_schizophrenia$Disorder, "darkblue", 
                      ifelse(nodes$label %in% filtered_schizophrenia$Intermediate, "#ffcc99", 
                             "darkred"))
#######

# Filtrar apenas os intermediários
intermediate_nodes <- nodes[nodes$label %in% schizophrenia$Intermediate, ]

# Calcular o número de arestas que saem de cada nó intermediário
degree_out <- table(edges$from[edges$from %in% intermediate_nodes$label])  # Contar arestas que saem

# Criar um dataframe com os graus
degree_df <- data.frame(label = names(degree_out), degree_out = as.numeric(degree_out))

# Normalizar o tamanho dos nós (opcional)
max_degree <- max(degree_df$degree_out, na.rm = TRUE)
degree_df$size <- (degree_df$degree_out / max_degree) * 30 + 10  # Tamanho mínimo de 10

# Juntar os tamanhos ao dataframe de nós intermediários
intermediate_nodes <- merge(intermediate_nodes, degree_df[, c("label", "size")], by = "label", all.x = TRUE)

# Adicionar os tamanhos ao dataframe original de nós
nodes <- merge(nodes, intermediate_nodes[, c("label", "size")], by = "label", all.x = TRUE)

# Preencher NA com um tamanho padrão para outros nós (não intermediários)
nodes$size[is.na(nodes$size)] <- 10  # Tamanho padrão para nós não intermediários


#######


# Criar o grafo usando visNetwork
visNetwork(nodes, edges) %>%
  visNodes(color = list(background = nodes$color)) %>%
  visEdges(color = list(color = "black")) %>%  # Cor das arestas
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 128)  # Para uma disposição mais organizada
