
# library -----------------------------------------------------------------

library(igraph)
library(tidyr)
library(ggplot2)
library(dplyr)


# data --------------------------------------------------------------------

disease_symp_complete <- read.csv2("/home/and/github/tryr/Precision_analytics/Disease_Symp_Complete.csv", sep = ',')

View(disease_symp_complete)

schizophrenia_symp <- disease_symp_complete %>% 
  filter(Chapter == 'Schizophrenia Spectrum')

View(schizophrenia_symp)

schizophrenia_symp <- schizophrenia_symp %>% 
  select(Disease, Symptom) %>%
  distinct()

grafo <- graph_from_data_frame(d = schizophrenia_symp, directed = FALSE)
plot(grafo) 

grafo2 <- matrix(c(0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0,
                  0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0), ncol = 5)
plot(graph_from_adjacency_matrix(grafo, mode =
                                   'directed'))
grafo3 <- graph_from_adjacency_matrix(grafo2, directed = FALSE)

plot(grafo3, edge.arrow.mode = 0)


disorders <- unique(schizophrenia_symp$Disorder)
