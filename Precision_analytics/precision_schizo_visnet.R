install.packages("visNetwork")

library(visNetwork)

schizophrenia_sympt <- 
  read.csv2("/home/and/github/tryr/Precision_analytics/schizophrenia_symp.csv", 
            header = TRUE)


nodes <- data.frame(id = c(schizophrenia_sympt$Disorder, 
                           schizophrenia_sympt$Intermediate, 
                           schizophrenia_sympt$Symptom),
                    label = c(schizophrenia_sympt$Disorder, 
                              schizophrenia_sympt$Intermediate, 
                              schizophrenia_sympt$Symptom))

edges <- data.frame(from = c(schizophrenia_sympt$Disorder,
                             schizophrenia_sympt$Intermediate),
                    to = c(schizophrenia_sympt$Intermediate,
                           schizophrenia_sympt$Symptom))

edges <- unique(edges)

visNetwork(nodes, edges) %>%
  visIgraphLayout() %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visInteraction(navigationButtons = TRUE) %>%
  visLayout(randomSeed = 123) %>%
  visEdges(arrows = 'to') %>%
  visPhysics(stabilization = TRUE) %>%
  visNodes(color = list(background = c("forestgreen", "darkorchid4", "darkorange")))