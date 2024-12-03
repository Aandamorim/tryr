
# library -----------------------------------------------------------------

library(igraph)
library(tidyr)
library(ggplot2)
library(dplyr)


# data --------------------------------------------------------------------

disease_symp_complete <- read.csv2("Disease_Symp_Complete.csv", sep = ',')

View(disease_symp_complete)

schizophrenia_symp <- disease_symp_complete %>% 
  filter(Chapter == 'Schizophrenia Spectrum')

View(schizophrenia_symp)



