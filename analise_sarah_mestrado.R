library(readxl)  # Para ler arquivos Excel
library(tidyverse)  # Para manipulação de dados
library(stringr)  # Para manipulação de strings
library(tm)  # Para processamento de texto
library(SnowballC)  # Para stemming
library(wordcloud2)  # Para nuvens de palavras interativas

nomes_abas <- excel_sheets("/home/and/Downloads/Contagem de palavras-chave_ escola.xlsx")

textos_colegios <- list()

for (aba in nomes_abas) {
  texto <- read_excel("/home/and/Downloads/Contagem de palavras-chave_ escola.xlsx",
                      sheet = aba,
                      range = "A1",
                      col_names = FALSE)
  textos_colegios[[aba]] <- texto[[1]]
}

# Vetor de palavras chave

termos_chave <- c(
  "Vestibular" = "vestibul",
  "ENEM" = "enem",
  "Ensino Superior" = "superior",
  "Universidade" = "universidades",
  "Trabalho" = "trabalho",
  "Projeto de Vida" = "projeto de vida",
  "Competências e Habilidades" = "competências e habilidades",
  "BNCC" = "bncc",
  "Empreendedorismo" = "empreend",
  "Tecnologia" = c("tecnolog", "tecnológ"),
  "Profissional" = "profiss"
)

# Função de pré-processamento
preprocessar <- function(text){
  text %>% 
    tolower() %>% 
    removePunctuation() %>%
    removeNumbers() %>%
    stripWhitespace()
}

# Função de stemming
stemming <- function(text) {
  palavras <- unlist(strsplit(text, "\\s+"))
  palavras_stem <- wordStem(palavras, language = "portuguese")
  paste(palavras_stem, collapse = " ")
}

# Função para contar termos
contar_termos <- function(text, termos) {
  # Pré-processar o texto
  texto_processado <- preprocessar(text)
  
  # Aplicar stemming ao texto processado
  texto_stem <- stemming(texto_processado)
  
  # Inicializar um vetor para armazenar as contagens
  contagens <- integer(length(termos))
  
  # Contar ocorrências de cada termo
  for (i in seq_along(termos)) {
    padrao <- termos[i]
    padrao_stem <- stemming(preprocessar(padrao))
    contagens[i] <- str_count(texto_stem, regex(padrao_stem, ignore_case = TRUE))
  }
  
  # Nomear o vetor de contagens
  names(contagens) <- names(termos)
  
  return(contagens)
}

resultados <- map(textos_colegios, ~contar_termos(., termos_chave))

resultados_df <- bind_rows(resultados, .id = "Colégio")

#somar colunas que contam o mesmo termo através de um vetor possível

resultados_df <- resultados_df %>%
  mutate(Tecnologia = rowSums(select(., Tecnologia1, Tecnologia2), na.rm = TRUE)) %>%
  select(-Tecnologia1, -Tecnologia2)

resultados_df <- resultados_df %>%
  mutate(Universidade = rowSums(select(., Universidade1, Universidade2), na.rm = TRUE)) %>%
  select(-Universidade1, -Universidade2)

total_ocorrencias <-  resultados_df %>% 
  select(-Colégio) %>%
  summarise(across(everything(), sum)) %>% 
  t() %>%
  as.data.frame()

colnames(total_ocorrencias) <- "frequencia"

wordcloud2(data = total_ocorrencias, size = 0.5, color = "random-dark")

total_ocorrencias <- data.frame(
  palavra = rownames(total_ocorrencias),
  frequencia = total_ocorrencias$frequencia,
  stringsAsFactors = FALSE
)

