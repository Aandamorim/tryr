# Weighted Dice


# carregando libs ---------------------------------------------------------


library(dplyr)
library(ggplot2)


# criando um dado em R ----------------------------------------------------


dado <- 1:6
dado
sample(dado,2, replace = T)

dados <- sample(dado, 2, replace = T)


# rolando dois dados e somando --------------------------------------------


rolagem <- function() {
  dado <- 1:6
  dados <- sample(dado, 2, replace = T)
  sum(dados)
}

rolagem()

# rolagem com argumento nº lados do dado (default d6) ---------------------


rolagem2 <- function(lados = 1:6) {
  dados <- sample(lados, size = 2, replace = T)
  sum(dados)
}

rolagem2(1:20)


# plotando a rolagem ------------------------------------------------------

x <- c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)

y <- x^3

ggplot()+
  geom_point(mapping = aes(x = x, y = y))


# posso fazer a operação dentro da aes do gráfico -------------------------

# ggplot()+
#   geom_point(mapping = aes(x = x, y = x^3))

x3 <- c(0, 1, 1, 2, 2, 2, 3, 3, 4)
ggplot()+
  geom_histogram(mapping = aes(x = x3), binwidth = 1)

rolagens <- replicate(10000, rolagem2())


# replicate dentro de aes -------------------------------------------------


ggplot()+
  geom_histogram(mapping = aes(rolagens), binwidth = 1)

sample()


rolagem_viciada <- function() {
  dado <- 1:6
  dados <- sample(dado, 2, replace = T, prob = c(1/8, 1/8, 1/8, 1/8, 1/8, 3/8))
  sum(dados)
}


ggplot()+
  geom_histogram(mapping = aes(
    replicate(
      10000, rolagens_viciadas
      )
    ), binwidth = 1)


# Playing Cards

hand <- c('ace', 'king', 'queen', 'jack', 'ten')

dim(dado)

naipe <-  rep('spades',5)
naipe
m <- matrix(c(hand, naipe), ncol = 2, nrow = 5)
class(m)
attributes(m)

deck <- read.csv("C:/Users/aanda/OneDrive/Documentos/hands_on_r/hands_on_r/deck.csv")

write.csv(deck, file = "cards.csv", row.names = FALSE)
