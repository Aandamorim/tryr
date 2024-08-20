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

ggplot() +
  geom_histogram(mapping = aes(replicate(1000000, rolagem2(1:20))), binwidth = 01)
rolagens <- replicate(10000, rolagem2())


# replicate dentro de aes -------------------------------------------------


ggplot()+
  geom_histogram(mapping = aes(rolagens), binwidth = 1)

?sample()


rolagem_viciada <- function() {
  dado <- 1:6
  dados <- sample(dado, 2, replace = T, prob = c(1/8, 1/8, 1/8, 1/8, 1/8, 3/8))
  sum(dados)
}
rolagem_viciada()

ggplot()+
  geom_histogram(mapping = aes(replicate(10000, rolagem_viciada())), binwidth = 1)

# Playing Cards

hand <- c('ace', 'king', 'queen', 'jack', 'ten')
dado
dim(dado)
length(dado)
naipe <-  rep('spades',5)
naipe
m <- matrix(c(hand, naipe), ncol = 2, nrow = 5)
class(m)
attributes(m)

deck <- read.csv("~/github/tryr/deck.csv")
deck
# escolhendo uma carta -------------------------------------------------

deal <- function(cards) {
  cards[1,]
}

deal(deck)

# sempre retona a primeira carta do deck


# criando uma randomização com as 52 cartas --------------------------------

aleatorio52 <- sample(1:52, size = 52)
aleatorio52

deal(deck[aleatorio52,])

embaralhar <- function(cards) {
  aleatorio52 <- sample(1:52, size =52)
  cards[aleatorio52, ]
}

deal(deck)

deck_embaralhado <- embaralhar(deck)

deal(deck_embaralhado)

mean(deck$value)


# mudando valores in loco -------------------------------------------------


deck2 <- deck

deck2[c(13, 26, 39, 52),]

deck2$value[c(13, 26, 39, 52)] <- 14

head(deck2, 13)

deck3 <- embaralhar(deck)

deck3


# encontrando os aces pra mudar seu valor ---------------------------------

deck3$value[deck3$face == "ace"] <-  14

count(deck3[deck3$face == 'ace', ])

sum(deck3$face == 'ace')

deck4 <- deck

deck4$value <- 0

# encontrando onde é True

deck4$suit == 'hearts'

# especificando 

deck4$value[deck4$suit== "hearts"]

# mudando o valor

deck4$value[deck$suit == 'hearts'] <-  1

# queen de spade vale 13, porém é difícil encontrá-la apenas com uma condição

deck4[deck4$face == 'queen', ]

deck4[deck4$suit == 'spades', ]

# combinando com boolean

deck4[deck4$suit == 'spades' & deck4$face == "queen", 'value'] <- 13

deck4

w <- c(-1, 0, 1)
x <- c(5, 15)
y <- 'February'
z <- c('monday', 'tuesday', 'friday')
 # is w possitive?

w > 0

# is x greater tahn 10 and less than 20?

x > 10 & x< 20

# is object y the word February?

y == 'February'

# is every value in z day of the week?

all(z %in% c('monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday'))

deck5 <- deck

# blackjack facecard 10 e ace 11 ou 1

deck5[deck5$face %in% c('queen', 'jack', 'king'), 'value'] <- 10

deck5[deck$face == 'ace', 'value'] <- NA


# slot machine

get_symbols <- function() {
  wheel <- c('DD', '7', "BBB", "BB", "B", 'C', '0')
  sample(wheel, size = 3, replace = T,
         prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
}

get_symbols()


num <- 15
if (num < 0) {
  print("num is negative")
  print(num)
  print("don't worry, i'll fix it")
  num <- num * -1
  print('now num is positive')
  print(num)
} else {
  print("num is already positive")
  print(num)
} 


# testando casos

if ("caso: todos iguais"){
  premio <- #busque o premio na lista
} else if ("caso: todas barras"){
  premio <- # assign 5
} else {
  ## count cherries
  premio <- #calcula o premio
}

 ## conta diamantes
 ## duplica se necessário


 ## contagem

contagem <- function(symbols) {

  
## calcula premio
  
  premio
}

# caso iguais

igual <- simbolos[1] == simbolos[2] && simbolos[2] == simbolos[3]

# barras

barras <- simbolos %in% c("B", "BB", "BBB")

# definindo a contagem do premio
# um código que olha pra subsets e onde as relações são conhecidas pode ser
# resumido em um "tabela" (lookup table)

payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, "B" = 10, "C" = 10, "0"= 0)

payouts["BB"]

# unname deixa o nome de fora do resultado

unname(payouts["BB"])

## montando


# contando a quantidade de Cs e Ds

cherries <- sum(simbolos == "C")
diamonds <- sum(simbolos == "D")

# usando o vetor de sum de cherries pra indexar o valor do premio

premio <- c(0, 2, 5)[cherries +1]

if (igual){
  payouts <- 
    c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, "B" = 10, "C" = 10, "0"= 0)
  premio <- unname(payouts[simbolos[1]])
  
} else if (all(barras)){
  premio <- 5
    
} else {
  cherries <- sum(simbolos == "C")
  premio <- premio <- c(0, 2, 5)[cherries +1]
}

diamonds <- sum(simbolos == "D")
premio * 2 ^ diamonds
## contagem



## full

premiacao <- function(simbolos) {
  # identifica caso
  igual <- simbolos[1] == simbolos[2] && simbolos[2] == simbolos[3]
  barras <- simbolos %in% c("B", "BB", "BBB")
  
  # define premio
  if (igual){
    payouts <- 
      c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, "B" = 10, "C" = 10, "0"= 0)
    premio <- unname(payouts[simbolos[1]])
    
  } else if (all(barras)){
    premio <- 5
    
  } else {
    cherries <- sum(simbolos == "C")
    premio <- premio <- c(0, 2, 5)[cherries +1]
  }
  
  #ajusta para quant diamantes
  diamonds <- sum(simbolos == "D")
  premio * 2 ^ diamonds
}

jogar <- function() {
  simbolos <- get_symbols()
  print(simbolos)
  premiacao(simbolos)
}
jogar()
dado <- 1:6

dado

rolls <- expand.grid(dado, dado)
rolls

#append

rolls$value <- rolls$Var1 + rolls$Var2

rename(rolls, sum = value)

proba <- c("1" = 1/6,"2" = 1/6, "3" = 1/6, "4" = 1/6, "5" = 1/6, "6" = 1/6)
proba2 <- c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)

rolls$Var1

proba[rolls$Var1]

# atribuir probabilidade aos resultados possíveis
rolls$prob1 <- proba[rolls$Var1]
rolls$prob2 <- proba[rolls$Var2]

#prob 2 eventos independentes = prob1 * prob2
rolls$prob <- rolls$prob1 * rolls$prob2

sum(rolls$value * rolls$prob)

proba[dados]

dados$prob <- proba2[dados]


dado <- c(1, 2, 3, 4, 5, 6)
prob <- c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)

dado <- data.frame(
  valores = dado,
  prob = prob
)
dado
dado$vxp <- dado$valores * dado$prob

sum(dado$vxp)

# esperança da slot machine

wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")

combinacoes <- expand.grid(wheel, wheel, wheel, stringsAsFactors = F)

combinacoes

probab <- c("DD" = 0.03, "7" = 0.03, "BBB" = 0.06, "BB" = 0.1, "B" = 0.25, "C" = 0.01, "0" = 0.52)


combinacoes$prob1 <- probab[combinacoes$Var1]
combinacoes$prob2 <- probab[combinacoes$Var2]
combinacoes$prob3 <- probab[combinacoes$Var3]

combinacoes$probtotal <- combinacoes$prob1 * combinacoes$prob2 * combinacoes$prob3

head(combinacoes, 3)

sum(combinacoes$probtotal)


