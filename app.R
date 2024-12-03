# library(shiny)
# library(ggplot2)
# library(dplyr)
# library(shinythemes)
# library(DT)
# library(corrplot)
# 
# # Carregar os dados
# pokemon <- read.csv("pokemon.csv")
# 
# pokemon$Generation <- cut(pokemon$Number, 
#                           breaks = c(0, 151, 251, 386, 493, 649, 721, 809, 905, Inf),
#                           labels = 1:9,
#                           include.lowest = TRUE)
# 
# pokemon$Image_URL <- paste0("https://img.pokemondb.net/sprites/sword-shield/icon/", 
#                             tolower(pokemon$Name), ".png")
# 
# 
# # UI
# ui <- fluidPage(
#   theme = shinytheme("cerulean"),
#   titlePanel("Dashboard de Pokémon"),
#   sidebarLayout(
#     sidebarPanel(
#       width = 2,
#       selectInput("generation", "Geração", 
#                   choices = c("Todas", unique(pokemon$Generation)), 
#                   selected = "Todas"),
#       selectInput("type", "Tipo", 
#                   choices = c("Todos", unique(pokemon$Type.1)), 
#                   selected = "Todos")
#     ),
#     mainPanel(
#       tabsetPanel(
#         tabPanel("Visão Geral", 
#                  fluidRow(
#                    column(6, plotOutput("typeDistribution", height = "400px")),
#                    column(6,
#                           fluidRow(
#                             column(6, selectInput("statX", "Eixo X", 
#                                                   choices = c("HP", "Attack", "Defense", "Sp.Attack", "Sp.Defense", "Speed"))),
#                             column(6, selectInput("statY", "Eixo Y", 
#                                                   choices = c("HP", "Attack", "Defense", "Sp.Attack", "Sp.Defense", "Speed")))
#                           ),
#                           plotOutput("scatterPlot", height = "326px")
#                    )
#                  ),
#                  fluidRow(
#                    column(6,
#                           h4("Top 10"),
#                           DTOutput("pokemonTable")
#                    ),
#                    column(6,
#                           h4("Correlação"),
#                           plotOutput("correlationHeatmap", height = "446px"))
#                  ),
#         ),
#         tabPanel("Detalhes do Pokémon",
#                  selectInput("pokemon", "Selecione um Pokémon", choices = NULL),
#                  fluidRow(
#                    column(4, uiOutput("pokemonImage")),
#                    column(8, tableOutput("pokemonDetails"))
#                  )
#         )
#       )
#     )
#   )
# )
# 
# # Server
# server <- function(input, output, session) {
#   
#   # Filtrar dados por geração e tipo
#   filtered_data <- reactive({
#     data <- pokemon
#     if (input$generation != "Todas") {
#       data <- data %>% filter(Generation == input$generation)
#     }
#     if (input$type != "Todos") {
#       data <- data %>% filter(Type.1 == input$type)
#     }
#     data
#   })
#   
#   # Atualizar as opções de Pokémon com base na geração e tipo selecionados
#   observe({
#     updateSelectInput(session, "pokemon",
#                       choices = filtered_data()$Name)
#   })
#   
#   # Distribuição de Tipos
#   output$typeDistribution <- renderPlot({
#     type_counts <- filtered_data() %>%
#       group_by(Type.1) %>%
#       summarise(Count = n()) %>%
#       arrange(desc(Count))
#     
#     ggplot(type_counts, aes(x = reorder(Type.1, -Count), y = Count, fill = Type.1)) +
#       geom_bar(stat = "identity") +
#       theme_minimal() +
#       theme(axis.text.x = element_text(angle = 45, hjust = 1),
#             legend.position = "none") +
#       labs(title = "Distribuição de Tipos",
#            x = "Tipo Primário", y = "Quantidade de Pokémon")
#   })
#   
#   # Comparação de Estatísticas
#   output$scatterPlot <- renderPlot({
#     ggplot(filtered_data(), aes_string(x = input$statX, y = input$statY, color = "Type.1")) +
#       geom_point() +
#       labs(title = paste("Comparação:", input$statX, "vs", input$statY), x = input$statX, y = input$statY) +
#       theme_minimal() +
#       theme(legend.position = "bottom")
#   })
#   
#   # Tabela de Pokémon
#   output$pokemonTable <- renderDT({
#     filtered_data() %>%
#       mutate(Total = HP + Attack + Defense + Sp.Attack + Sp.Defense + Speed) %>%
#       select(Nome = Name, Total, Ataque = Attack, Velocidade = Speed, Tipo = Type.1) %>%
#       datatable(options = list(pageLength = 10,
#                                dom = 'ft',
#                                order = list(list(1, 'desc'))),
#                 rownames = FALSE)
#   })
#   
#   # Mapa de Calor de Correlações
#   output$correlationHeatmap <- renderPlot({
#     cor_matrix <- cor(filtered_data()[, c("HP", "Attack", "Defense", "Sp.Attack", "Sp.Defense", "Speed")])
#     corrplot(cor_matrix, method="color", type="upper", order="hclust",
#              tl.col="black", tl.srt=45, addCoef.col="black",
#              mar=c(2,0.5,2,0.5)) # Ajustando margens para alinhamento
#   })
# 
#   
#   # Detalhes do Pokémon
#   output$pokemonDetails <- renderTable({
#     req(input$pokemon)
#     pokemon %>%
#       filter(Name == input$pokemon) %>%
#       select(Nome = Name, Tipo1=Type.1, Tipo2=Type.2,
#              HP,Ataque=Attack,Defesa=Defense,AtaqueEsp=Sp.Attack,
#              DefesaEsp=Sp.Defense,Velocidade=Speed)
#   })
#   
#   # Imagem do Pokémon
#   output$pokemonImage <- renderUI({
#     req(input$pokemon)
#     selected_pokemon <- pokemon[pokemon$Name == input$pokemon, ]
#     tags$img(src = selected_pokemon$Image_URL, alt = input$pokemon, height = "100px")
#   })
#   
# }
# 
# # Run the application 
# shinyApp(ui=ui, server=server)


library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(DT)
library(corrplot)

# Carregar os dados
pokemon <- read.csv("pokemon.csv")

pokemon$Generation <- cut(pokemon$Number, 
                          breaks = c(0, 151, 251, 386, 493, 649, 721, 809, 905, Inf),
                          labels = 1:9,
                          include.lowest = TRUE)

generations_labels <- paste0(1:9, "ª Geração")

# Adicionar URLs das imagens
pokemon$Image_URL <- paste0("https://img.pokemondb.net/sprites/sword-shield/icon/", tolower(gsub(" ", "-", pokemon$Name)), ".png")

# UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  tags$head(
    tags$style(HTML("
    .title-centered {
                    text-align: center;
    }
    "))
  ),
  titlePanel(
    title = div(class = "title-centered", "Dashboard de Pokémon")),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      selectInput("generation", "Geração", 
                  choices = c("Todas", setNames(as.character(1:9),
                  generations_labels)), 
                  selected = "Todas"),
      selectInput("type", "Tipo", 
                  choices = c("Todos", sort(unique(pokemon$Type.1))), 
                  selected = "Todos")
    ),
    mainPanel(
      width = 10,
      tabsetPanel(
        tabPanel("Visão Geral", 
                 fluidRow(
                   column(6, plotOutput("typeDistribution", height = "400px")),
                   column(6,
                          fluidRow(
                            column(6, selectInput("statX", "Eixo X", 
                                                  choices = c("HP", "Attack", "Defense", "Sp.Attack", "Sp.Defense", "Speed"))),
                            column(6, selectInput("statY", "Eixo Y", 
                                                  choices = c("HP", "Attack", "Defense", "Sp.Attack", "Sp.Defense", "Speed")))
                          ),
                          plotOutput("scatterPlot", height = "326px")
                   )
                 ),
                 fluidRow(
                   column(6,
                          h4("Top 10"),
                          DTOutput("pokemonTable")
                   ),
                   column(6, 
                          h4("Correlação"),
                          plotOutput("correlationHeatmap", height = "446px"))
                 )
        ),
        tabPanel("Lista Completa",
                 DTOutput("fullPokemonList")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Filtrar dados por geração e tipo
  filtered_data <- reactive({
    data <- pokemon
    if (input$generation != "Todas") {
      data <- data %>% filter(Generation == input$generation)
    }
    if (input$type != "Todos") {
      data <- data %>% filter(Type.1 == input$type)
    }
    data
  })
  
  # Distribuição de Tipos
  output$typeDistribution <- renderPlot({
    type_counts <- filtered_data() %>%
      group_by(Type.1) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count))
    
    ggplot(type_counts, aes(x = reorder(Type.1, -Count), y = Count, fill = Type.1)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none") +
      labs(title = "Distribuição de Tipos",
           x = "Tipo Primário", y = "Quantidade de Pokémon")
  })
  
  # Comparação de Estatísticas
  output$scatterPlot <- renderPlot({
    ggplot(filtered_data(), aes_string(x = input$statX, y = input$statY, color = "Type.1")) +
      geom_point() +
      labs(title = paste("Comparação:", input$statX, "vs", input$statY), x = input$statX, y = input$statY) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Tabela de Pokémon
  output$pokemonTable <- renderDT({
    filtered_data() %>%
      mutate(Total = HP + Attack + Defense + Sp.Attack + Sp.Defense + Speed) %>%
      select(Nome = Name, Total, Ataque = Attack, Velocidade = Speed, Tipo = Type.1) %>%
      datatable(options = list(pageLength = 10, 
                               dom = 'ft', 
                               order = list(list(1, 'desc'))),
                rownames = FALSE)
  })
  
  # Mapa de Calor de Correlações
  output$correlationHeatmap <- renderPlot({
    cor_matrix <- cor(filtered_data()[, c("HP", "Attack", "Defense", "Sp.Attack", "Sp.Defense", "Speed")])
    corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", 
             tl.col = "black", tl.srt = 45, addCoef.col = "black")
  })
  
  # Lista completa de Pokémon com imagens
  output$fullPokemonList <- renderDT({
    filtered_data() %>%
      mutate(
        Imagem = paste0('<img src="', Image_URL, '" height="40">'),
        Total = HP + Attack + Defense + Sp.Attack + Sp.Defense + Speed
      ) %>%
      select(Imagem, Número = Number, Nome = Name, Tipo1 = Type.1, Tipo2 = Type.2, 
             HP, Ataque = Attack, Defesa = Defense, 
             AtaqueEsp = Sp.Attack, DefesaEsp = Sp.Defense, 
             Velocidade = Speed, Total, Geração = Generation) %>%
      datatable(
        options = list(
          pageLength = 50,  # Aumenta o número de linhas por página
          scrollY = "calc(100vh - 200px)",
          scrollCollapse = TRUE,
          dom = 'ft',
          ordering = TRUE
        ),
        rownames = FALSE,
        escape = FALSE
      ) %>%
      formatStyle('Imagem', width = '60px')
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
