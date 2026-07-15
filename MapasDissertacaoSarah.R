library('sf')
library('ggplot2')
library('ggrepel')
library('dplyr')
library('stringr')

bairros = st_read('~/Downloads/DIVISA_DE_BAIRROS/DIVISA_DE_BAIRROS.shp')
regionais = st_read('~/Downloads/DIVISA_DE_REGIONAIS/DIVISA_DE_REGIONAIS.shp')

bairros_com_centro <- bairros %>%
  mutate(
    X = st_coordinates(st_centroid(geometry))[,1],
    Y = st_coordinates(st_centroid(geometry))[,2]
  )

numero_bairros <- nrow(bairros_com_centro)
cores_expandidas <- colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(numero_bairros)



bairros_final = ggplot(bairros_com_centro) +
  geom_sf(aes(fill = NOME), alpha = .6, show.legend = FALSE) +
  scale_fill_manual(values = cores_expandidas) +
  geom_text_repel(aes(x = X, y = Y, label = NOME), 
                  size = 2,
                  fontface = "bold",
                  segment.color = "black",     # Cor da linha/seta que liga o nome ao bairro
                  segment.size = 0.3,           # Espessura da linha
                  box.padding = 0.2,            # Espaço livre ao redor de cada texto
                  point.padding = 0.05,
                  max.overlaps = Inf,
                  force = 2) +
  coord_sf(datum = NA) +
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(title = "Mapa de Bairros de Curitiba") 

ggsave("bairros_com_centro.png", plot = bairros_final, width = 30, height = 20, units = "cm", dpi = 300)

bairros_privadas = c('Água Verde', 'Água Verde',
                     'Ahú', 'Ahú',
                     'Alto Boqueirão',
                     'Bacacheri',
                     'Bairro Alto', 'Bairro Alto',
                     'Campina do Siqueira',
                     'Batel', 'Batel', 'Batel',
                     'Bigorrilho',
                     'Boa Vista', 'Boa Vista', 'Boa Vista', 'Boa Vista',
                     'Bom Retiro', 'Bom Retiro',
                     'Boqueirão', 'Boqueirão', 'Boqueirão', 'Boqueirão',
                     'Cabral',
                     'Cajuru', 'Cajuru', 'Cajuru',
                     'Capão Raso', 'Capão Raso', 'Capão Raso',
                     'Centro', 'Centro', 'Centro', 'Centro',
                     'Cidade Industrial de Curitiba', 'Cidade Industrial de Curitiba', 'Cidade Industrial de Curitiba', 'Cidade Industrial de Curitiba', 'Cidade Industrial de Curitiba',
                     'Cristo Rei', 'Cristo Rei',
                     'Guabirotuba', 'Hauer', 'Hauer', 'Hauer',
                     'Hugo Lange',
                     'Juvevê', 'Juvevê',
                     'Sítio Cercado',
                     'Novo Mundo', 'Novo Mundo',
                     'Portão', 'Portão', 'Portão',
                     'Prado Velho',
                     'Rebouças',
                     'Santa Cândida',
                     'Santa Felicidade', 'Santa Felicidade', 'Santa Felicidade', 'Santa Felicidade', 'Santa Felicidade',
                     'São Francisco',
                     'São Lourenço', 'São Lourenço', 'São Lourenço',
                     'Seminário', 'Seminário',
                     'Sítio Cercado',
                     'Vista Alegre',
                     'Xaxim', 'Xaxim')



contagem_privadas <- data.frame(NOME = bairros_privadas) |> 
  count(NOME, name = 'Total')
contagem_privadas

bairros_publicas <- c('Abranches',
                      'Água Verde', 'Água Verde',
                      'Ahú', 'Ahú',
                      'Alto Boqueirão', 'Alto Boqueirão',
                      'Alto da Glória',
                      'Augusta',
                      'Bacacheri',
                      'Bairro Alto', 'Bairro Alto',
                      'Barreirinha',
                      'Batel', 'Batel',
                      'Boa Vista', 'Boa Vista',
                      'Boqueirão', 'Boqueirão', 'Boqueirão', 'Boqueirão', 'Boqueirão', 'Boqueirão', 'Boqueirão', 'Boqueirão', 'Boqueirão',
                      'Cachoeira',
                      'Cajuru', 'Cajuru', 'Cajuru', 'Cajuru', 'Cajuru', 'Cajuru',
                      'Campina do Siqueira',
                      'Campo Comprido',
                      'Campo de Santana', 'Campo de Santana',
                      'Capão da Imbuia',
                      'Capão Raso', 'Capão Raso', 'Capão Raso',
                      'Caximba',
                      'Centro', 'Centro', 'Centro',
                      'Cidade Industrial de Curitiba', 'Cidade Industrial de Curitiba', 'Cidade Industrial de Curitiba', 'Cidade Industrial de Curitiba', 'Cidade Industrial de Curitiba', 'Cidade Industrial de Curitiba', 'Cidade Industrial de Curitiba', 'Cidade Industrial de Curitiba', 'Cidade Industrial de Curitiba', 'Cidade Industrial de Curitiba', 'Cidade Industrial de Curitiba', 'Cidade Industrial de Curitiba', 'Cidade Industrial de Curitiba',
                      'Cristo Rei',
                      'Fanny', 'Fanny',
                      'Fazendinha',
                      'Ganchinho',
                      'Guabirotuba',
                      'Hauer', 'Hauer',
                      'Jardim Botânico',
                      'Jardim das Américas',
                      'Jardim Social',
                      'Lindóia', 'Lindóia',
                      'Mercês', 'Mercês',
                      'Novo Mundo', 'Novo Mundo', 'Novo Mundo',
                      'Pilarzinho', 'PIlarzinho', 'Pinheirinho',
                      'Pinheirinho', 'Pinheirinho', 'Pinheirinho',
                      'Portão', 'Portão', 'Portão',
                      'Prado Velho',
                      'Rebouças', 'Rebouças', 'Rebouças', 'Rebouças',
                      'Santa Cândida', 'Santa Cândida', 'Santa Cândida',
                      'Santa Felicidade', 'Santa Felicidade', 'Santa Felicidade',
                      'Santa Quitéria',
                      'São Braz', 'São Braz', 'São Braz',
                      'São Lourenço',
                      'Seminário',
                      'Sítio Cercado', 'Sítio Cercado', 'Sítio Cercado', 'Sítio Cercado', 'Sítio Cercado', 'Sítio Cercado',
                      'Taboão',
                      'Tarumã', 'Tarumã',
                      'Tatuquara', 'Tatuquara', 'Tatuquara', 'Tatuquara',
                      'Tingui', 'Tingui',
                      'Uberaba', 'Uberaba', 'Uberaba', 'Uberaba', 'Uberaba',
                      'Umbará', 'Umbará',
                      'Vila Guaíra',
                      'Vila Izabel', 'Vila Izabel',
                      'Xaxim', 'Xaxim', 'Xaxim', 'Xaxim')

contagem_publicas <- data.frame(NOME = bairros_publicas) |> 
  count(NOME, name = 'Total')

contagem_publicas

# limpeza das colunas seu relavância

teste_bairroscentro <- bairros_com_centro |> select(-OBJECTID, -CODIGO, -TIPO, -FONTE, -CD_REGIONA, -NM_REGIONA)

# plot

ggplot(teste_bairroscentro) +
  geom_sf(aes(fill = NOME), alpha = .6, show.legend = FALSE) +
  scale_fill_manual(values = cores_expandidas) +
  geom_text_repel(aes(x = X, y = Y, label = NOME), 
                  size = 2,
                  #fontface = "bold",
                  segment.color = "black",     
                  segment.size = 0.3,           
                  box.padding = 0.2,          
                  point.padding = 0.05,
                  max.overlaps = Inf,
                  force = 2) +
  coord_sf(datum = NA) +
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(title = "Mapa de Bairros de Curitiba") 

# padronização da coluna NOME em ambos dfs

teste_bairroscentro <- teste_bairroscentro |> 
  mutate(NOME = str_trim(NOME),              
         NOME = str_to_upper(NOME),          
         NOME = iconv(NOME, to = "ASCII//TRANSLIT"))

contagem_privadas <- contagem_privadas |> 
  mutate(NOME = str_trim(NOME),
         NOME = str_to_upper(NOME),
         NOME = iconv(NOME, to = "ASCII//TRANSLIT"))

contagem_publicas <- contagem_publicas |> 
  mutate(NOME = str_trim(NOME),
         NOME = str_to_upper(NOME),
         NOME = iconv(NOME, to = "ASCII//TRANSLIT"))

# join do TOTAL

comContPri <- teste_bairroscentro |> 
  left_join(contagem_privadas, by = 'NOME')

comContPub <- teste_bairroscentro |> 
  left_join(contagem_publicas, by = 'NOME')


# substitui NA

comContPri <- comContPri |> 
  mutate(Total = ifelse(is.na(Total), 0, Total))

comContPub <- comContPub |> 
  mutate(Total = ifelse(is.na(Total), 0, Total))

# mapas com contagens

## Pública

mapaContPub <- ggplot(data = comContPub) +
  geom_sf(aes(fill = NOME), alpha = 0.6, show.legend = FALSE) +
  scale_fill_manual(values = cores_expandidas) +
  geom_text(
    data = comContPub,
    aes(x = X, y = Y, label = Total),
    size = 4,
    fontface = "bold",
    color = "black",
  ) +
  
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + 
  labs(title = "Distribuição de Escolas Públicas")

mapaContPub

## Privadas

mapaContPri <- ggplot(data = comContPri) +
  geom_sf(aes(fill = NOME), alpha = 0.6, show.legend = FALSE) +
  scale_fill_manual(values = cores_expandidas) +
  geom_text(
    data = comContPri,
    aes(x = X, y = Y, label = Total),
    size = 4,
    fontface = "bold",
    color = "black",
  ) +
  
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + 
  labs(title = "Distribuição de Escolas Privadas")

mapaContPri

# exportar

ggsave("bairros_com_centro.png", plot = teste_bairroscentro, width = 30, height = 20, units = "cm", dpi = 300)

ggsave("Privadas.png", plot = mapaContPri, width = 30, height = 20, units = "cm", dpi = 300)

ggsave("Publicas.png", plot = mapaContPub, width = 30, height = 20, units = "cm", dpi = 300)
