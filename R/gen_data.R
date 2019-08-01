#####################################################################################################
# Etapa 00: Carregamento e adequação de bibliotecas e base de dados

## Busca, instalação e carregamento automático dos pacotes necessários
### remotes: Habilitação da instalação de pacotes no github
### taRifx.geo: Geocoding com APIs do Bing e Google
### geosphere: Cálculo da distância entre duas coordenadas numa elipsoide
### forcats: Transformação das variáveis de qualidade em fatores com níveis claros
### lsr: Cálculo da correlação entre variáveis nominais através do V de Cramer
### tidyverse: Organização e manejo dos dados
### visNetwork: Geração do Gráfico de rede
### RColorBrewer: Palhetas de cores para gráficos
if (!require("remotes")) install.packages("remotes") ; library(remotes)
if (!require("taRifx.geo")) remotes::install_github("gsk3/taRifx.geo") ; library(taRifx.geo)
if (!require("geosphere")) install.packages("geosphere") ; library(geosphere)
if (!require("forcats")) install.packages("forcats") ; library(forcats)
if (!require("lsr")) install.packages("lsr") ; library(lsr)
if (!require("psych")) install.packages("psych") ; library(psych)
if (!require("scales")) install.packages("scales") ; library(scales)
if (!require("purrr")) install.packages("purrr") ; library(purrr)
if (!require("BBmisc")) install.packages("BBmisc") ; library(BBmisc)
if (!require("tidyverse")) install.packages("tidyverse") ; library(tidyverse)
if (!require("visNetwork")) install.packages("visNetwork") ; library(visNetwork)
if (!require("RColorBrewer")) install.packages("RColorBrewer") ; library(RColorBrewer)
if (!require("sp")) install.packages("sp") ; library(sp)
if (!require("stplanr")) install.packages("stplanr") ; library(stplanr)
if (!require("leaflet")) install.packages("leaflet") ; library(leaflet)

## Funcao para mutate somente em secoes do dataframe em que variaveis obedecem certa condicao 
mutate_cond <- function(.data, condition, ..., new_init = NA, envir = parent.frame()) {
  # Initialize any new variables as new_init
  new_vars <- substitute(list(...))[-1]
  new_vars %<>% sapply(deparse) %>% names %>% setdiff(names(.data))
  .data[, new_vars] <- new_init
  
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data %>% filter(condition) %>% mutate(...)
  .data
}

## Chama as keys dos APIS a partir do Renviron da maquina local
BingMapsKey <- Sys.getenv("BingMapsKey")
GRAPHHOPPER <- Sys.getenv("GRAPHHOPPER")
options(BingMapsKey = BingMapsKey)

## Adiciona as paradas do Buzufba. Paradas 1-19 sao pontos de parada dos veiculos
## 20-25 sao pontos por onde os onibus apenas passam
stops <- data.frame(locals = c("Estacionamento PAF I", "R5: Garibaldi", "Arquitetura",
                               "Politecnica", "Creche Canela", "Reitoria",
                               "Geociencias", "ADM/FACED/ICS/FAMEB", "Viaduto do Campo Grande",
                               "Sao Lazaro", "R1: Corredor da Vitoria", "R2: Largo da Vitoria",
                               "R3: Deli e Cia", "Direito", "Musica/ISC",
                               "Belas-Artes", "Odontologia", "Nutricao",
                               "Economia", "Vasco da Gama", "Orixas Center",
                               "Piedade", "Campo Grande", "Reitor Miguel Calmon",
                               "Rotula Reis Catolicos"),
                    lat = c(-13.001850, -12.999221, -12.997254,
                            -12.999031, -12.995121, -12.992404,
                            -12.998513, -12.995006, -12.989718,
                            -13.005253, -12.994019, -12.996367,
                            -12.997606, -12.996312, -12.994672,
                            -12.991260, -12.994857, -12.993076,
                            -12.983977, -12.990312, -12.985864,
                            -12.982551, -12.987767, -12.994355,
                            -12.995540),
                    long = c(-38.507053, -38.505888, -38.508919,
                             -38.511497, -38.517132, -38.520148,
                             -38.506496, -38.519894, -38.521589,
                             -38.512766, -38.526406, -38.527435,
                             -38.519151, -38.521780, -38.522011,
                             -38.521103, -38.523011, -38.522089,
                             -38.515399, -38.503386, -38.517457,
                             -38.514312, -38.521913, -38.525276,
                             -38.514770),
                    stringsAsFactors = FALSE)

############### EXPRESSO ##################
## Adiciona as rotas do Buzufba (Ida do EXPRESSO)
EXPRESSO.ida <- data.frame(local = c("Estacionamento PAF I", "R5: Garibaldi", "Arquitetura", "Politecnica",
                               "Creche Canela", "Reitoria", "Belas-Artes"))
EXPRESSO.ida <- EXPRESSO.ida %>% mutate(dist = rep(0, dim(EXPRESSO.ida)[1]))

## Calcula as distancias entre as paradas do Buzufba (Ida do EXPRESSO)
for (i in 2:dim(EXPRESSO.ida)[1]) {
  ### Recupera as coordenadas dos pontos de parada registrados
  ptA <- stops[match(EXPRESSO.ida$local[i-1], stops$locals), 2:3]
  ptB <- stops[match(EXPRESSO.ida$local[i], stops$locals), 2:3]
  
  EXPRESSO.ida$dist[i] <- as.numeric(georoute(rbind(ptA, ptB), returntype = "distance"))
}

## Adiciona as rotas do Buzufba (Volta do EXPRESSO)
EXPRESSO.volta <- data.frame(local = c("Belas-Artes", "Reitoria", "Creche Canela", "Politecnica", "Arquitetura",
                                       "Geociencias", "Estacionamento PAF I"))
EXPRESSO.volta <- EXPRESSO.volta %>% mutate(dist = rep(0, dim(EXPRESSO.volta)[1]))

## Calcula as distancias entre as paradas do Buzufba (Volta do EXPRESSO)
for (i in 2:dim(EXPRESSO.volta)[1]) {
  ### Recupera as coordenadas dos pontos de parada registrados
  ptA <- stops[match(EXPRESSO.volta$local[i-1], stops$locals), 2:3]
  ptB <- stops[match(EXPRESSO.volta$local[i], stops$locals), 2:3]
  
  EXPRESSO.volta$dist[i] <- as.numeric(georoute(rbind(ptA, ptB), returntype = "distance"))
}

################## B1 #####################
## Adiciona as rotas do Buzufba (Ida do B1)
B1.ida <- data.frame(local = c("Estacionamento PAF I", "R5: Garibaldi", "Rotula Reis Catolicos",
                               "ADM/FACED/ICS/FAMEB", "Viaduto do Campo Grande", "Belas-Artes", "Reitoria"))
B1.ida <- B1.ida %>% mutate(dist = rep(0, dim(B1.ida)[1]))

## Calcula as distancias entre as paradas do Buzufba (Ida do B1)
for (i in 2:dim(B1.ida)[1]) {
  ### Recupera as coordenadas dos pontos de parada registrados
  ptA <- stops[match(B1.ida$local[i-1], stops$locals), 2:3]
  ptB <- stops[match(B1.ida$local[i], stops$locals), 2:3]
  
  B1.ida$dist[i] <- as.numeric(georoute(rbind(ptA, ptB), returntype = "distance"))
}

## Adiciona as rotas do Buzufba (Volta do B1)
B1.volta <- data.frame(local = c("Reitoria", "Creche Canela", "Politecnica", "Sao Lazaro", "Politecnica",
                                 "Arquitetura", "Geociencias", "Estacionamento PAF I"))
B1.volta <- B1.volta %>% mutate(dist = rep(0, dim(B1.volta)[1]))

## Calcula as distancias entre as paradas do Buzufba (Volta do B1)
for (i in 2:dim(B1.volta)[1]) {
  ### Recupera as coordenadas dos pontos de parada registrados
  ptA <- stops[match(B1.volta$local[i-1], stops$locals), 2:3]
  ptB <- stops[match(B1.volta$local[i], stops$locals), 2:3]
  
  B1.volta$dist[i] <- as.numeric(georoute(rbind(ptA, ptB), returntype = "distance"))
}

################## B2 #####################
## Adiciona as rotas do Buzufba (Ida do B2)
B2.ida <- data.frame(local = c("Estacionamento PAF I", "R5: Garibaldi", "Arquitetura", "Politecnica",
                               "Creche Canela", "Reitoria"))
B2.ida <- B2.ida %>% mutate(dist = rep(0, dim(B2.ida)[1]))

## Calcula as distancias entre as paradas do Buzufba (Ida do B2)
for (i in 2:dim(B2.ida)[1]) {
  ### Recupera as coordenadas dos pontos de parada registrados
  ptA <- stops[match(B2.ida$local[i-1], stops$locals), 2:3]
  ptB <- stops[match(B2.ida$local[i], stops$locals), 2:3]
  
  B2.ida$dist[i] <- as.numeric(georoute(rbind(ptA, ptB), returntype = "distance"))
}

## Adiciona as rotas do Buzufba (Volta do B2)
B2.volta <- data.frame(local = c("Reitoria", "R1: Corredor da Vitoria", "R2: Largo da Vitoria",
                                 "R3: Deli e Cia", "Politecnica", "Sao Lazaro", "Politecnica",
                                 "Arquitetura", "Geociencias", "Estacionamento PAF I"))
B2.volta <- B2.volta %>% mutate(dist = rep(0, dim(B2.volta)[1]))

## Calcula as distancias entre as paradas do Buzufba (Volta do B2)
for (i in 2:dim(B2.volta)[1]) {
  ### Recupera as coordenadas dos pontos de parada registrados
  ptA <- stops[match(B2.volta$local[i-1], stops$locals), 2:3]
  ptB <- stops[match(B2.volta$local[i], stops$locals), 2:3]
  
  B2.volta$dist[i] <- as.numeric(georoute(rbind(ptA, ptB), returntype = "distance"))
}

################## B3 #####################
## Adiciona as rotas do Buzufba (Ida do B3)
B3.ida <- data.frame(local = c("Estacionamento PAF I", "R5: Garibaldi", "Arquitetura", "Politecnica",
                               "Sao Lazaro", "Politecnica", "Creche Canela", "R3: Deli e Cia", "Direito"))
B3.ida <- B3.ida %>% mutate(dist = rep(0, dim(B3.ida)[1]))

## Calcula as distancias entre as paradas do Buzufba (Ida do B3)
for (i in 2:dim(B3.ida)[1]) {
  ### Recupera as coordenadas dos pontos de parada registrados
  ptA <- stops[match(B3.ida$local[i-1], stops$locals), 2:3]
  ptB <- stops[match(B3.ida$local[i], stops$locals), 2:3]
  
  B3.ida$dist[i] <- as.numeric(georoute(rbind(ptA, ptB), returntype = "distance"))
}

## Adiciona as rotas do Buzufba (Volta do B3)
B3.volta <- data.frame(local = c("Direito", "ADM/FACED/ICS/FAMEB", "Rotula Reis Catolicos",
                                 "Geociencias", "Estacionamento PAF I"))
B3.volta <- B3.volta %>% mutate(dist = rep(0, dim(B3.volta)[1]))

## Calcula as distancias entre as paradas do Buzufba (Volta do B3)
for (i in 2:dim(B3.volta)[1]) {
  ### Recupera as coordenadas dos pontos de parada registrados
  ptA <- stops[match(B3.volta$local[i-1], stops$locals), 2:3]
  ptB <- stops[match(B3.volta$local[i], stops$locals), 2:3]
  
  B3.volta$dist[i] <- as.numeric(georoute(rbind(ptA, ptB), returntype = "distance"))
}

################## B4 #####################
## Adiciona as rotas do Buzufba (Ida do B4)
B4.ida <- data.frame(local = c("Estacionamento PAF I", "R5: Garibaldi", "Arquitetura", "Politecnica", "Sao Lazaro",
                               "Politecnica", "Creche Canela", "R3: Deli e Cia", "Direito", "Musica/ISC",
                               "Odontologia", "Nutricao", "Reitoria"))
B4.ida <- B4.ida %>% mutate(dist = rep(0, dim(B4.ida)[1]))

## Calcula as distancias entre as paradas do Buzufba (Ida do B4)
for (i in 2:dim(B4.ida)[1]) {
  ### Recupera as coordenadas dos pontos de parada registrados
  ptA <- stops[match(B4.ida$local[i-1], stops$locals), 2:3]
  ptB <- stops[match(B4.ida$local[i], stops$locals), 2:3]
  
  B4.ida$dist[i] <- as.numeric(georoute(rbind(ptA, ptB), returntype = "distance"))
}

## Adiciona as rotas do Buzufba (Volta do B4)
B4.volta <- data.frame(local = c("Reitoria", "Creche Canela", "Politecnica", "Sao Lazaro", "Politecnica",
                                 "Arquitetura", "R5: Garibaldi", "Estacionamento PAF I"))
B4.volta <- B4.volta %>% mutate(dist = rep(0, dim(B4.volta)[1]))

## Calcula as distancias entre as paradas do Buzufba (Volta do B4)
for (i in 2:dim(B4.volta)[1]) {
  ### Recupera as coordenadas dos pontos de parada registrados
  ptA <- stops[match(B4.volta$local[i-1], stops$locals), 2:3]
  ptB <- stops[match(B4.volta$local[i], stops$locals), 2:3]
  
  B4.volta$dist[i] <- as.numeric(georoute(rbind(ptA, ptB), returntype = "distance"))
}

################## B5 #####################
## Adiciona as rotas do Buzufba (Ida do B5)
B5.ida <- data.frame(local = c("Estacionamento PAF I", "R5: Garibaldi", "Vasco da Gama", "Orixas Center",
                               "Economia", "Piedade"))
B5.ida <- B5.ida %>% mutate(dist = rep(0, dim(B5.ida)[1]))

## Calcula as distancias entre as paradas do Buzufba (Ida do B5)
for (i in 2:dim(B5.ida)[1]) {
  ### Recupera as coordenadas dos pontos de parada registrados
  ptA <- stops[match(B5.ida$local[i-1], stops$locals), 2:3]
  ptB <- stops[match(B5.ida$local[i], stops$locals), 2:3]
  
  B5.ida$dist[i] <- as.numeric(georoute(rbind(ptA, ptB), returntype = "distance"))
}

## Adiciona as rotas do Buzufba (Volta do B5)
B5.volta <- data.frame(local = c("Piedade", "Economia", "Campo Grande", "Viaduto do Campo Grande",
                                 "Reitor Miguel Calmon", "ADM/FACED/ICS/FAMEB", "Rotula Reis Catolicos",
                                 "Geociencias", "Estacionamento PAF I"))
B5.volta <- B5.volta %>% mutate(dist = rep(0, dim(B5.volta)[1]))

## Calcula as distancias entre as paradas do Buzufba (Volta do B5)
for (i in 2:dim(B5.volta)[1]) {
  ### Recupera as coordenadas dos pontos de parada registrados
  ptA <- stops[match(B5.volta$local[i-1], stops$locals), 2:3]
  ptB <- stops[match(B5.volta$local[i], stops$locals), 2:3]
  
  B5.volta$dist[i] <- as.numeric(georoute(rbind(ptA, ptB), returntype = "distance"))
}

## Carrega o banco de dados
dados <- read.delim("Data/bus2.txt")

## Carrega a lista de locais de transito dos usuarios
locais <- read.csv("Data/locais.csv", sep= ";", colClasses = c("character", "factor"))
locais <- mutate(locais,
                 lat = c(-12.99248, -12.99248, -12.99248, -12.981339, -13.001726, -12.994536, -12.99826,
                         -13.005857, -12.99248, -12.99248, -12.99493, -12.995111, -12.991510, -13.005353,
                         -12.993322, -13.004377, -12.992450, -12.993150, -12.992450, -12.973165, -12.999470,
                         -12.996976, -12.983626,  -12.983626, -13.001675, -12.996155, -12.995363, -13.000177,
                         -13.005559, -12.994608, -12.995097, -12.993133, -13.003466, -13.001385, -12.997331,
                         -12.994713, -12.998904, -12.99826, -13.002500, -13.001170, -13.005911, -12.999490,
                         -12.994373, -12.986657, -12.972747, -13.005639, -13.002633, NA, NA, NA, NA,
                         -12.99248, -13.002633, -13.003494, -13.003160, -13.000824, -12.997764, -13.005015,
                         -12.995605, -13.005015, -12.993800, -12.997378, -12.997764, -13.001610, -12.992320,
                         -12.995220, -13.001079, -12.995220, -12.99248, -12.99248, -12.993948, -12.996119,
                         -12.999324, -12.99827, -13.002159, -13.00556, -12.99248, -12.992182, -12.99248,
                         -12.992182, -13.00316, -13.00316, -13.00082, -13.001598),
                 long = c(-38.52064, -38.52064, -38.52064, -38.516656, -38.509122, -38.522926, -38.50663,
                          -38.514148, -38.52064, -38.52064, -38.51745, -38.520031, -38.521252, -38.509760,
                          -38.520673, -38.508770, -38.521790,  -38.521999, -38.521790, -38.509815,  -38.510311,
                          -38.508297, -38.515171,  -38.515171,  -38.510013, -38.521423, -38.519322,  -38.507926,
                          -38.513763, -38.520318, -38.522713, -38.520044, -38.510000, -38.508351, -38.508913,
                          -38.519180, -38.508105, -38.50663, -38.509409, -38.507571, -38.513962, -38.507340,
                          -38.521868, -38.518963, -38.509941, -38.514123, -38.509282, NA, NA, NA, NA,
                          -38.52064, -38.509282, -38.509391, -38.509501, -38.507619, -38.509417, -38.512857,
                          -38.518652, -38.512857, -38.520881, -38.509303, -38.509417, -38.508143, -38.520442,
                          -38.523710, -38.510041, -38.523710, -38.52064, -38.52064, -38.526680, -38.527769,
                          -38.518742, -38.50584, -38.508477, -38.51376, -38.52064, -38.521484, -38.52064,
                          -38.521484, -38.5095, -38.5095, -38.50762, -38.507526),
                 prox.stop = rep(NA, dim(locais)[1]),
                 dist.met = rep(NA, dim(locais)[1]))

## Busca o ponto de Buzufba mais proximo dos locais de transito (distancia em metros)
for (i in 1:dim(locais)[1]) {
  if (is.na(locais$lat[i])) {
    locais$prox.stop[i] <- NA
    locais$dist.met[i] <- NA
    next
  }
  aprox.dist <- distGeo(locais[i, 3:4], stops[1:19, 2:3]) ### Gera o vetor de distancias somente com os pontos válidos
  aprox.stop <- which.min(aprox.dist) ### Encontra a parada mais proxima
  locais$prox.stop[i] <- stops$locals[aprox.stop] ### Guarda o ponto do Buzufba mais proximo
  locais$dist.met[i] <- aprox.dist[aprox.stop] ### Guarda a distancia em metros a este ponto
}

## Insere o ponto de Buzufba mais proximo das origens e destinos dos usuarios no banco de dados
dados <- dados %>% mutate_at(.vars = vars(starts_with("ORIGEM")),
                             .funs = funs(STOP = locais$prox.stop[match(., locais$ORIGEM.DESTINO)])) %>%
  mutate_at(.vars = vars(starts_with("DESTINO")),
            .funs = funs(STOP = locais$prox.stop[match(., locais$ORIGEM.DESTINO)]))

## Procura a presenca simultanea dos pontos Buzufba usados pelo usuario em cada roteiro de Buzufba (ida e volta) e
## fornece a possivel distancia percorrida via Buzufba para cada um dos roteiro
opcoes <- dados %>% rowwise() %>% mutate(
  EXPRESSO.ida.flag = ifelse(is.na(match(DESTINO1_STOP, EXPRESSO.ida$local) > match(ORIGEM1_STOP, EXPRESSO.ida$local)),
                              FALSE,
                              match(DESTINO1_STOP, EXPRESSO.ida$local) > match(ORIGEM1_STOP, EXPRESSO.ida$local)),
  EXPRESSO.volta.flag = ifelse(is.na(match(DESTINO1_STOP, EXPRESSO.volta$local) > match(ORIGEM1_STOP, EXPRESSO.volta$local)),
                              FALSE,
                              match(DESTINO1_STOP, EXPRESSO.volta$local) > match(ORIGEM1_STOP, EXPRESSO.volta$local)),
  B1.ida.flag = ifelse(is.na(match(DESTINO1_STOP, B1.ida$local) > match(ORIGEM1_STOP, B1.ida$local)),
                              FALSE,
                              match(DESTINO1_STOP, B1.ida$local) > match(ORIGEM1_STOP, B1.ida$local)),
  B1.volta.flag = ifelse(is.na(match(DESTINO1_STOP, B1.volta$local) > match(ORIGEM1_STOP, B1.volta$local)),
                                FALSE,
                                match(DESTINO1_STOP, B1.volta$local) > match(ORIGEM1_STOP, B1.volta$local)),
  B2.ida.flag = ifelse(is.na(match(DESTINO1_STOP, B2.ida$local) > match(ORIGEM1_STOP, B2.ida$local)),
                              FALSE,
                              match(DESTINO1_STOP, B2.ida$local) > match(ORIGEM1_STOP, B2.ida$local)),
  B2.volta.flag = ifelse(is.na(match(DESTINO1_STOP, B2.volta$local) > match(ORIGEM1_STOP, B2.volta$local)),
                                FALSE,
                                match(DESTINO1_STOP, B2.volta$local) > match(ORIGEM1_STOP, B2.volta$local)),

  B3.ida.flag = ifelse(is.na(match(DESTINO1_STOP, B3.ida$local) > match(ORIGEM1_STOP, B3.ida$local)),
                              FALSE,
                              match(DESTINO1_STOP, B3.ida$local) > match(ORIGEM1_STOP, B3.ida$local)),
  
  B3.volta.flag = ifelse(is.na(match(DESTINO1_STOP, B3.volta$local) > match(ORIGEM1_STOP, B3.volta$local)),
                                FALSE,
                                match(DESTINO1_STOP, B3.volta$local) > match(ORIGEM1_STOP, B3.volta$local)),
  
  B4.ida.flag = ifelse(is.na(match(DESTINO1_STOP, B4.ida$local) > match(ORIGEM1_STOP, B4.ida$local)),
                              FALSE,
                              match(DESTINO1_STOP, B4.ida$local) > match(ORIGEM1_STOP, B4.ida$local)),
  B4.volta.flag = ifelse(is.na(match(DESTINO1_STOP, B4.volta$local) > match(ORIGEM1_STOP, B4.volta$local)),
                                FALSE,
                                match(DESTINO1_STOP, B4.volta$local) > match(ORIGEM1_STOP, B4.volta$local)),
  B5.ida.flag = ifelse(is.na(match(DESTINO1_STOP, B5.ida$local) > match(ORIGEM1_STOP, B5.ida$local)),
                              FALSE,
                              match(DESTINO1_STOP, B5.ida$local) > match(ORIGEM1_STOP, B5.ida$local)),
  
  B5.volta.flag = ifelse(is.na(match(DESTINO1_STOP, B5.volta$local) > match(ORIGEM1_STOP, B5.volta$local)),
                                FALSE,
                                match(DESTINO1_STOP, B5.volta$local) > match(ORIGEM1_STOP, B5.volta$local))) %>%
  mutate(
    EXPRESSO.ida.dist = ifelse(EXPRESSO.ida.flag == 0, 1/EXPRESSO.ida.flag,
                                sum(EXPRESSO.ida$dist[c(match(ORIGEM1_STOP, EXPRESSO.ida$local):match(DESTINO1_STOP, EXPRESSO.ida$local))])),
    EXPRESSO.volta.dist = ifelse(EXPRESSO.volta.flag == 0, 1/EXPRESSO.volta.flag,
                                sum(EXPRESSO.volta$dist[c(match(ORIGEM1_STOP, EXPRESSO.volta$local):match(DESTINO1_STOP, EXPRESSO.volta$local))])),
    B1.ida.dist = ifelse(B1.ida.flag == 0, 1/B1.ida.flag,
                                sum(B1.ida$dist[c(match(ORIGEM1_STOP, B1.ida$local):match(DESTINO1_STOP, B1.ida$local))])),
    B1.volta.dist = ifelse(B1.volta.flag == 0, 1/B1.volta.flag,
                                  sum(B1.volta$dist[c(match(ORIGEM1_STOP, B1.volta$local):match(DESTINO1_STOP, B1.volta$local))])),
    B2.ida.dist = ifelse(B2.ida.flag == 0, 1/B2.ida.flag,
                                sum(B2.ida$dist[c(match(ORIGEM1_STOP, B2.ida$local):match(DESTINO1_STOP, B2.ida$local))])),
    B2.volta.dist = ifelse(B2.volta.flag == 0, 1/B2.volta.flag,
                                  sum(B2.volta$dist[c(match(ORIGEM1_STOP, B2.volta$local):match(DESTINO1_STOP, B2.volta$local))])),
    B3.ida.dist = ifelse(B3.ida.flag == 0, 1/B3.ida.flag,
                                sum(B3.ida$dist[c(match(ORIGEM1_STOP, B3.ida$local):match(DESTINO1_STOP, B3.ida$local))])),
    B3.volta.dist = ifelse(B3.volta.flag == 0, 1/B3.volta.flag,
                                  sum(B3.volta$dist[c(match(ORIGEM1_STOP, B3.volta$local):match(DESTINO1_STOP, B3.volta$local))])),
    B4.ida.dist = ifelse(B4.ida.flag == 0, 1/B4.ida.flag,
                                sum(B4.ida$dist[c(match(ORIGEM1_STOP, B4.ida$local):match(DESTINO1_STOP, B4.ida$local))])),
    B4.volta.dist = ifelse(B4.volta.flag == 0, 1/B4.volta.flag,
                                  sum(B4.volta$dist[c(match(ORIGEM1_STOP, B4.volta$local):match(DESTINO1_STOP, B4.volta$local))])),
    B5.ida.dist = ifelse(B5.ida.flag == 0, 1/B5.ida.flag,
                                sum(B5.ida$dist[c(match(ORIGEM1_STOP, B5.ida$local):match(DESTINO1_STOP, B5.ida$local))])),
    B5.volta.dist = ifelse(B5.volta.flag == 0, 1/B5.volta.flag,
                                  sum(B5.volta$dist[c(match(ORIGEM1_STOP, B5.volta$local):match(DESTINO1_STOP, B5.volta$local))]))) %>%
  
  select(contains("flag"), contains("dist")) %>%
  mutate(bus.id = NA, bus.dist = NA)

## Seleciona ou sorteia a rota mais curta via Buzufba que pode ser usada pelo usuario
set.seed(42) ### Define a semente de geracao de numeros aleatorios
opcoes.dists <- select(opcoes, contains("a.dist"))
nomes <- colnames(opcoes.dists) ### Guarda o nome dos roteiros Buzufba
nomes <- gsub(".dist", "", nomes) ### Retira o '.dist' do nome das rotas

for (i in 1:dim(opcoes)[1]) {
  ### Obtem todos os roteiro com distancia minima e sorteia um deles de maneira uniforme
  id <- as.numeric(sample(as.character(which(min_rank(unlist(opcoes.dists[i,])) %in% 1)), 1))
  
  opcoes$bus.id[i] <- nomes[id] ### Retem a id do roteiro
  opcoes$bus.dist[i] <- opcoes.dists[i, id] ### Retem a distancia percorrida dentro do roteiro
}
opcoes$bus.dist <- as.numeric(opcoes$bus.dist)

## Gera a matriz de variaveis preditoras
deslocamento <- dados %>% select(FREQ1, ORIGEM1, DESTINO1, ORIGEM1_STOP, DESTINO1_STOP, S_SATS) %>%
  mutate(bus.id = opcoes$bus.id, trecho.bus = opcoes$bus.dist) %>% 
  filter_all(all_vars(!is.na(.))) %>% filter(trecho.bus != Inf) %>% 
  mutate(trecho.pe = locais[match(ORIGEM1, locais$ORIGEM.DESTINO), "dist.met"] +
           locais[match(DESTINO1, locais$ORIGEM.DESTINO), "dist.met"]) %>% 
  mutate(dist.pe = distGeo(locais[match(ORIGEM1, locais$ORIGEM.DESTINO), c("lat", "long")],
                           locais[match(DESTINO1, locais$ORIGEM.DESTINO), c("lat", "long")])) %>% 
  mutate(trecho.pe = trecho.pe/1000, dist.pe = dist.pe/1000) %>% 
  mutate(fracao.pe = trecho.pe/(trecho.pe + trecho.bus), alternativa = dist.pe/(trecho.pe + trecho.bus))

## Inicializa um dataframe para guardar as ligações entre paradas
edges <- data.frame(from = "Init", to = "Init", S_SATS = "CC", stringsAsFactors = FALSE)

## Preenche o dataframe de conexões (edges)
for (i in 1:dim(deslocamento)[1]) {
  ### Capta a lista de locais pelos quais o Buzufba que o usuário pegou passa
  varname <- get(deslocamento$bus.id[i])$local
  ### Mantém somente os locais pelos quais o usuário passou (de parada de origem a destino)
  varname <- varname[match(deslocamento$ORIGEM1_STOP[i], varname):match(deslocamento$DESTINO1_STOP[i], varname)]
  ### Pega os pares de locais e insere no dataframe de conexões (edges)
  for (j in 1:(length(varname)-1)) {
    edges <- rbind(edges, c(as.character(varname[j:(j+1)]), as.character(deslocamento$S_SATS[i])))
  }
}

## Elimina a linha de preenchimento inicial ("Init")
edges <- edges[-1,]

## Converte a satisfação em numérico com certa ordem
edges <- mutate(edges, S_SATS = as.numeric(fct_relevel(factor(S_SATS), "DC", "DP", "CP", "CC")))

## Calcula a qtd de conexões entre as paradas (soma e une conexões repetidas)
edges <- edges %>% group_by(from, to) %>% summarise(value = n(), S_SATS = median(S_SATS)) %>% ungroup()

### Gera o vetor de cores das conexões em função da satisfação global e insere em edges
mycolor <- data.frame(S_SATS = seq(1, 4, 0.5))
mycolor <- mutate(mycolor, color = brewer.pal(n = dim(mycolor)[1], name = "RdYlGn"))
edges <- left_join(edges, mycolor)
mycolor <- mycolor %>% rename(label = S_SATS) %>% mutate(font.align = "top")

## Gera o vetor de nós e acrescenta a qtd de pessoas com origem ou destino nos nós
nodes <- data.frame(id = stops$locals) %>% rowid_to_column(var = "label")
aux <- group_by(deslocamento, ORIGEM1_STOP) %>% summarise(origin = n())
count <- group_by(deslocamento, DESTINO1_STOP) %>% summarise(destin = n()) %>%
  full_join(aux, by = c("DESTINO1_STOP" = "ORIGEM1_STOP")) %>%
  mutate(origin = ifelse(is.na(origin), 0, origin), destin = ifelse(is.na(destin), 0, destin)) %>% 
  rename(id = "DESTINO1_STOP")
nodes <- nodes %>% left_join(count) %>% 
  mutate(origin = ifelse(is.na(origin), 0, origin), destin = ifelse(is.na(destin), 0, destin))

## Duplica os pontos para gerar pares que indicam a qtd de pessoas que os tomam como origem e destino
nodes <- nodes %>%  rbind(., nodes) %>% rowid_to_column(var = "pos") %>% mutate(value = 0) %>% 
  mutate_cond(pos <= 25, value = destin, color = "green") %>%
  mutate_cond(pos > 25, value = origin, color = "red")

## Armazena as coordenadas dos locais e impoe onde os pontos serao plotados
coords <- nodes %>% left_join(stops, by = c("id" = "locals")) %>%
  rename(x = long, y = lat) %>% select(x, y) %>% mutate(y = -y) %>% as.matrix()
## Desloca os pontos levemente para melhorar a visualizacao
coords[c(1,1+25), 2] <- coords[c(1,1+25), 2]*1.00004 ### Estacionamento PAF I
coords[c(2,2+25), 2] <- coords[c(2,2+25), 2]*1.00004 ### R5: Garibaldi
coords[c(3,3+25), 2] <- coords[c(3,3+25), 2]*(1-0.00003) ### Arquitetura
coords[c(5,5+25), 1] <- coords[c(5,5+25), 1]*1.00001 ### Creche Canela
coords[c(5,5+25), 2] <- coords[c(5,5+25), 2]*(1-0.00004) ### Creche Canela
coords[c(6,6+25), 1] <- coords[c(6,6+25), 1]*(1-0.00001) ### Reitoria
coords[c(6,6+25), 2] <- coords[c(6,6+25), 2]*(1-0.00003) ### Reitoria
coords[c(7,7+25), 1] <- coords[c(7,7+25), 1]*1.00005 ### Geociencias
coords[c(7,7+25), 2] <- coords[c(7,7+25), 2]*1.00004 ### Geociencias
coords[c(11,11+25), 2] <- coords[c(11,11+25), 2]*(1-0.00010) ### R1: Corredor da Vitoria
coords[c(13,13+25), 2] <- coords[c(13,13+25), 2]*1.00002 ### R3: Deli e Cia
coords[c(16,16+25), 1] <- coords[c(16,16+25), 1]*(1-0.00002) ### Belas-Artes
coords[c(16,16+25), 2] <- coords[c(16,16+25), 2]*(1-0.00004) ### Belas-Artes
coords[c(17,17+25), 1] <- coords[c(17,17+25), 1]*1.00003 ### Odontologia
coords[c(17,17+25), 2] <- coords[c(17,17+25), 2]*1.00003 ### Odontologia
coords[c(24,24+25), 2] <- coords[c(24,24+25), 2]*(1-0.00005) ### Reitor Miguel Calmon

## Organiza os nós para que os pares sejam visualisados
## Na parte superior os pontos representam quantas pessoas se destinam ao ponto (destin)
## Na parte inferior os pontos representam quantas pessoas partiram do ponto (origin)
nodes <- nodes %>%
  mutate_cond((pos <= 25) & (destin >= origin), label = id, id = pos) %>% 
  mutate_cond((pos <= 25) & (destin < origin), label = "") %>% 
  mutate_cond((pos > 25) & (destin < origin), label = id, id = pos) %>%
  mutate_cond((pos > 25) & (destin >= origin), label = "") %>%
  select(label, id, value, color)

## Configuracoes da rede
opts <- . %>% visOptions(highlightNearest = TRUE) %>%
  visInteraction(dragNodes = FALSE) %>%
  visEdges(arrows = 'to', arrowStrikethrough = FALSE, 
           shadow = list(enabled = TRUE, size = 5),
           scaling = list(min = 5, max = 20),
           smooth = list(enabled = TRUE, type = "dynamic")) %>% 
  visNodes(scaling = list(min = 5, max = 70), font = list(size = 30))

## Plota a rede
visNetwork(nodes, edges) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = coords) %>% 
  opts

crs <- CRS("+proj=longlat +datum=WGS84")
coords[,2] <- -coords[,2]
spdf <- SpatialPointsDataFrame(coords = coords,
                               data = nodes, 
                               proj4string = crs)
poly.net <- od2line(edges, spdf)

#visLegend(addEdges = mycolor, main = "Mediana da satisfação global no trecho",
#          useGroups = FALSE, zoom = FALSE, width = 0.2, ncol = 2) %>%

#####################################################################################################
# Etapa 02: Criando um ìndice para variáveis resposta

dados <- dados %>% 
  mutate(I_PONT = fct_relevel(factor(I_PONT), "ND IMP", "PC IMP", "IMP", "MT IMP")) %>% 
  mutate(I_FREQ = fct_relevel(factor(I_FREQ), "ND IMP", "PC IMP", "IMP", "MT IMP")) %>% 
  mutate(I_ACES = fct_relevel(factor(I_ACES), "ND IMP", "PC IMP", "IMP", "MT IMP")) %>% 
  mutate(I_CONF = fct_relevel(factor(I_CONF), "ND IMP", "PC IMP", "IMP", "MT IMP")) %>% 
  mutate(I_ESPR = fct_relevel(factor(I_ESPR), "ND IMP", "PC IMP", "IMP", "MT IMP")) %>% 
  mutate(I_INFO = fct_relevel(factor(I_INFO), "ND IMP", "PC IMP", "IMP", "MT IMP")) %>% 
  mutate(I_TRAT = fct_relevel(factor(I_TRAT), "ND IMP", "PC IMP", "IMP", "MT IMP")) %>% 
  mutate(I_TRAN = fct_relevel(factor(I_TRAN), "ND IMP", "PC IMP", "IMP", "MT IMP")) %>%
  mutate(I_VIOL = fct_relevel(factor(I_VIOL), "ND IMP", "PC IMP", "IMP", "MT IMP")) %>% 
  mutate(S_PONT = fct_relevel(factor(S_PONT), "DC", "DP", "CP", "CC")) %>% 
  mutate(S_FREQ = fct_relevel(factor(S_FREQ), "DC", "DP", "CP", "CC")) %>% 
  mutate(S_ACES = fct_relevel(factor(S_ACES), "DC", "DP", "CP", "CC")) %>% 
  mutate(S_CONF = fct_relevel(factor(S_CONF), "DC", "DP", "CP", "CC")) %>% 
  mutate(S_ESPR = fct_relevel(factor(S_ESPR), "DC", "DP", "CP", "CC")) %>% 
  mutate(S_INFO = fct_relevel(factor(S_INFO), "DC", "DP", "CP", "CC")) %>% 
  mutate(S_TRAT = fct_relevel(factor(S_TRAT), "DC", "DP", "CP", "CC")) %>% 
  mutate(S_TRAN = fct_relevel(factor(S_TRAN), "DC", "DP", "CP", "CC")) %>%
  mutate(S_VIOL = fct_relevel(factor(S_VIOL), "DC", "DP", "CP", "CC")) %>% 
  mutate(S_SATS = fct_relevel(factor(S_SATS), "DC", "DP", "CP", "CC")) %>% 
  mutate(FREQ_20172 = fct_relevel(factor(FREQ_20172), "1-4 Semestre", "1-4 Mês", "1-4 Semana",
                                  "5-9 Semana", "10-M Semana")) %>% 
  mutate(FREQ1 = fct_relevel(factor(FREQ1), "1-4 Semestre", "1-4 Mês", "1-4 Semana",
                             "5-9 Semana", "10-M Semana")) %>% 
  mutate(FREQ2 = fct_relevel(factor(FREQ2), "1-4 Semestre", "1-4 Mês", "1-4 Semana",
                             "5-9 Semana", "10-M Semana")) %>% 
  mutate(FREQ3 = fct_relevel(factor(FREQ3), "1-4 Semestre", "1-4 Mês", "1-4 Semana",
                             "5-9 Semana", "10-M Semana"))

#####################################################################################################
# Etapa 01: Carregamento e adequação de bibliotecas e base de dados  

#####################################################################################################
# Etapa 02: Correlação entre frequencia e indice


# save(dados, deslocamento, file = "buzufba.RData")
