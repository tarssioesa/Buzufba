## Calcula a correlação entre todas as variáveis de Importância e Concordância e verifica significância

#####################################################################################################
# Etapa 02: Correlação entre deslocamento e respostas


require(caret)
require(forcats)
require(tidyverse)
require(likert)


# opt (GRAFICO)

opt <- theme_bw()+
  theme(axis.title = element_text(face = "bold", color = "black", size = 20),
        axis.text.x = element_text(face = "plain", color = "black", 
                                   size = 18, angle = 90),
        axis.text.y = element_text(face = "plain", color = "black", size = 18),
        legend.text=element_text(size=20, face = "bold"),
        legend.title = element_text(size = 20, face = "bold"),
        strip.text.x = element_text(size = 18, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"))

# Para S 

load("Data/buzufba.RData")

summary(dados$S_SATS)

### Para S_SATS ----

breaks <- seq(0,10, by = 2)

desloc <- deslocamento %>% 
  select(c(8:10,12,6)) %>% 
  mutate(trecho.bus = cut(deslocamento$trecho.bus, breaks = breaks, label = FALSE)) %>% 
  mutate(dist.pe = cut(deslocamento$dist.pe, breaks = breaks, label = FALSE)) %>% 
  mutate(alternativa = cut(deslocamento$alternativa, breaks = breaks, label = FALSE)) %>% 
  mutate(trecho.pe = cut(deslocamento$trecho.pe, breaks = breaks, label = FALSE))


y <- dados %>% 
  select(c(51,26:44,15), id) %>% 
  na.omit() %>% 
  mutate(MOTIVO1 = fct_lump(MOTIVO1, n = 5)) %>% 
  mutate(MOTIVO1 = fct_collapse(MOTIVO1, 
                                other = c("Other", "Outro"))) %>% 
  mutate(S_SATS = fct_collapse(S_SATS, 
                               Ruim = "DC", 
                               Ruim = "DP", 
                               Bom = "CP", 
                               Bom = "CC"))

y$MOTIVO1 <- reverse.levels(y$MOTIVO1)

x <- y %>% 
  left_join(deslocamento) %>% 
  select(c(1:21)) %>% 
  mutate_if(is.factor, as.numeric) %>% 
  filter(S_SATS < 5) %>% 
  left_join(desloc) %>% 
  na.omit() 
  

# Arvore de Scores

set.seed(12)

# Training e Testing

trainIndex <- createDataPartition(x$S_SATS, p = .7, 
                                  list = FALSE, 
                                  times = 1)

Train <- x[trainIndex,] %>% 
  mutate_all(ordered) %>% 
  select(-c(trecho.bus))

Test  <- x[-trainIndex,] %>% 
  mutate_all(ordered)

# Separando os treinos e testes: 

Trainx <- Train[,-c(11)]

Trainy <- Train[,c(11)]

Testx <- Test[,-c(11)]

Testy <- Test[,c(11)]

#Crossvalidation

cctrl1 <- trainControl(method="repeatedcv", number= 5, repeats=5)



# Score

cv_model <- train(x = Trainx[,-c(1)], y = Trainy, 
                             method = "rpartScore", 
                             trControl = cctrl1,
                             metric = "Kappa")


cv_model

test_weight <- varImp(cv_model)

plot(test_weight)
 
# # predição 
# 

predictions <- predict(cv_model, Testx)

# 
# # summarize results
# 

confusionMatrix(predictions, Testy)


### Lime 
