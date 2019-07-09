# Librarys

require(caret)
require(forcats)
require(tidyverse)
require(likert)
require(reshape2)
library(reshape2)
library(ggrepel)
library('scales')

# Data

load("Data/buzufba.RData")

# opt (GRAFICO)

opt <- theme_bw()+
  theme(axis.title = element_text(face = "bold", color = "black", size = 16),
        axis.text.x = element_text(face = "plain", color = "black", 
                                   size = 16, angle = 90),
        axis.text.y = element_text(face = "plain", color = "black", size = 16),
        legend.text=element_text(size=16, face = "bold"),
        legend.title = element_text(size = 16, face = "bold"),
        strip.text.x = element_text(size = 16, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"))

# Profile

aux2 <- dados %>% 
  select(2,3,5,11) %>% 
  mutate(id = factor(seq(1,1657))) %>% 
  mutate(RENDA = fct_collapse(RENDA,
                  "0-3 SM" = c("0-1 SM","1-3 SM"),
                  "3-6 SM" = c("3-6 SM"),
                  "6-12 SM" = c("6-9 SM","9-12 SM"),
                  "12+ SM" = c("12-M SM"))) %>% 
  mutate(VINC = fct_collapse(VINC,
         "Pos Grad" = c("Mest", "Dout"))) %>% 
  filter(FREQ_20172 != "Nenhuma") %>% 
  mutate(FREQ_20172 = fct_collapse(FREQ_20172,
         'Diária' = c("5-9 Semana", "10-M Semana"),
         "Semanal" = c("1-4 Semana"), 
         "Mensal" = c("1-4 Mês"),
         "Raramente" = c("1-4 Semestre"))) %>% 
  mutate(RENDA = fct_relevel(RENDA,"12+ SM", "6-12 SM",
                             "3-6 SM", "0-3 SM" )) %>% 
  mutate(VINC = fct_relevel(VINC, "Outro",  "STA", "Prof" , 
                            "Pos Grad", "Grad")) %>% 
  mutate(FREQ_20172 = fct_relevel(FREQ_20172, "Nenhuma", "Raramente" ,
                                  "Mensal", "Semanal", "Diária" ))
 
 


 ####################################################################333
 
 ### BOXPLOTS
 
desloc <- deslocamento %>% 
   select(8:10) %>% 
   mutate(id = as.factor(seq(1:nrow(deslocamento)))) %>% 
   melt()

p1 <- ggplot(desloc, aes(x = variable, y = value, color = I("blue"))) +
   geom_boxplot(outlier.colour = "green",
                outlier.shape = 8, outlier.size = 3) +
   labs(y = "Distância (km)", x = "") + 
  scale_y_continuous(breaks = c(seq(0,10, by = 0.5))) +
   opt

p2 <- ggplot(desloc, aes(x = value, color = I("blue"))) +
  geom_histogram(bins = 60) +
  facet_grid(variable~.) +
  labs(x = "Distância (km)", y = "") +
  scale_x_continuous(breaks = c(seq(0,10, by = 0.5))) +
  opt


gridExtra::grid.arrange(p1,p2, ncol = 2)



 
 
