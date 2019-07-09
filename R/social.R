
# Library

library(tidyverse)
require(inspectdf)


# Dados

load("Data/buzufba.RData")

# Gráfico 

social <- dados %>% 
  select(2,3,5,11) %>% 
  mutate(id = factor(seq(1,nrow(dados)))) %>% 
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
                                  "Mensal", "Semanal", "Diária" )) %>% 
  select(-id)


inspect_cat(social, show_plot = TRUE)
