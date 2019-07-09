#Library 

require(tidyverse)
require(likert)

# Data

load("Data/buzufba.RData")

# Importancia

imp <- dados[, 36:44] %>% 
  na.omit() %>%
  mutate_all(all_vars(fct_relevel(., "ND IMP", "PC IMP", "IMP", "MT IMP"))) %>% 
  likert()

plot(imp)


# Satisfação


sats <- dados[, 26:35] %>% 
  na.omit() %>%
  mutate_all(all_vars(fct_relevel(
    ., "DC", "DP", "CP", "CC"))) %>% 
  likert()

plot(sats)
