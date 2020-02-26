#--- Assunto: Leitura dos dados (PNAD)
#--- Autor: Rafael Barbosa
#--- Data: 18/07/2019


if(!require(tidyverse)) {
  install.packages("tidyverse", dependencies = T);
  require(tidyverse)
}


if(!require(data.table)) {
  install.packages("data.table", dependencies = T);
  require(data.table)
}

library(bit64)
library(data.table)
library(descr)
library(xlsx)


setwd("F:/UFPA/Trabalhos/PNAD - Lidia")


dicdom <- fread(file = "dicdom.csv")


dicdom <-
  dicdom %>%
  set_names(c("inicio", "tamanho", "variavel"))



end_dom = dicdom$inicio + dicdom$tamanho - 1



fwf2csv(fwffile='Dados/PES2015.txt', csvfile='dadosdom.csv', names=dicdom$variavel, begin=dicdom$inicio, end=end_dom)


## Efetua a leitura do conjunto de dados com o fread do data.table
dadosdom <- fread(input='dadosdom.csv', sep='auto', sep2='auto', integer64='double')


dadosdom <- fread(file = "dadosdom.csv")


dadosdom <-
  dadosdom %>%
  mutate_all(~replace(., is.na(.), "Não aplicável"))

dadosdom %>%
  readr::write_csv(x = ., path = "PNAD_PES.csv", na = " ")


