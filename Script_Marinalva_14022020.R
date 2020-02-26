#-------------------------

#--- Nome: Script_Marinalva_14022020.R


#--- Objetivo: Criação de um gráfico


#--- Autor: Rafael Barbosa da Silva
#--- e-mail: lul.rafaelbarbosa@gmail.com


#--- Data de criação: 14/02/2020

#-------------------------


#--- Limpando o ambiente


rm(list = ls())


#--- Setando o diretório do banco de dados


setwd("F:/UFPA/Trabalhos/Marinalva_14022020")


#--- Pacotes usados


if(!require(tidyverse)) {
  install.packages("tidyverse", dependencies = T);
  require(tidyverse)
}


if(!require(foreign)) {
  install.packages("foreign", dependencies = T);
  require(foreign)
}


#--- Valores em formato BR


formato_real_graf <- function(values, nsmall = 0) { #- Formatando o valor como moeda
  values %>%
    as.numeric() %>%
    format(nsmall = nsmall, decimal.mark = ",", big.mark = ".") %>%
    str_trim()
}


#--- Carregando os banco de dados


banco <- read.spss(file = "obito05.sav", to.data.frame = T)


#--- Verificando a estrutura dos dados

banco %>% glimpse


#---


municipios <-
  banco %>%
  drop_na %>%
  filter(ANO_CMPT %in% c(2008, 2017)) %>%
  mutate(MESO1 = case_when(MESO == "Baixo Amazonas" ~ "Lower Amazon",
                           MESO == "Marajó" ~ "Marajó",
                           MESO == "Metropolitana de Belém" ~ "Metropolitan of Belém",
                           MESO == "Nordeste Paraense" ~ "Northeast of Pará",
                           MESO == "Sudeste Paraense" ~ "Southeast of Pará",
                           MESO == "Sudoeste Paraense" ~ "Southwest of Pará"
  ),
  NOME_MUNIC = str_trim(string = NOME_MUNIC, side = "right")) %>%
  select(MESO1, ANO_CMPT, NOME_MUNIC, Txmort) %>%
  group_by(MESO1, ANO_CMPT, NOME_MUNIC) %>%
  summarise(min1 = min(Txmort),
            med1 = mean(Txmort),
            max1 = max(Txmort)) %>%
  reshape2::melt(1:3) %>%
  ungroup %>%
  mutate(value = round(x = value, digits = 5))



banco1 <-
  banco %>%
  drop_na %>%
  filter(ANO_CMPT %in% c(2008, 2017)) %>%
  mutate(MESO1 = case_when(MESO == "Baixo Amazonas" ~ "Lower Amazon",
                           MESO == "Marajó" ~ "Marajó",
                           MESO == "Metropolitana de Belém" ~ "Metropolitan of Belém",
                           MESO == "Nordeste Paraense" ~ "Northeast of Pará",
                           MESO == "Sudeste Paraense" ~ "Southeast of Pará",
                           MESO == "Sudoeste Paraense" ~ "Southwest of Pará"
  ),
  NOME_MUNIC = str_trim(string = NOME_MUNIC, side = "right")) %>%
  group_by(MESO1, ANO_CMPT) %>%
  summarise(min1 = min(Txmort),
            med1 = mean(Txmort),
            max1 = max(Txmort)) %>%
  reshape2::melt(1:2) %>%
  ungroup %>%
  mutate(value = round(x = value, digits = 5))





grafico1 <- banco1 %>%
  left_join(y = municipios) %>%
  mutate(variable1 = case_when(variable == "min1" ~ "Lowest regional mortality rate",
                               variable == "med1" ~ "Mean regional mortality rate",
                               variable == "max1" ~ "Highest regional mortality rate"),
         NOME_MUNIC = ifelse(NOME_MUNIC == "Santa Bárbara do Pará",
                             "Sta. Bárbara", NOME_MUNIC),
         NOME_MUNIC = ifelse(NOME_MUNIC == "Santo Antônio do Tauá",
                             "Sto. Antônio do Tauá", NOME_MUNIC)) %>%

  ggplot(data = .) +
  geom_point(aes(x = MESO1, y = value, color = variable1, shape = variable1),
             size = 6) +
  facet_wrap(~ ANO_CMPT, nrow = 2) +
  geom_text(aes(x = MESO1, y = value, label = NOME_MUNIC),
            hjust = 0, nudge_x = 0.08, size = 4.5) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(colour = "black", face = "bold", size = 14),
        axis.title.x = element_text(colour = "black", face = "bold", size = 14),
        axis.text = element_text(colour = "black", size = 14),
        strip.text.x = element_text(size = 12, colour = "black", face = "bold"),
        strip.text.y = element_text(size = 12, colour = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color = "black"),
        axis.line = element_line(size = 0.5, colour = "black")) +
  labs(x = "Mesoregion", y = "Mortality rate")  +
  scale_y_continuous(limits = c(0, 60)) +
  scale_shape_manual(values = c(16, 16, 18))





#--- Salvando o gráfico

# 1. PDF


ggsave(filename = "Grafico_final1.pdf", plot = grafico1,
       device = "pdf", width = 16, height = 9, dpi = 300)


# 2. TIFF


ggsave(filename = "Grafico_final1.tiff", plot = grafico1,
       device = "tiff", width = 16, height = 9, dpi = 300,
       compression = "lzw")


# 3. JPEG


ggsave(filename = "Grafico_final1.jpeg", plot = grafico1,
       device = "jpeg", width = 16, height = 9, dpi = 300)

