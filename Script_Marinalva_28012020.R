#-------------------------

#--- Nome: Script_Marinalva_28012020.R


#--- Objetivo: Criação de um gráfico


#--- Autor: Rafael Barbosa da Silva
#--- e-mail: lul.rafaelbarbosa@gmail.com


#--- Data de criação: 28/01/2020

#-------------------------


#--- Limpando o ambiente


rm(list = ls())


#--- Setando o diretório do banco de dados


setwd("F:/UFPA/Trabalhos/Marinalva_28012020")


#--- Pacotes usados


if(!require(tidyverse)) {
  install.packages("tidyverse", dependencies = T);
  require(tidyverse)
}


if(!require(openxlsx)) {
  install.packages("openxlsx", dependencies = T);
  require(openxlsx)
}


#--- Carregando os banco de dados


banco <- read.xlsx(xlsxFile = "Marinalva_28012020.xlsx", sheet = "BD_Ajust")


#--- Verificar a estrutura dos dados


banco %>% glimpse


#--- Gráfico 2006


grafico_2006 <-
  banco %>%
  filter(Ano == 2006) %>%
  ggplot(data = .) +
  geom_bar(aes(x = UF, y = Porcentagem, fill = `Migração`),
           position = "stack", stat = "identity", width = .7,
           color = "black") +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(colour = "black", face = "bold", size = 14),
        axis.title.x = element_text(colour = "black", face = "bold", size = 14),
        axis.text = element_text(colour = "black", size = 16),
        strip.text.x = element_text(size = 12, colour = "white"),
        strip.text.y = element_text(size = 12, colour = "white"),
        legend.title = element_text(size = 16, color = "black", face = "bold"),
        legend.text = element_text(size = 16, color = "black"),
        axis.line = element_line(size = 0.5, colour = "black")) +
  labs(x = "", y = "Percentual", fill = "") +
  scale_fill_brewer(palette = "Blues")


grafico_2006


#--- Gráfico 2015


grafico_2015 <-
  banco %>%
  filter(Ano == 2015) %>%
  ggplot(data = .) +
  geom_bar(aes(x = UF, y = Porcentagem, fill = `Migração`),
           position = "stack", stat = "identity", width = .7,
           color = "black") +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(colour = "black", face = "bold", size = 14),
        axis.title.x = element_text(colour = "black", face = "bold", size = 14),
        axis.text = element_text(colour = "black", size = 16),
        strip.text.x = element_text(size = 12, colour = "white"),
        strip.text.y = element_text(size = 12, colour = "white"),
        legend.title = element_text(size = 16, color = "black", face = "bold"),
        legend.text = element_text(size = 16, color = "black"),
        axis.line = element_line(size = 0.5, colour = "black")) +
  labs(x = "", y = "Percentual", fill = "") +
  scale_fill_brewer(palette = "Blues")


grafico_2015


#--- Gráfico 2006 e 2015


banco$Ano <- factor(x = banco$Ano, levels = c(2015, 2006))


grafico_todos_anos <-
  banco %>%
  ggplot(data = .) +
  geom_bar(aes(x = Ano, y = Porcentagem, fill = `Migração`),
           position = "stack", stat = "identity", width = .7,
           color = "black") +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(colour = "black", face = "bold", size = 14),
        axis.title.x = element_text(colour = "black", face = "bold", size = 14),
        axis.text = element_text(colour = "black", size = 16),
        strip.text.x = element_text(size = 14, colour = "black", face = "bold"),
        strip.text.y = element_text(size = 12, colour = "black"),
        legend.title = element_text(size = 16, color = "black", face = "bold"),
        legend.text = element_text(size = 16, color = "black"),
        axis.line = element_line(size = 0.5, colour = "black"),
        strip.background = element_blank()) +
  labs(x = "", y = "Percentual", fill = "") +
  scale_fill_brewer(palette = "Blues") +
  facet_wrap(~ UF)



grafico_todos_anos1 <-
  banco %>%
  ggplot(data = .) +
  geom_bar(aes(x = UF, y = Porcentagem, fill = `Migração`),
           position = "stack", stat = "identity", width = .7,
           color = "black") +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(colour = "black", face = "bold", size = 14),
        axis.title.x = element_text(colour = "black", face = "bold", size = 14),
        axis.text = element_text(colour = "black", size = 16),
        strip.text.x = element_text(size = 14, colour = "black", face = "bold"),
        strip.text.y = element_text(size = 12, colour = "black"),
        legend.title = element_text(size = 16, color = "black", face = "bold"),
        legend.text = element_text(size = 16, color = "black"),
        axis.line = element_line(size = 0.5, colour = "black"),
        strip.background = element_blank()) +
  labs(x = "", y = "Percentual", fill = "") +
  scale_fill_brewer(palette = "Blues") +
  facet_wrap(~ Ano)



#--- Salvando o gráfico

# 1. PDF


ggsave(filename = "Grafico_todos_anos.pdf", plot = grafico_todos_anos,
       device = "pdf", width = 10, height = 6, dpi = 300)


# 2. JPEG

ggsave(filename = "Grafico_todos_anos.jpeg", plot = grafico_todos_anos,
       device = "jpeg", width = 10, height = 6, dpi = 300)
