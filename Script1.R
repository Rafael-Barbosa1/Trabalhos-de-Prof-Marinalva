#--- Assunto: Gráfico - VOCs (Marilene)
#--- Autor: Rafael Barbosa
#--- Data: 09/09/2019


#--- Limpando o banco:

rm(list = ls())


#--- Pacotes


if(!require(tidyverse)) {
  install.packages("tidyverse", dependencies = T);
  require(tidyverse)
}


#--- Diretório


setwd("F:/UFPA/Trabalhos/VOCs - Marilene")


#--- Tirando os valores de exponenciação

options(scipen = 999)


#--- Formatando em BR


formato_real_graf <- function(values, nsmall = 0) { # ::: Formatando o valor como moeda
  values %>%
    as.numeric() %>%
    format(nsmall = nsmall, decimal.mark = ",", big.mark = ".") %>%
    str_trim()
}


#--- Gráfico


readxl::read_xlsx(path = "Tabela VOCs Marilene.xlsx", sheet = 1) %>%
  select(Matrix, `Variável`, Valor) %>%
  magrittr::set_colnames(c("Matriz", "Matriz1", "Valor")) %>%
  mutate(Matriz = factor(x = Matriz, levels = c("M9", "M7", "M5", "M4", "M8",
                                                "M6", "M3", "M1", "M2", "M10"),
                         ordered = T)) %>%
  mutate(Matriz1 = factor(x = Matriz1, levels = c("P1", "P2", "P5", "P6", "P21"))) %>%
  ggplot(data = .) +
  geom_bar(aes(x = Matriz, y = Valor, group = Matriz1, fill = Matriz1),
           stat = "identity", color = "black", width = 0.75) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = formato_real_graf) +
  labs(x = "Matrix", y = "Value", fill = "Peak's area") +
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
  guides(fill = guide_legend(reverse = TRUE))
