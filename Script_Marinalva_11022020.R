#-------------------------

#--- Nome: Script_Marinalva_11022020.R


#--- Objetivo: Cria��o de um gr�fico


#--- Autor: Rafael Barbosa da Silva
#--- e-mail: lul.rafaelbarbosa@gmail.com


#--- Data de cria��o: 11/02/2020

#-------------------------


#--- Limpando o ambiente


rm(list = ls())


#--- Setando o diret�rio do banco de dados


setwd("F:/UFPA/Trabalhos/Marinalva_11022020")


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


banco <- read.xlsx(xlsxFile = "Banco de dados.xlsx", sheet = "BD_Ajust")


#--- Verificar a estrutura dos dados


banco %>% glimpse



grafico <-
  banco %>%
  magrittr::set_colnames(c("Mesoregion", "Ano", "Vari�vel")) %>%
  mutate(Ano = factor(x = Ano)) %>%
  mutate(Mesoregion = case_when(Mesoregion == "Baixo Amazonas" ~ "Lower Amazon",
                                Mesoregion == "Maraj�" ~ "Maraj�",
                                Mesoregion == "Metropolitana de Bel�m" ~ "Metropolitan of Bel�m",
                                Mesoregion == "Nordeste Paraense" ~ "Northeast of Par�",
                                Mesoregion == "Sudeste Paraense" ~ "Southeast of Par�",
                                Mesoregion == "Sudoeste Paraense" ~ "Southwest of Par�"
  )) %>%
  ggplot(data = .) +
  geom_line(aes(x = Ano, y = `Vari�vel`, group = Mesoregion,
                colour = Mesoregion), size = 1.05) +
  geom_point(aes(x = Ano, y = `Vari�vel`, group = Mesoregion,
                 colour = Mesoregion), size = 2) +
  labs(x = "Year", y = "Hospitalization rate CSAP") +
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
  scale_color_brewer(palette = "Dark2")



#-----------------------------------


banco1 <- foreign::read.spss(file = "Para04IcsapAg.sav", to.data.frame = T)

banco1 %>% glimpse


grafico1 <-
  banco1 %>%
  filter(ANO_CMPT %in% c(2008, 2017)) %>%
  rename(Year = ANO_CMPT) %>%
  mutate(Year = factor(x = Year)) %>%
  mutate(Mesoregion = case_when(MESO == "Baixo Amazonas" ~ "Lower Amazon",
                                MESO == "Maraj�" ~ "Maraj�",
                                MESO == "Metropolitana de Bel�m" ~ "Metrop. Bel�m",
                                MESO == "Nordeste Paraense" ~ "Northeast of Par�",
                                MESO == "Sudeste Paraense" ~ "Southeast of Par�",
                                MESO == "Sudoeste Paraense" ~ "Southwest of Par�"
  )) %>%
  ggplot(data = .) +
  geom_boxplot(aes(x = Mesoregion, y = TXicsap, fill = Year),
               width = 0.4) +
  labs(x = "Mesoregion", y = "Hospitalization rate CSAP") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(colour = "black", face = "bold", size = 14),
        axis.title.x = element_text(colour = "black", face = "bold", size = 14),
        axis.text = element_text(colour = "black", size = 13),
        strip.text.x = element_text(size = 12, colour = "white"),
        strip.text.y = element_text(size = 12, colour = "white"),
        legend.title = element_text(size = 16, color = "black", face = "bold"),
        legend.text = element_text(size = 16, color = "black"),
        axis.line = element_line(size = 0.5, colour = "black")) +
  scale_fill_brewer(palette = "Greys")



#--- Salvando o gr�fico

# 1. PDF


ggsave(filename = "Grafico_final3.pdf", plot = grafico1,
       device = "pdf", width = 10, height = 6, dpi = 300)


# 2. TIFF


ggsave(filename = "Grafico_final3.tiff", plot = grafico1,
       device = "tiff", width = 10, height = 6, dpi = 300,
       compression = "lzw")


# 3. JPEG


ggsave(filename = "Grafico_final3.jpeg", plot = grafico1,
       device = "jpeg", width = 10, height = 6, dpi = 300)
