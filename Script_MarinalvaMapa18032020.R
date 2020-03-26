#-------------------------

#--- Nome: Script_MarinalvaMapa18032020.R


#--- Objetivo: Criar um mapa para verificar a taxa de mortalidade infantil
#--- entre os anos de 2008 e 2017 no estado do Pará


#--- Autor: Rafael Barbosa da Silva
#--- e-mail: lul.rafaelbarbosa@gmail.com


#--- Data de criação: 18/03/2020

#-------------------------


#--- Limpando o ambiente


rm(list = ls())


#--- Setando o diretório do banco de dados


setwd("F:/UFPA/Trabalhos/MapaMarinalva18032020")


#--- Pacotes usados


if(!require(tidyverse)) {
  install.packages("tidyverse", dependencies = T);
  require(tidyverse)
}

if(!require(brazilmaps)) {
  install.packages("brazilmaps", dependencies = T);
  require(brazilmaps)
}

if(!require(foreign)) {
  install.packages("foreign", dependencies = T);
  require(foreign)
}

if(!require(abjutils)) {
  install.packages("abjutils", dependencies = T);
  require(abjutils)
}

if(!require(sf)) {
  install.packages("sf", dependencies = T);
  require(sf)
}

if(!require(ggsflabel)) {
  install.packages("ggsflabel", dependencies = T);
  require(ggsflabel)
}



#--- Carregando o banco de dados


banco_inicial <- foreign::read.spss(file = "Para04IcsapAgmort.sav", to.data.frame = TRUE)


#--- Explorar o banco de dados

banco_inicial %>%
  glimpse


#--- Selecionar somente as variáveis importantes para análise


banco <-
  banco_inicial %>%
  select(NOME_MUNIC, ANO_CMPT, MUNIC_RES, MESO, TXmort04) %>%
  mutate(NOME_MUNIC = str_trim(string = NOME_MUNIC, side = "right"),
         ANO_CMPT = factor(x = ANO_CMPT)) %>%
  filter(ANO_CMPT %in% c(2008, 2017))


#--- Baixar os dados do Par�


mapa_para <- get_brmap(geo = "City", geo.filter = list(State = 15))

mapa_para <-
  mapa_para %>%
  mutate(City = str_sub(string = City, start = 1, end = 6))


#--- Merge


novo_banco <-
  banco %>%
  mutate(MUNIC_RES = as.character(MUNIC_RES)) %>%
  left_join(mapa_para, c("MUNIC_RES" = "City"))



#--- Mapa


grafico <-
  novo_banco %>%
  mutate(variavel = cut(x = TXmort04, c(0, 10, 20, 30, 40, 50, 60),
                        include.lowest = T)) %>%
  sf::st_as_sf() %>%
  ggplot(data = .) +
  geom_sf(aes(geometry = geometry, fill = variavel)) +
  geom_sf(data = get_brmap(geo = "MesoRegion", geo.filter = list(State = 15)),
          fill = "transparent",
          colour = "black", size = 1.3) +
  ggsflabel::geom_sf_label_repel(data = get_brmap(geo = "MesoRegion", geo.filter = list(State = 15)),
                                 aes(label = case_when(nome == "BAIXO AMAZONAS" ~ "L.A.",
                                                       nome == "MARAJ�" ~ "M",
                                                       nome == "METROPOLITANA DE BEL�M" ~ "M.B.",
                                                       nome == "NORDESTE PARAENSE" ~ "N.P.",
                                                       nome == "SUDESTE PARAENSE" ~ "SE.P.",
                                                       nome == "SUDOESTE PARAENSE" ~ "SW.P."
                                 )),
                                 color = "black", size = 5, fontface = "bold",
                                 force = 100, nudge_x = -0.5, nudge_y = 0.5, seed = 10,
                                 segment.size = 1.25) +
  facet_wrap(~ ANO_CMPT) +
  scale_fill_brewer(palette = "Blues") +
  coord_sf() +
  theme_void() +
  labs(x = "Longitude", y = "Latitude", fill = "Mortality Rate (%)") +
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.size = unit(1, "cm"),
        legend.key.width = unit(1,"cm"),
        strip.text = element_text(size = 20))




#--- Salvando o gráfico

# 1. PDF


ggsave(filename = "Grafico.pdf", plot = grafico,
       device = "pdf", width = 10, height = 6, dpi = 300)



# 2. TIFF


ggsave(filename = "Grafico.tiff", plot = grafico,
       device = "tiff", width = 10, height = 6, dpi = 300,
       compression = "lzw")


# 3. JPEG


ggsave(filename = "Grafico.jpeg", plot = grafico,
       device = "jpeg", width = 10, height = 6, dpi = 300)
