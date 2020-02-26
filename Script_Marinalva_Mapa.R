#-------------------------

#--- Nome: Script_Marinalva_Mapa.R


#--- Objetivo: Criar um mapa para verificar a diferença na cobertura
#--- entre os anos de 2008 e 2017 no estado do Pará


#--- Autor: Rafael Barbosa da Silva
#--- e-mail: lul.rafaelbarbosa@gmail.com


#--- Data de criação: 04/11/2019

#-------------------------


#--- Limpando o ambiente


rm(list = ls())


#--- Setando o diretório do banco de dados


setwd("F:/UFPA/Trabalhos/Mapa - Marinalva")


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


require(ggsflabel)


#--- Carregando o banco de dados


banco <- read.spss("COBERTURAMAPAS1.sav", to.data.frame = T)


#--- Verificar a estrutura dos dados


banco %>% glimpse


#--- Manuseio dos dados


banco <-
  banco %>%
  mutate(NOME_MUNIC = str_trim(string = NOME_MUNIC, side = "right")) %>%
  mutate(NOME_MUNIC = as.character(str_to_upper(string = NOME_MUNIC))) %>%
  mutate(NOME_MUNIC = rm_accent(NOME_MUNIC))


#--- Baixando os dados do mapa (pacote brazilmaps)
#--- e filtrando somente pelo estado do Pará


mapa_para <- get_brmap(geo = "City", geo.filter = list(State = 15)) %>%
  mutate(City = as.numeric(str_sub(string = City, start = 1, end = 6)))


#--- Modificando a estrutura dos dados


banco1 <- reshape2::melt(data = banco,
                         id.vars = c("MUNIC_RES", "NOME_MUNIC", "CODMESO", "MESORREGIAO",
                                     "MESO", "TXCSAP2008", "TXCSAP2017", "latitude",
                                     "longitude", "CLASSECOB2008", "CLASSECOB2017",
                                     "DIFCOB", "DIFTXCSAP"))


#--- Fazendo o join (ou merge) do mapa e do novo banco


novo_banco <- merge(x = banco1, y = mapa_para, by.x = "MUNIC_RES", by.y = "City")


#--- Gráfico final


grafico_final1 <-
novo_banco %>%
  mutate(value_escala = cut(value, breaks = c(0, 20, 40, 60, 80, 100), include.lowest = T)) %>%
  mutate(variable1 = ifelse(test = variable == "COBERTURA2008N",
                            yes = 2008, no = 2017)) %>%
  filter(!is.na(value_escala)) %>%
  mutate(translate_mesoregion =
           case_when(MESO == "Baixo Amazonas" ~ "L.A.",
                     MESO == "Marajó" ~ "M",
                     MESO == "Metropolitana de Belém" ~ "M.B.",
                     MESO == "Nordeste Paraense" ~ "N.P.",
                     MESO == "Sudeste Paraense" ~ "SE.P.",
                     MESO == "Sudoeste Paraense" ~ "SW.P."
           )) %>%
  ggplot(data = .) +
  geom_sf(aes(geometry = geometry, fill = value_escala),
          colour = "black", size = 0.1) +
  geom_sf(data = get_brmap(geo = "MesoRegion", geo.filter = list(State = 15)),
          fill = "transparent",
          colour = "black", size = 1.3) +
  geom_sf_label(data = get_brmap(geo = "MesoRegion", geo.filter = list(State = 15)),
                aes(label = case_when(nome == "BAIXO AMAZONAS" ~ "L.A.",
                                      nome == "MARAJÓ" ~ "M",
                                      nome == "METROPOLITANA DE BELÉM" ~ "M.B.",
                                      nome == "NORDESTE PARAENSE" ~ "N.P.",
                                      nome == "SUDESTE PARAENSE" ~ "SE.P.",
                                      nome == "SUDOESTE PARAENSE" ~ "SW.P."
                )),
                    color = "black", size = 5, fontface = "bold") +
  coord_sf() +
  theme_void() +
  labs(x = "Longitude", y = "Latitude", fill = "Coverage (%)") +
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.size = unit(1, "cm"),
        legend.key.width = unit(1,"cm"),
        strip.text = element_text(size = 20)) +
  scale_fill_brewer(palette = "Blues") +
  facet_wrap(~ variable1)



grafico_final2 <-
  novo_banco %>%
  mutate(value_escala = cut(value, breaks = c(0, 20, 40, 60, 80, 100), include.lowest = T)) %>%
  mutate(variable1 = ifelse(test = variable == "COBERTURA2008N",
                            yes = 2008, no = 2017)) %>%
  filter(!is.na(value_escala)) %>%
  mutate(translate_mesoregion =
           case_when(MESO == "Baixo Amazonas" ~ "L.A.",
                     MESO == "Marajó" ~ "M",
                     MESO == "Metropolitana de Belém" ~ "M.B.",
                     MESO == "Nordeste Paraense" ~ "N.P.",
                     MESO == "Sudeste Paraense" ~ "SE.P.",
                     MESO == "Sudoeste Paraense" ~ "SW.P."
           )) %>%
  ggplot(data = .) +
  geom_sf(aes(geometry = geometry, fill = value_escala),
          colour = "black", size = 0.1) +
  geom_sf(data = get_brmap(geo = "MesoRegion", geo.filter = list(State = 15)),
          fill = "transparent",
          colour = "black", size = 1.3) +
  geom_sf_text(data = get_brmap(geo = "MesoRegion", geo.filter = list(State = 15)),
                aes(label = case_when(nome == "BAIXO AMAZONAS" ~ "L.A.",
                                      nome == "MARAJÓ" ~ "M",
                                      nome == "METROPOLITANA DE BELÉM" ~ "M.B.",
                                      nome == "NORDESTE PARAENSE" ~ "N.P.",
                                      nome == "SUDESTE PARAENSE" ~ "SE.P.",
                                      nome == "SUDOESTE PARAENSE" ~ "SW.P."
                )),
                color = "black", size = 5, fontface = "bold") +
  coord_sf() +
  theme_void() +
  labs(x = "Longitude", y = "Latitude", fill = "Coverage (%)") +
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.size = unit(1, "cm"),
        legend.key.width = unit(1,"cm"),
        strip.text = element_text(size = 20)) +
  scale_fill_brewer(palette = "Blues") +
  facet_wrap(~ variable1)




grafico_final3 <-
  novo_banco %>%
  mutate(value_escala = cut(value, breaks = c(0, 20, 40, 60, 80, 100), include.lowest = T)) %>%
  mutate(variable1 = ifelse(test = variable == "COBERTURA2008N",
                            yes = 2008, no = 2017)) %>%
  filter(!is.na(value_escala)) %>%
  mutate(translate_mesoregion =
           case_when(MESO == "Baixo Amazonas" ~ "L.A.",
                     MESO == "Marajó" ~ "M",
                     MESO == "Metropolitana de Belém" ~ "M.B.",
                     MESO == "Nordeste Paraense" ~ "N.P.",
                     MESO == "Sudeste Paraense" ~ "SE.P.",
                     MESO == "Sudoeste Paraense" ~ "SW.P."
           )) %>%
  ggplot(data = .) +
  geom_sf(aes(geometry = geometry, fill = value_escala),
          colour = "black", size = 0.1) +
  geom_sf(data = get_brmap(geo = "MesoRegion", geo.filter = list(State = 15)),
          fill = "transparent",
          colour = "black", size = 1.3) +
  geom_sf_label_repel(data = get_brmap(geo = "MesoRegion", geo.filter = list(State = 15)),
                aes(label = case_when(nome == "BAIXO AMAZONAS" ~ "L.A.",
                                      nome == "MARAJÓ" ~ "M",
                                      nome == "METROPOLITANA DE BELÉM" ~ "M.B.",
                                      nome == "NORDESTE PARAENSE" ~ "N.P.",
                                      nome == "SUDESTE PARAENSE" ~ "SE.P.",
                                      nome == "SUDOESTE PARAENSE" ~ "SW.P."
                )),
                color = "black", size = 5, fontface = "bold",
                force = 100, nudge_x = -0.5, nudge_y = 0.5, seed = 10,
                segment.size = 1.25) +
  coord_sf() +
  theme_void() +
  labs(x = "Longitude", y = "Latitude", fill = "Coverage (%)") +
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.size = unit(1, "cm"),
        legend.key.width = unit(1,"cm"),
        strip.text = element_text(size = 20)) +
  scale_fill_brewer(palette = "Blues") +
  facet_wrap(~ variable1)



grafico_final4 <-
  novo_banco %>%
  mutate(value_escala = cut(value, breaks = c(0, 20, 40, 60, 80, 100), include.lowest = T)) %>%
  mutate(variable1 = ifelse(test = variable == "COBERTURA2008N",
                            yes = 2008, no = 2017)) %>%
  filter(!is.na(value_escala)) %>%
  mutate(translate_mesoregion =
           case_when(MESO == "Baixo Amazonas" ~ "L.A.",
                     MESO == "Marajó" ~ "M",
                     MESO == "Metropolitana de Belém" ~ "M.B.",
                     MESO == "Nordeste Paraense" ~ "N.P.",
                     MESO == "Sudeste Paraense" ~ "SE.P.",
                     MESO == "Sudoeste Paraense" ~ "SW.P."
           )) %>%
  ggplot(data = .) +
  geom_sf(aes(geometry = geometry, fill = value_escala),
          colour = "black", size = 0.1) +
  geom_sf(data = get_brmap(geo = "MesoRegion", geo.filter = list(State = 15)),
          fill = "transparent",
          colour = "black", size = 1.3) +
  geom_sf_text_repel(data = get_brmap(geo = "MesoRegion", geo.filter = list(State = 15)),
                      aes(label = case_when(nome == "BAIXO AMAZONAS" ~ "L.A.",
                                            nome == "MARAJÓ" ~ "M",
                                            nome == "METROPOLITANA DE BELÉM" ~ "M.B.",
                                            nome == "NORDESTE PARAENSE" ~ "N.P.",
                                            nome == "SUDESTE PARAENSE" ~ "SE.P.",
                                            nome == "SUDOESTE PARAENSE" ~ "SW.P."
                      )),
                      color = "black", size = 5, fontface = "bold",
                      force = 100, nudge_x = -0.5, nudge_y = 0.5, seed = 10) +
  coord_sf() +
  theme_void() +
  labs(x = "Longitude", y = "Latitude", fill = "Coverage (%)") +
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.size = unit(1, "cm"),
        legend.key.width = unit(1,"cm"),
        strip.text = element_text(size = 20)) +
  scale_fill_brewer(palette = "Blues") +
  facet_wrap(~ variable1)



#--- Salvando o gráfico

# 1. PDF


ggsave(filename = "Grafico_final3.pdf", plot = grafico_final3,
       device = "pdf", width = 10, height = 6, dpi = 300)


# 2. TIFF


ggsave(filename = "Grafico_final3.tiff", plot = grafico_final3,
       device = "tiff", width = 10, height = 6, dpi = 300,
       compression = "lzw")


# 3. JPEG


ggsave(filename = "Grafico_final3.jpeg", plot = grafico_final3,
       device = "jpeg", width = 10, height = 6, dpi = 300)
