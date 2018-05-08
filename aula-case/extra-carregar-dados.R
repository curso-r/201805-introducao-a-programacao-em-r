library(tidyverse)

pesqEle <- pesqEle::pesqEle %>% 
  filter(info_election == "Eleições Municipais 2016",
         !is.na(info_muni)) %>% 
  select(-starts_with("txt"))

d_latlon <- abjData::dados_muni %>% 
  mutate(municipio = toupper(abjutils::rm_accent(municipio)))

pesqEle <- pesqEle %>% 
  filter(info_election == "Eleições Municipais 2016",
         !is.na(info_muni)) %>% 
  mutate(info_muni = abjutils::rm_accent(toupper(info_muni))) %>% 
  mutate(info_muni = case_when(
    info_muni == 'PARATY' ~ 'PARATI',
    info_muni == 'MUQUEM DO SAO FRANCISCO' ~ 'MUQUEM DE SAO FRANCISCO',
    info_muni == 'MOGI MIRIM' ~ 'MOJI MIRIM',
    info_muni == 'PARAISO DAS AGUAS' ~ 'CHAPADAO DO SUL',
    info_muni == 'NAZARIA' ~ 'TERESINA',
    info_muni == 'QUINJINGUE' ~ 'QUIJINGUE',
    info_muni == 'ASSU' ~ 'ACU',
    info_muni == 'EMBU DAS ARTES' ~ 'EMBU',
    info_muni == 'FLORINEA' ~ 'FLORINIA',
    info_muni == 'BIRITIBA MIRIM' ~ 'BIRITIBA-MIRIM',
    info_muni == 'LAGOA DE ITAENGA' ~ 'LAGOA DO ITAENGA',
    info_muni == 'SAO FELIPE DO OESTE' ~ 'SAO FELIPE DOESTE',
    info_muni == 'ALVORADA DO OESTE' ~ 'ALVORADA DOESTE',
    info_muni == 'SAO MIGUEL DO GOSTOSO' ~ 'SAO MIGUEL',
    info_muni == 'BOA SAUDE' ~ 'LAGOA NOVA',
    info_muni == 'MACHADINHO DO OESTE' ~ 'MACHADINHO DOESTE',
    info_muni == 'ESPIGAO DO OESTE' ~ 'ESPIGAO DOESTE',
    info_muni == 'SANTA LUZIA DO OESTE' ~ 'SANTA LUZIA DOESTE',
    info_muni == 'ALTA FLORESTA DO OESTE' ~ 'ALTA FLORESTA DOESTE',
    info_muni == 'NOVA BRASILANDIA DO OESTE' ~ 'NOVA BRASILANDIA DOESTE',
    info_muni == 'OLHO DAGUA DO BORGES' ~ 'OLHO-DAGUA DO BORGES',
    info_muni == 'POXOREU' ~ 'POXOREO',
    TRUE ~ info_muni
  )) %>% 
  left_join(d_latlon, c("info_uf" = "uf", "info_muni" = "municipio")) %>% 
  select(-pesq_contractors)

# write_rds(pesqEle, "aula-case/pesqEle.rds", compress = "bz2")
write_csv(pesqEle, "aula-case/pesqEle.csv")
