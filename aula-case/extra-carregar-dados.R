library(tidyverse)
library(abjutils)

# bases de dados originais -----------------------------------------------------
pesq_details <- read_rds("aula-case/pesq_details.rds")
pesq_main <- read_rds("aula-case/pesq_main.rds")
data(dados_muni, package = "abjData")

# municipios com latitude e longitude ------------------------------------------
d_latlon <- dados_muni %>% 
  mutate(municipio = toupper(rm_accent(municipio)))

# bases de dados originais -----------------------------------------------------
pesq_main_tidy <- pesq_main %>%
  filter(numero_de_identificacao != 'Nenhum registro encontrado!') %>%
  set_names(c('arq', 'id', 'empresa', 'id_stat', 'nm_stat',
              'dt_reg', 'abrangencia', 'acoes')) %>%
  select(arq:abrangencia) %>%
  separate(abrangencia, c('uf', 'muni'), sep = ' / ', fill = "right") %>%
  mutate(nm_stat = rm_accent(toupper(nm_stat))) %>%
  mutate(arq_id = str_extract(arq, "([0-9A-Z_)]+)(?=\\.)")) %>%
  distinct(arq_id, id, .keep_all = TRUE)

# expressões regulares ---------------------------------------------------------
re_origem <- "(?<=Origem do Recurso: )([a-zA-Z()\\s]+)"
re_cnpj <- "(?<=CNPJ:\\s{1,5})([0-9]+)"

clean_emp <- function(x) {
  re <- "(?<=CNPJ:\\s{1,5}[0-9]{14}\\s{1,3}-)((.|\n)+)"
  x %>%
    str_extract(re) %>%
    str_squish() %>%
    str_to_upper()
}

# base de dados arrumada -------------------------------------------------------
pesq_details_tidy <- pesq_details %>%
  mutate(key = str_squish(key)) %>%
  spread(key, val) %>%
  select(-result) %>%
  janitor::clean_names() %>%
  set_names(c("arq",
              "cargo",
              "contratante_propria_empresa",
              "contratantes",
              "sobre_municipio",
              "dt_divulgacao",
              "dt_inicio",
              "dt_registro",
              "dt_termino",
              "eleicao",
              "empresa_contratada",
              "n_entrevistados",
              "estatistico_responsavel",
              "metodologia_pesquisa",
              "id",
              "pagantes",
              "plano_amostral",
              "estatistico_registro",
              "verificacao",
              "valor")) %>%
  mutate_at(vars(starts_with("dt_")), funs(lubridate::dmy)) %>%
  mutate(n_entrevistados = as.numeric(n_entrevistados),
         valor = parse_number(valor, locale = locale(
           decimal_mark = ",", grouping_mark = "."
         ))) %>%
  mutate(contratantes = rslp:::remove_accents(contratantes)) %>%
  mutate(preco_por_entrevistado = valor / n_entrevistados,
         origem = str_extract(contratantes, re_origem),
         origem = str_remove_all(origem, "[^a-zA-Z ]"),
         origem = str_squish(origem)) %>%
  replace_na(list(origem = "Vazio")) %>%
  mutate(contratante_propria_empresa = if_else(
    contratante_propria_empresa == "Não", "Não", "Sim")) %>%
  mutate(estatistico_registro = str_extract(estatistico_registro, "[0-9]+")) %>%
  mutate(arq_id = str_extract(arq, "([0-9A-Z_)]+)(?=_)")) %>%
  mutate(criterio_origem = origem == "Recursos proprios" &
           contratante_propria_empresa == "Sim") %>%
  # tudo o que nao bateu é lixo - dado duplicado
  inner_join(select(pesq_main_tidy, -arq), c("id", "arq_id")) %>%
  group_by(estatistico_registro) %>%
  mutate(empresas_por_estatistico = n_distinct(empresa), n = n()) %>%
  ungroup() %>%
  mutate(cnpj = str_extract(empresa_contratada, re_cnpj),
         emp_nm = clean_emp(empresa_contratada)) %>% 
  group_by_at(vars(-arq, -arq_id, -id)) %>%
  slice(1) %>%
  ungroup()

# base de dados para visualizacao ----------------------------------------------
pesqEle <- pesq_details_tidy %>%
  rowid_to_column("id_seq") %>%
  select(
    # informações de identificação
    id_seq,
    id_pesq = id,
    id_muni = arq_id,
    # informações básicas
    info_uf = uf,
    info_muni = muni,
    info_election = eleicao,
    info_position = cargo,
    # informações da empresa
    comp_nm = emp_nm,
    comp_cnpj = cnpj,
    comp_contract_same = contratante_propria_empresa,
    # informações do estatístico responsável
    stat_id = id_stat,
    stat_nm = nm_stat,
    # informações da pesquisa
    pesq_n = n_entrevistados,
    pesq_val = valor,
    pesq_contractors = contratantes,
    pesq_origin = origem,
    # datas
    dt_reg = dt_registro,
    dt_pub = dt_divulgacao,
    dt_start = dt_inicio,
    dt_end = dt_termino,
    # textos (nao serao usados)
    txt_verif = verificacao,
    txt_method = metodologia_pesquisa,
    txt_about = sobre_municipio,
    txt_plan = plano_amostral
  ) %>% 
  filter(info_election == "Eleições Municipais 2016", 
         !is.na(info_muni)) %>% 
  select(-starts_with("txt")) %>% 
  # join com base de latitude e longitude
  mutate(info_muni = toupper(rm_accent(info_muni))) %>% 
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
