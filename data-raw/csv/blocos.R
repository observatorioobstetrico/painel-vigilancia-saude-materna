#Carregando a base auxiliar que contém variáveis referentes ao nome do município, UF, região e IDHM
municipios_kmeans <- read.csv2("data-raw/csv/IDH_municipios-com-agrupamento_Kmeans.csv", sep = ";") |>
  janitor::clean_names() |>
  dplyr::select(
    codmunres = codmun6,
    municipio = nome,
    uf = nome_uf,
    regiao = regiao_pais,
    idhm,
    grupo_kmeans
  )

municipios_adicionais <- read.csv2("data-raw/csv/tabela_auxiliar_de_municipios_e_IDH.csv", sep = ";") |>
  janitor::clean_names() |>
  dplyr::select(
    codmunres = codmun6,
    municipio = nome,
    uf = nome_uf,
    regiao = regiao_pais,
    idhm
  ) |>
  dplyr::filter(idhm == "#N/A")

municipios_adicionais$idhm <- NA

aux_municipios_1 <- dplyr::full_join(municipios_kmeans, municipios_adicionais)

aux_municipios_1$regiao[which(aux_municipios_1$regiao == "Centro-oeste")] <- "Centro-Oeste"

aux_municipios_1$uf[which(aux_municipios_1$uf == "SAO PAULO")] <- "São Paulo"

#Carregando a base auxiliar que contém variáveis referentes às micro e macrorregiões de saúde
aux_r_saude <- readODS::read_ods("data-raw/ods/regioes_macrorregioes.ods") |>
  janitor::clean_names() |>
  dplyr::rename(
    codmunres = codmun,
    municipio2 = nomemun,
    cod_r_saude = codr_saude,
    r_saude = rsaude
  )

#Juntando as duas bases auxiliares
aux_municipios <- dplyr::left_join(aux_municipios_1, aux_r_saude, by = "codmunres") |>
  dplyr::select(!municipio2)

#Criando uma coluna contendo o ranking dos IDHMs
aux_idhm <- data.frame(
  "idhm" = unique(sort(aux_municipios$idhm)),
  "posicao_idhm" = c(0, (length(unique(sort(aux_municipios$idhm))) - 1):1)
)

#Carregando a base auxiliar que contém variáveis referentes ao IDH das UFs e do Brasil
aux_idh_estados <- read.csv("data-raw/csv/tabela_IDH-censo2010_UFs-e-Brasil.csv", dec = ",")[-1, c(1, 3, 2)] |>
  janitor::clean_names() |>
  dplyr::rename(
    "uf" = territorialidade,
    "idh_uf" = idhm,
    "posicao_idh_uf" = posicao_idhm
  )

#Juntando todas as bases auxiliares (a tabela_aux_municipios será salva para ser utilizada ao carregar o pacote)
tabela_aux_municipios <- dplyr::left_join(dplyr::left_join(aux_municipios, aux_idhm, by = "idhm"), aux_idh_estados, by = "uf")

#Lendo os arquivos originais
bloco1_aux <- read.csv("data-raw/csv/indicadores_bloco1_socioeconomicos_2012-2022.csv") |>
  janitor::clean_names()

bloco2_aux <- read.csv("data-raw/csv/indicadores_bloco2_planejamento_reprodutivo_SUS_ANS_2012_2022.csv") |>
  janitor::clean_names()

bloco3_aux <- read.csv("data-raw/csv/indicadores_bloco3_assistencia_pre-natal_2012-2022.csv") |>
  janitor::clean_names()

bloco4_aux <- read.csv("data-raw/csv/indicadores_bloco4_assistencia_ao_parto_2012-2022.csv") |>
  janitor::clean_names()

bloco4_deslocamento_muni_aux <- read.csv("data-raw/csv/indicadores_bloco4_deslocamento_parto_municipio_2012-2020.csv") |>
  janitor::clean_names()

bloco4_deslocamento_muni_aux$km_partos_fora_uf <- as.numeric(bloco4_deslocamento_muni_aux$km_partos_fora_uf)
bloco4_deslocamento_muni_aux$km_partos_fora_uf_alta_complexidade <- as.numeric(bloco4_deslocamento_muni_aux$km_partos_fora_uf_alta_complexidade)
bloco4_deslocamento_muni_aux$km_partos_fora_uf_baixa_complexidade <- as.numeric(bloco4_deslocamento_muni_aux$km_partos_fora_uf_baixa_complexidade)

bloco4_deslocamento_uf_aux <- read.csv("data-raw/csv/indicadores_bloco4_deslocamento_parto_UF_2012-2020.csv") |>
  janitor::clean_names() |>
  dplyr::select(!uf) |>
  dplyr::rename(uf = nome) |>
  dplyr::mutate(
    uf = sub('.', '', uf)
  )

bloco4_deslocamento_uf_aux$uf[which(bloco4_deslocamento_uf_aux$uf == "rasil")] <- "Brasil"
bloco4_deslocamento_uf_aux$km_partos_fora_macrorregiao <- as.numeric(bloco4_deslocamento_uf_aux$km_partos_fora_macrorregiao)
bloco4_deslocamento_uf_aux$km_partos_fora_macrorregiao_alta_complexidade <- as.numeric(bloco4_deslocamento_uf_aux$km_partos_fora_macrorregiao_alta_complexidade)
bloco4_deslocamento_uf_aux$km_partos_fora_macrorregiao_baixa_complexidade <- as.numeric(bloco4_deslocamento_uf_aux$km_partos_fora_macrorregiao_baixa_complexidade)

bloco5_aux <- read.csv("data-raw/csv/indicadores_bloco5_condicao_de_nascimento_2012_2022.csv") |>
  janitor::clean_names()

# asfixia_aux <- read.csv("data-raw/csv/asfixia_2012_2022.csv", sep = ';') |>
#   janitor::clean_names() |>
#   dplyr::rename(total_nascidos = total_de_nascidos_vivos)

# malformacao_aux <- read.csv("data-raw/csv/malformacao_2012_2022.csv", sep = ';') |>
#   janitor::clean_names() |>
#   dplyr::arrange(codmunres, ano) |>
#   dplyr::filter(codmunres %in% aux_municipios$codmunres)

bloco6_mortalidade_aux <- read.csv("data-raw/csv/indicadores_bloco6_mortalidade_materna_2012-2022.csv") |>
  dplyr::select(!c(uf, municipio, regiao))

bloco6_morbidade_aux <- read.csv("data-raw/csv/indicadores_bloco6_morbidade_materna_2012-2020.csv", sep = ";") |>
  janitor::clean_names()

bloco6_aux <- dplyr::left_join(bloco6_mortalidade_aux, bloco6_morbidade_aux, by = c("ano", "codmunres"))

# bloco7_neonatal_aux <- read.csv("data-raw/csv/indicadores_bloco7_mortalidade_neonatal_2012-2022.csv") #|>
#   #dplyr::select(!c(uf, municipio, regiao))
#
# bloco7_fetal_aux <- read.csv("data-raw/csv/indicadores_bloco7_mortalidade_fetal_2012-2022.csv") #|>
#   #dplyr::select(!(nascidos)
#   #) |>
#  # dplyr::select(!c(uf, municipio, regiao))
#
# bloco7_perinatal_aux <- read.csv("data-raw/csv/indicadores_bloco7_mortalidade_perinatal_2012-2022.csv") #|>
#   #dplyr::select(!c(uf, municipio, regiao))
# bloco7_perinatal_aux$codmunres <- as.numeric(bloco7_perinatal_aux$codmunres)
#
# juncao_bloco7_aux <- dplyr::left_join(bloco7_neonatal_aux, bloco7_fetal_aux, by = c("ano", "codmunres"))
# bloco7_aux <- dplyr::left_join(juncao_bloco7_aux, bloco7_perinatal_aux, by = c("ano", "codmunres"))
#
# bloco8_materno_garbage_aux <- read.csv("data-raw/csv/materno_garbage_2012-2022.csv") |>
#   janitor::clean_names() |>
#   dplyr::select(!c(uf, municipio, regiao))
#
# bloco8_fetal_garbage_aux <- read.csv("data-raw/csv/fetais_garbage_2012-2022.csv") |>
#   janitor::clean_names() |>
#   dplyr::select(!c(uf, municipio, regiao))
#
# bloco8_neonat_garbage_aux <- read.csv("data-raw/csv/neonat_garbage_2012-2022.csv") |>
#   janitor::clean_names()
#
# bloco8_fetal_causas_aux <- read.csv("data-raw/csv/fetais_causas_2012-2022.csv") |>
#   janitor::clean_names() |>
#   dplyr::select(!c(uf, municipio, regiao))
#
# bloco8_neonat_causas_aux <- read.csv("data-raw/csv/neonat_causas_2012-2022.csv") |>
#   janitor::clean_names()
#
# bloco8_fetal_evitaveis_aux <- read.csv("data-raw/csv/fetais_evitaveis_2012-2022.csv") |>
#   janitor::clean_names() |>
#   dplyr::select(!c(uf, municipio, regiao))
#
# bloco8_neonat_evitaveis_aux <- read.csv("data-raw/csv/neonat_evitaveis_2012-2022.csv") |>
#   janitor::clean_names()


###VER QUAIS COLUNAS VOCÊS PRECISAM TIRAR AQUI; NO ARQUIVO DE 2012 A 2020, ESTOU TIRANDO A COLUNA 'X'
base_incompletude_sinasc_aux <- read.csv2("data-raw/csv/incompletude_SINASC_2012-2022.csv", sep = ",")[, -1] |>
  janitor::clean_names() |>
  dplyr::filter(codmunres %in% aux_municipios$codmunres)

base_incompletude_sim_maternos_aux <- read.csv("data-raw/csv/incompletude_sim_obitos_maternos.csv") |>
  janitor::clean_names() |>
  dplyr::filter(codmunres %in% aux_municipios$codmunres)

base_incompletude_sim_mif_aux <- read.csv("data-raw/csv/incompletude_sim_obitos_mif.csv") |>
  janitor::clean_names() |>
  dplyr::filter(codmunres %in% aux_municipios$codmunres)

base_incompletude_sim_aux <- dplyr::full_join(base_incompletude_sim_maternos_aux, base_incompletude_sim_mif_aux, by = c("codmunres", "ano"))

base_incompletude_deslocamento_aux <- read.csv("data-raw/csv/incompletitude_indicadores_deslocamento_parto.csv") |>
  janitor::clean_names() |>
  dplyr::select(!uf)


#Adicionando as variáveis referentes ao nome do município, UF, região e micro e macrorregiões de saúde
bloco1 <- dplyr::left_join(bloco1_aux, aux_municipios, by = "codmunres")
bloco1 <- bloco1 |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco1) == "ano") + 1):(which(names(bloco1) == "municipio") - 1)
  )

bloco2 <- dplyr::left_join(bloco2_aux, aux_municipios, by = "codmunres")
bloco2 <- bloco2 |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco2) == "ano") + 1):(which(names(bloco2) == "municipio") - 1)
  )

bloco3 <- dplyr::left_join(bloco3_aux, aux_municipios, by = "codmunres")
bloco3 <- bloco3 |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco3) == "ano") + 1):(which(names(bloco3) == "municipio") - 1)
  )


bloco4 <- dplyr::left_join(bloco4_aux, aux_municipios, by = "codmunres")
bloco4 <- bloco4 |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco4) == "ano") + 1):(which(names(bloco4) == "municipio") - 1)
  )

bloco4_deslocamento_muni <- dplyr::left_join(bloco4_deslocamento_muni_aux, aux_municipios, by = "codmunres")
bloco4_deslocamento_muni <- bloco4_deslocamento_muni |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco4_deslocamento_muni) == "ano") + 1):(which(names(bloco4_deslocamento_muni) == "municipio") - 1)
  )

bloco4_deslocamento_uf <- dplyr::left_join(bloco4_deslocamento_uf_aux, aux_municipios |> dplyr::select(uf, regiao) |> unique(), by = "uf")

bloco5 <- dplyr::left_join(bloco5_aux, aux_municipios, by = "codmunres")
bloco5 <- bloco5 |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco5) == "ano") + 1):(which(names(bloco5) == "municipio") - 1)
  )

bloco6 <- dplyr::left_join(bloco6_aux, aux_municipios, by = "codmunres")
bloco6 <- bloco6 |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco6) == "ano") + 1):(which(names(bloco6) == "municipio") - 1)
  )

# bloco7 <- dplyr::left_join(bloco7_aux, aux_municipios, by = "codmunres")
# # bloco7 <- bloco7 |>
# #   dplyr::select(
# #     ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
# #     (which(names(bloco7) == "ano") + 1):(which(names(bloco7) == "municipio") - 1)
# #   )
#
# asfixia <- dplyr::left_join(asfixia_aux, aux_municipios, by = "codmunres")
# asfixia <- asfixia |>
#   dplyr::filter(codmunres %in% tabela_aux_municipios$codmunres) |>
#   dplyr::select(
#     ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
#     (which(names(asfixia) == "ano") + 1):(which(names(asfixia) == "municipio") - 1)
#   )
#
# malformacao <- dplyr::left_join(malformacao_aux, aux_municipios, by = "codmunres")
#
# malformacao <- malformacao |>
#   dplyr::select(
#     ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
#     (which(names(malformacao) == "ano") + 1):(which(names(malformacao) == "municipio") - 1)
#   )
#
# bloco8_materno_garbage_aux <- dplyr::left_join(bloco8_materno_garbage_aux, aux_municipios, by = "codmunres")
# bloco8_materno_garbage <- bloco8_materno_garbage_aux |>
#   dplyr::select(
#     ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
#     (which(names(bloco8_materno_garbage_aux) == "ano") + 1):(which(names(bloco8_materno_garbage_aux) == "municipio") - 1)
#   )
#
# bloco8_fetal_garbage_aux <- dplyr::left_join(bloco8_fetal_garbage_aux, aux_municipios, by = "codmunres")
# bloco8_fetal_garbage <- bloco8_fetal_garbage_aux |>
#   dplyr::select(
#     ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
#     (which(names(bloco8_fetal_garbage_aux) == "ano") + 1):(which(names(bloco8_fetal_garbage_aux) == "municipio") - 1)
#   )
#
#
# bloco8_neonat_garbage_aux <- dplyr::left_join(bloco8_neonat_garbage_aux, aux_municipios, by = "codmunres")
# bloco8_neonat_garbage <- bloco8_neonat_garbage_aux |>
#   dplyr::select(
#     ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
#     (which(names(bloco8_neonat_garbage_aux) == "ano") + 1):(which(names(bloco8_neonat_garbage_aux) == "municipio") - 1)
#   )
#
# bloco8_fetal_causas_aux <- dplyr::left_join(bloco8_fetal_causas_aux, aux_municipios, by = "codmunres")
# bloco8_fetal_causas <- bloco8_fetal_causas_aux |>
#   dplyr::select(
#     ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
#     (which(names(bloco8_fetal_causas_aux) == "ano") + 1):(which(names(bloco8_fetal_causas_aux) == "municipio") - 1)
#   )
#
# bloco8_neonat_causas_aux <- dplyr::left_join(bloco8_neonat_causas_aux, aux_municipios, by = "codmunres")
# bloco8_neonat_causas <- bloco8_neonat_causas_aux |>
#   dplyr::select(
#     ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
#     (which(names(bloco8_neonat_causas_aux) == "ano") + 1):(which(names(bloco8_neonat_causas_aux) == "municipio") - 1)
#   )
#
# bloco8_fetal_evitaveis_aux <- dplyr::left_join(bloco8_fetal_evitaveis_aux, aux_municipios, by = "codmunres")
# bloco8_fetal_evitaveis <- bloco8_fetal_evitaveis_aux |>
#   dplyr::select(
#     ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
#     (which(names(bloco8_fetal_evitaveis_aux) == "ano") + 1):(which(names(bloco8_fetal_evitaveis_aux) == "municipio") - 1)
#   )
#
#
# bloco8_neonat_evitaveis_aux <- dplyr::left_join(bloco8_neonat_evitaveis_aux, aux_municipios, by = "codmunres")
# bloco8_neonat_evitaveis <- bloco8_neonat_evitaveis_aux |>
#   dplyr::select(
#     ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
#     (which(names(bloco8_neonat_evitaveis_aux) == "ano") + 1):(which(names(bloco8_neonat_evitaveis_aux) == "municipio") - 1)
#   )


base_incompletude_sinasc <- dplyr::left_join(base_incompletude_sinasc_aux, aux_municipios, by = "codmunres")
base_incompletude_sinasc <- base_incompletude_sinasc |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(base_incompletude_sinasc) == "ano") + 1):(which(names(base_incompletude_sinasc) == "municipio") - 1)
  )

base_incompletude_sim <- dplyr::left_join(base_incompletude_sim_aux, aux_municipios, by = "codmunres")
base_incompletude_sim <- base_incompletude_sim |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(base_incompletude_sim) == "ano") + 1):(which(names(base_incompletude_sim) == "municipio") - 1)
  )

base_incompletude_deslocamento <- dplyr::left_join(base_incompletude_deslocamento_aux, aux_municipios, by = "codmunres")
base_incompletude_deslocamento <- base_incompletude_deslocamento |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(base_incompletude_deslocamento) == "ano") + 1):(which(names(base_incompletude_deslocamento) == "municipio") - 1)
  )

base_incompletude <- dplyr::full_join(
  dplyr::full_join(
    dplyr::full_join(
      base_incompletude_sinasc,
      base_incompletude_deslocamento,
      by = c("ano", "codmunres", "municipio", "grupo_kmeans", "uf", "regiao", "cod_r_saude", "r_saude", "cod_macro_r_saude", "macro_r_saude")
    ),
    base_incompletude_sim,
    by = c("ano", "codmunres", "municipio", "grupo_kmeans", "uf", "regiao", "cod_r_saude", "r_saude", "cod_macro_r_saude", "macro_r_saude")
  ),
  base_incompletude_deslocamento
)


#Carregando a base que contém as informações necessárias para o cálcula da referência de baixo peso
base_referencia_baixo_peso <- read.csv("data-raw/csv/Nasc_baixo_peso_muni2006_2010.csv")

#Carregando a base que contém os fatores de correção para a RMM
rmm_fator_de_correcao <- read.csv("data-raw/csv/rmm_fator_de_correcao.csv", sep = ";", dec = ",", fileEncoding = "utf-8")[, -c(2, 3, 4)] |>
  janitor::clean_names() |>
  tidyr::pivot_longer(
    cols = !localidade,
    names_to = "ano",
    values_to = "fator_de_correcao"
  ) |>
  dplyr::mutate(
    ano = dplyr::case_when(
      ano == "x2012" ~ 2012,
      ano == "x2013" ~ 2013,
      ano == "x2014" ~ 2014,
      ano == "x2015" ~ 2015,
      ano == "x2016" ~ 2016,
      ano == "x2017" ~ 2017,
      ano == "x2018" ~ 2018,
      ano == "x2019" ~ 2019,
      ano == "x2020" ~ 2020,
    )
  )

#Carregando a base que contém as RMM corrigidas para estado, região e Brail de 2012 a 2021
rmm_corrigida <- read.csv("data-raw/csv/rmm_corrigida_2012-2021.csv") |>
  dplyr::select(ano, localidade, RMM) |>
  dplyr::mutate(RMM = round(RMM, 1))

#Criando os dataframes/vetores contendo as escolhas de municípios, estados e micro e macrorregões de saúde
municipios_choices <- tabela_aux_municipios |>
  dplyr::select(uf, municipio)

estados_choices <- tabela_aux_municipios |>
  dplyr::pull(uf) |>
  unique()

micro_r_saude_choices <- tabela_aux_municipios |>
  dplyr::select(uf, r_saude) |>
  unique()

macro_r_saude_choices <- tabela_aux_municipios |>
  dplyr::select(uf, macro_r_saude) |>
  unique()

#Lendo a tabela contendo as informações sobre os indicadores
tabela_indicadores <- read.csv("data-raw/csv/tabela_indicadores.csv")


usethis::use_data(bloco1, overwrite = TRUE)
usethis::use_data(bloco2, overwrite = TRUE)
usethis::use_data(bloco3, overwrite = TRUE)
usethis::use_data(bloco4, overwrite = TRUE)
usethis::use_data(bloco4_deslocamento_muni, overwrite = TRUE)
usethis::use_data(bloco4_deslocamento_uf, overwrite = TRUE)
usethis::use_data(bloco5, overwrite = TRUE)
usethis::use_data(bloco6, overwrite = TRUE)
# usethis::use_data(bloco7, overwrite = TRUE)
# usethis::use_data(asfixia, overwrite = TRUE)
# usethis::use_data(malformacao, overwrite = TRUE)
# usethis::use_data(bloco8_materno_garbage, overwrite = TRUE)
# usethis::use_data(bloco8_fetal_garbage, overwrite = TRUE)
# usethis::use_data(bloco8_neonat_garbage, overwrite = TRUE)
# usethis::use_data(bloco8_fetal_causas, overwrite = TRUE)
# usethis::use_data(bloco8_neonat_causas, overwrite = TRUE)
# usethis::use_data(bloco8_fetal_evitaveis, overwrite = TRUE)
# usethis::use_data(bloco8_neonat_evitaveis, overwrite = TRUE)
usethis::use_data(base_incompletude, overwrite = TRUE)
usethis::use_data(tabela_aux_municipios, overwrite = TRUE)
usethis::use_data(municipios_choices, overwrite = TRUE)
usethis::use_data(estados_choices, overwrite = TRUE)
usethis::use_data(micro_r_saude_choices, overwrite = TRUE)
usethis::use_data(macro_r_saude_choices, overwrite = TRUE)
usethis::use_data(base_referencia_baixo_peso, overwrite = TRUE)
usethis::use_data(tabela_indicadores, overwrite = TRUE)  #Utilizada no nível 3
usethis::use_data(rmm_fator_de_correcao, overwrite = TRUE)
usethis::use_data(rmm_corrigida, overwrite = TRUE)





