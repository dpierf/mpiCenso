#-------------------------------------------------------------------------------#
#                                                                               #
#                  REVISITANDO A TESE DE DOUTORADO - PROJETO T                  #
#                                                                               #
#-------------------------------------------------------------------------------#

# =================== FASE 2 - Criação de crosswalk com RIM =================== #

source('R/00_packages.R')

# -- Etapa 1: download de shapefile ---------------------------------------------

# Regiões Imediatas — delimitação IBGE 2017/2019.
shp_rim <- geobr::read_immediate_region(
  year         = 2019,
  showProgress = FALSE
) |>
  dplyr::select(rim = code_immediate, geom = geom) |>
  st_transform(crs = 4674)   # SIRGAS 2000


# Função de centroides municipais
crosswalk_ano <- function(ano_censo) {
  
  message('Processando municípios de ', ano_censo, '...')
  
  # Shapefile municipal do ano do censo
  # geobr pode não ter 1980 — ver nota no cabeçalho
  shp_mun <- geobr::read_municipality(
    year         = ano_censo,
    cache        = FALSE,
    showProgress = FALSE
  ) |>
    dplyr::select(code_mun = code_muni, geom = geom) |>
    st_transform(crs = 4674)
  
  # Centróide robusto (st_point_on_surface garante ponto dentro do polígono)
  centroides <- shp_mun |>
    st_transform(crs = 5880) |>          # SIRGAS 2000 / Policônica — projeção plana para o Brasil
    st_point_on_surface() |>
    st_transform(crs = 4674) |>          # volta para geográfico para o join
    mutate(ano = ano_censo)
  
  # Join espacial: cada centróide → RIM em que cai
  joined <- st_join(centroides, shp_rim, join = st_within)
  
  # Municípios sem RIM atribuído (raro — fronteiras ou ilhas)
  n_missing <- sum(is.na(joined$rim))
  if (n_missing > 0) {
    message('  ', n_missing, ' município(s) sem RIM: usando nearest para resolver.')
    idx_missing <- which(is.na(joined$rim))
    nearest     <- st_nearest_feature(centroides[idx_missing, ], shp_rim)
    joined$rim[idx_missing] <- shp_rim$rim[nearest]
  }
  
  joined |>
    st_drop_geometry() |>
    dplyr::select(ano, code_mun, rim) |>
    mutate(
      code_mun = as.integer(code_mun),
      rim      = as.integer(rim)
    )
}


# -- Etapa 2: construção do crosswalk -------------------------------------------

anos_censo <- c(1980, 1991, 2000, 2010)

crosswalk_rim <- map_dfr(anos_censo, crosswalk_ano) |>
  mutate(code_mun = ifelse(ano == 1980,
                           code_mun %/% 10L,   # remove último dígito (verificador) para 1980
                           code_mun)) |>
  rename(code_muni = code_mun)                 # renomeando para bater com bases do Censo

# Checar municípios sem RIM após fallback
n_na <- sum(is.na(crosswalk_rim$rim))
if (n_na > 0) {
  warning(n_na, ' registros sem RIM após fallback: investigar.')
  print(crosswalk_rim |> filter(is.na(rim)))
} else {
  message('Crosswalk completo: sem NA em RIM.')
}

# Contagem de municípios por ano e RIM (sanidade)
resumo <- crosswalk_rim |>
  group_by(ano) |>
  summarise(
    n_mun  = n_distinct(code_muni),
    n_rim  = n_distinct(rim),
    .groups = 'drop'
  )

print(resumo)

# Problema com os dados de ES/2000. Correção usando 2010
es_2010 <- data.table(crosswalk_rim)[
  ano == 2010 & substr(as.character(code_muni), 1, 2) == '32',
  .(ano = 2000L, code_muni, rim)   # ano já na posição certa e tipo correto
]

crosswalk_rim <- rbindlist(list(
  data.table(crosswalk_rim)[!(ano == 2000 & substr(as.character(code_muni), 1, 2) == '32')],
  es_2010
))


# -- Etapa 3: salvamento da base ------------------------------------------------

dir_out <- 'data/04_output'
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

write_parquet(crosswalk_rim,
              file.path(dir_out, 'crosswalk_mun_rim.parquet'))

message('\n✓ Crosswalk salvo em: ', file.path(dir_out, 'crosswalk_mun_rim.parquet'))
