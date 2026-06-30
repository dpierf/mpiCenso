#-------------------------------------------------------------------------------#
#                                                                               #
#                  REVISITANDO A TESE DE DOUTORADO - PROJETO T                  #
#                                                                               #
#-------------------------------------------------------------------------------#

# ============== FASE 3 - Criação de bases derivadas e amostras ================ #

source('R/00_packages.R')

# Bases originais
mpi    <- read_parquet('data/04_output/mpi.parquet')                # bases do censo demográfico
cw_rim <- read_parquet('data/04_output/crosswalk_mun_rim.parquet')  # de-para município - RIM
gc()

cw_rim <- cw_rim |>
  mutate(ano = factor(ano))

# Simplificação da base original
mpi_final <- mpi |>
  left_join(cw_rim, by = c('code_muni', 'ano')) |>
  dplyr::select(  # Mantendo só atributos que não podem ser derivados de outro jeito
    ano, uf, code_muni, rim, V0300, peso,
    urbano, sexo, raca, arranjo2,
    d1:d5, score, log_rpcr
  )

write_parquet(mpi_final, 'data/04_output/mpi_simplificado.parquet')
rm(mpi, cw_rim)
gc()


# Criando base agregada por RIM e ano
mpi_rim <- mpi |>
  left_join(cw_rim, by = c('code_muni', 'ano')) |>
  filter(!is.na(rim)) |>
  mutate(
    score_c  = score * ifelse(score >= 1/3, 1, 0),
    grupo_sc = cut(score,
                   breaks = c(0, 1/5, 1/3, 1/2, 1.001),
                   include.lowest = TRUE, right = FALSE, labels = FALSE)
  ) |>
  group_by(rim, ano) |>
  summarise(
    # MPI e componentes
    MPI           = weighted.mean(score_c,                       w = peso, na.rm = TRUE),
    H             = weighted.mean(ifelse(score >= 1/3, 1, 0),    w = peso, na.rm = TRUE),
    score_med     = weighted.mean(score,                         w = peso, na.rm = TRUE),
    # Renda
    rpcr_medG     = expm1(weighted.mean(log_rpcr,                w = peso, na.rm = TRUE)),  #Média geométrica
    rpcr_medA     = weighted.mean(expm1(log_rpcr),               w = peso, na.rm = TRUE),   #Média aritmética
    # Dimensões e Indicadores
    across(d1:d5,   ~ weighted.mean(.x, w = peso, na.rm = TRUE), .names = '{.col}_med'),
    across(d11:d52, ~ weighted.mean(.x, w = peso, na.rm = TRUE), .names = '{.col}_med'),
    # Situação de domicílio
    pct_urb       = weighted.mean(urbano == 1,                   w = peso, na.rm = TRUE),
    # Sexo do responsável
    pct_homem     = weighted.mean(sexo == 1,                     w = peso, na.rm = TRUE),
    # Raça/cor
    pct_branca    = weighted.mean(raca == 1,                     w = peso, na.rm = TRUE),
    pct_negra     = weighted.mean(raca %in% c(2L, 4L),           w = peso, na.rm = TRUE),
    # Arranjo domiciliar (referência implícita: Unipessoal = NN)
    pct_mono      = weighted.mean(arranjo2 == 'NS',              w = peso, na.rm = TRUE),
    pct_casal_com = weighted.mean(arranjo2 == 'SS',              w = peso, na.rm = TRUE),
    n             = n(),
    n_exp         = sum(peso, na.rm = TRUE),
    .groups       = 'drop'
  ) |>
  mutate(A = MPI / H) |>
  filter(!is.na(MPI), n >= 100)   # excluir RIMs com poucas obs

write_parquet(mpi_rim, 'data/04_output/mpi_rims.parquet')


# Criando amostra multiplamente estratificada
mpi_sample <- mpi_final |>
  filter(!is.na(score), !is.na(uf), !is.na(urbano),
         !is.na(sexo), !is.na(raca), !is.na(arranjo2),
         !is.na(peso), peso > 0) |>
  group_by(ano, uf, urbano, sexo, raca, arranjo2) |>
  slice_sample(n = 250, weight_by = peso) |>
  ungroup() |>
  mutate(pobre_33 = as.integer(score >= 1/3))

write_parquet(mpi_sample, 'data/04_output/mpi_sample.parquet')
rm(mpi_final) ; gc()
