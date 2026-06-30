#-------------------------------------------------------------------------------#
#                                                                               #
#                  REVISITANDO A TESE DE DOUTORADO - PROJETO T                  #
#                                                                               #
#-------------------------------------------------------------------------------#

# ==================== FASE 1 - Criação das bases de dados ==================== #

source('R/00_packages.R')

# -- Etapa 1: criação das bases do MPI por ano ----------------------------------

## -- CENSO 2010 ----

# Base de Domicílios
dom_2010 <- read_households(
  year         = 2010,
  columns      = c('V0300',                                                # ID domicílio
                   'V4001',                                                # espécie (filtro)
                   'code_muni',                                            # código do município
                   'V0010','V0011',                                        # peso, strata (AP)
                   'V1006', 'V1005',                                       # urbano/rural
                   'V0201', 'V0202', 'V0203', 'V6203',                     # propriedade, paredes, cômodos, densidade
                   'V0205', 'V0206', 'V0207', 'V0208', 'V0210', 'V0211',   # eletricidade, água e esgoto, banheiros, lixo
                   'V0214', 'V0215', 'V0216', 'V0217', 'V0218', 'V0213',   # bens duráveis (TV, roupa, geladeira, tel/cel, rádio)
                   'V0221', 'V0222',                                       # bens durários (moto / carro)
                   'V0401', 'V6529'),                                      # moradores, renda domiciliar
  add_labels   = NULL,
  cache        = FALSE,
  showProgress = TRUE
) |> 
  filter(V4001 %in% c('01', '05')) |>   # particulares ocupados (01 e 05 do SAS)
  collect()

write_parquet(dom_2010, 'data/01_raw/dom_2010.parquet')

# Base de Pessoas
pes_2010 <- read_population(
  year         = 2010,
  columns      = c('V0300', 'V0502',                     # ID domicílio (chave join) e posição no domicílio
                   'V6036', 'V0601', 'V0606',            # idade, sexo, raça
                   'V0627', 'V0628', 'V0629', 'V0630',   # frequência e curso
                   'V0633', 'V0634', 'V6400',            # instrução e anos de estudo
                   'V0641', 'V0642', 'V0648',            # trabalho
                   'V0650', 'V0656', 'V0657', 'V0658'),  # proteção social / benefícios
  add_labels   = NULL,
  cache        = FALSE,
  showProgress = TRUE
) |>
  collect()

write_parquet(pes_2010, 'data/01_raw/pes_2010.parquet')
gc()

# Join pelo ID do domicílio
censo_2010 <- dom_2010 |>
  inner_join(pes_2010, by = 'V0300')

rm(dom_2010, pes_2010)
write_parquet(censo_2010, 'data/01_raw/raw_2010.parquet')
gc() 

# Indicadores D1:D3
ind_dom <- censo_2010 |>
  filter(V0502 == '01') |>                         # pessoa de referência
  mutate(
    
    # Dimensão 1 — Moradia
    d11 = as.integer(is.na(V0202) | V0202 > 3),                   # paredes inadequadas
    d12 = as.integer(is.na(V6203) | V6203 >= 3),                  # densidade >= 3 pessoas/cômodo
    d13 = as.integer(is.na(V0201) | V0201 > 3),                   # condição do domicílio
    
    # Dimensão 2 — Serviços básicos
    d21 = as.integer(                              # água inadequada
      (V1006 == 1 & (as.integer(V0208) != 1 | is.na(V0208))) |
      (V1006 == 2 & (!as.integer(V0208) %in% c(1, 2, 3) | is.na(V0208)))
    ),
    d22 = as.integer(                              # esgoto inadequado
      (V1006 == 1 & (V0205 == 0 | !V0207 %in% c(1, 2) | is.na(V0207))) |
        (V1006 == 2 & (V0205 == 0 | !V0207 %in% c(1, 2, 3) | is.na(V0207)))
    ),
    d23 = as.integer(!V0211 %in% c(1L, 2L) | is.na(V0211)),                  # sem eletricidade
    
    # Dimensão 3 — Padrões de vida
    V6529 = if_else(V6529 >= 9999998, NA_real_, as.numeric(V6529)),
    rpc = (V6529 / V0401),                                                          # renda per capita nominal
    rpcr = rpc * (4370.12 / 3112.29),                                               # renda per capita real
    d31 = as.integer(rpcr < 418.92),                                                # renda < 1/2 SM (set/2015)
    d32 = as.integer(                                                               # bens duráveis insuficientes
      ((is.na(V0213) | V0213 != 1) & (is.na(V0214) | V0214 != 1)) |                   # sem rádio e TV
      (is.na(V0215) | V0215 != 1) |                                                   # sem lavarroupas
      (is.na(V0216) | V0216 != 1) |                                                   # sem geladeira
      ((is.na(V0217) | V0217 != 1) & (is.na(V0218) | V0218 != 1))                     # sem telefone (fixo ou móvel)
    ),
    d33 = as.integer((is.na(V0221) | V0221 != 1) & (is.na(V0222) | V0222 != 1))     # sem veículo locomotor
  ) |>
  mutate(
    peso   = V0010,
    strata = V0011,
    urbano = V1006,
    local  = V1005,
    idade  = V6036,
    sexo   = V0601,
    raca   = V0606
  ) |>
  dplyr::select(V0300, code_muni, peso, strata, urbano, local, idade, sexo, raca, rpc, rpcr,
         d11, d12, d13, d21, d22, d23, d31, d32, d33) |>
  as.data.table()

write_parquet(ind_dom, 'data/02_processed/dom_2010_processed.parquet')

# Indicadores D4:D5
ind_pes <- censo_2010 |>
  mutate(
    
    # Dimensão 4 — Educação
    d41a = case_when(                              # frequência escolar (6-17 anos)
      V6036 < 6 | V6036 > 17 ~ NA_integer_,
      V0628 < 3              ~  0L,
      TRUE                   ~  1L
    ),
    d42a = case_when(                              # defasagem escolar (6-17 anos)
      V6036 < 6 | V6036 > 17                                                                    ~ NA_integer_,
      (V6036 < 15 & ((V6036 - as.integer(V0630)) < 9 | V0629 == 7)) | (V6036 > 14 & V0629 == 7) ~ 0L,
      TRUE                                                                                      ~ 1L
    ),
    d43a = case_when(                              # educação adultos (20+ anos)
      V6036 < 20                                                                                            ~ NA_integer_,
      (V6036 >= 60 & (V0633 %in% c(1,2,3,5) | (V0633 %in% c(4,6) & V0634 != 1))) | (V6036 < 60 & V6400 < 2) ~ 1L,
      TRUE                                                                                                  ~ 0L
    ),
    
    # Dimensão 5 — Emprego e proteção social
    d51a = case_when(  # trabalho assalariado (última semana)
      V6036 < 15 | V6036 > 65                                        ~ NA_integer_,
      (V0641 != 1 & V0642 != 1) | V0648 %in% c(4,5,7) | is.na(V0648) ~ 1L,
      TRUE                                                           ~ 0L
    ),
    d52a = case_when(  # previdência ou programas sociais
      is.na(V0650) & (is.na(V0656) | V0656 == 9) & (is.na(V0657) | V0657 == 9) & (is.na(V0658) | V0658 == 9) ~ NA_integer_,
      V0650 < 3 | V0656 == 1 | V0657 == 1 | V0658 == 1                                                       ~ 0L,
      TRUE                                                                                                   ~ 1L
    )
  ) |>
  dplyr::select(V0300, d41a, d42a, d43a, d51a, d52a) |>
  as.data.table() |>
  (\(dt) dt[, .(
    d41 = if (all(is.na(d41a))) 0L else as.integer(any(d41a == 1, na.rm = TRUE)),  # pelo menos 1 pessoa
    d42 = if (all(is.na(d42a))) 0L else as.integer(any(d42a == 1, na.rm = TRUE)),  # pelo menos 1 pessoa
    d43 = if (all(is.na(d43a))) 0L else as.integer(any(d43a == 1, na.rm = TRUE)),  # pelo menos 1 pessoa
    d51 = if (all(is.na(d51a))) 0L else as.integer(any(d51a == 1, na.rm = TRUE)),  # pelo menos 1 pessoa
    d52 = if (all(is.na(d52a))) 0L else as.integer(all(d52a == 1, na.rm = TRUE))   # ninguém
  ), by = V0300])()

write_parquet(ind_pes, 'data/02_processed/pes_2010_processed.parquet')

# Arranjos domiciliares
arranjo <- censo_2010 |>
  dplyr::select(V0300, V0502, V0601) |>
  as.data.table() |>
  (\(dt) dt[, .(
    sexo_res  = fcase(
      any(V0502 == '01' & V0601 == 1L), 'H',
      any(V0502 == '01' & V0601 == 2L), 'M',
      default = NA_character_
    ),
    conjuge   = fifelse(any(V0502 %in% c('02', '03')),       'S', 'N'),
    filhos    = fifelse(any(V0502 %in% c('04', '05', '06')), 'S', 'N'),
    agregados = fifelse(any(V0502 > '06'),                   'S', 'N')
  ), by = V0300][, arranjo := paste0(sexo_res, conjuge, filhos)])()

write_parquet(arranjo, 'data/02_processed/arranjos_2010.parquet')

# Tabela final
mpi_2010 <- ind_dom |>
  left_join(ind_pes, by = 'V0300') |>
  left_join(arranjo, by = 'V0300') |>
  mutate(
    d1 = (d11 + d12 + d13)              / 3,
    d2 = (d21 + d22 + d23)              / 3,
    d3 = (2*d31 + 0.75*d32 + 0.25*d33)  / 3,
    d4 = (d41 + d42 + d43)              / 3,
    d5 = (2*d51 + d52)                  / 3
  ) |>
  mutate(
    score = (2*d1 + 2*d2 + 2*d3 + 2*d4 + 1*d5) / 9
  ) |>
  mutate(
    pobre_10 = as.integer(score >= 1/10),
    pobre_20 = as.integer(score >= 1/5),
    pobre_25 = as.integer(score >= 1/4),
    pobre_33 = as.integer(score >= 1/3),
    pobre_40 = as.integer(score >= 2/5),
    pobre_50 = as.integer(score >= 1/2),
    pobre_67 = as.integer(score >= 2/3)
  )

write_parquet(mpi_2010, 'data/03_filtered/mpi_2010.parquet')
rm(list=ls()) ; gc()


## -- CENSO 2000 ----

# Base de Domicílios
dom_2000 <- read_households(
  year         = 2000,
  columns      = c('V0300',                                               # ID domicílio
                   'V0201',                                               # espécie (filtro)
                   'code_muni',                                           # código do município
                   'P001', 'AREAP',                                       # peso e strata (AP)
                   'V1006', 'V1005',                                      # urbano/rural
                   'V0203', 'V7203', 'V0205', 'V0202', 'V0221',           # propriedade, cômodos, densidade, tipo de ambiente
                   'V0207', 'V0209', 'V0210', 'V0211', 'V0212', 'V0213',  # eletricidade, água e esgoto, banheiros, lixo
                   'V0214', 'V0215', 'V0217', 'V0219', 'V0220', 'V0222',  # bens duráveis (rádio, roupa, geladeira, tel, carro)
                   'V7100', 'V7616'),                                     # moradores, renda domiciliar
  add_labels   = NULL,
  cache        = FALSE,
  showProgress = TRUE
) |>
  filter(V0201 %in% c('1', '2')) |>
  collect()

write_parquet(dom_2000, 'data/01_raw/dom_2000.parquet')

# Base de Pessoas
pes_2000 <- read_population(
  year         = 2000,
  columns      = c('V0300', 'V0402',                               # ID domicílio (chave join) e posição no domicílio
                   'V4752', 'V0401', 'V0408',                      # idade, sexo, raça
                   'V0428', 'V0429', 'V0430', 'V0431',             # frequência e curso
                   'V0432', 'V0433', 'V0434', 'V4300',             # instrução e anos de estudo
                   'V0439', 'V0440', 'V0447', 'V0448',             # trabalho
                   'V0450', 'V0456', 'V4573', 'V4593', 'V4603'),   # proteção social / benefícios
  add_labels   = NULL,
  cache        = FALSE,
  showProgress = TRUE
) |>
  collect()

write_parquet(pes_2000, 'data/01_raw/pes_2000.parquet')
gc()

# Join pelo ID do domicílio
censo_2000 <- dom_2000 |>
  mutate(V0300 = as.integer(V0300)) |>
  inner_join(pes_2000, by = 'V0300')

rm(dom_2000, pes_2000)
write_parquet(censo_2000, 'data/01_raw/raw_2000.parquet')
gc()


# Indicadores D1:D3
ind_dom <- censo_2000 |>
  filter(V0402 == 1) |>
  mutate(
    
    # Dimensão 1 — Moradia
    d11 = as.integer(V0201 == 2 | V0202 == 3),                    # tipo de moradia inadequado (substituto para 2000)
    d12 = as.integer(is.na(V7203) | V7203 >= 3),                  # densidade >= 3 pessoas/cômodo
    d13 = as.integer(is.na(V0205) | V0205 > 3),                   # condição de ocupação
    
    # Dimensão 2 — Serviços básicos
    d21 = as.integer(                              # água inadequada
      is.na(V0207) |
      (V1006 == 1 & V0207 != 1) |
      (V1006 == 2 & !V0207 %in% c(1, 2))
    ),
    d22 = as.integer(                              # esgoto inadequado
      !(
        (V0210 %in% 1L | (is.na(V0210) & !is.na(V0209))) &
          ((V1006 %in% 1L & V0211 %in% 1:2) |
             (V1006 %in% 2L & V0211 %in% 1:3))
      )
    ),
    d23 = as.integer(is.na(V0213) | V0213 != 1),                  # sem eletricidade
    
    # Dimensão 3 — Padrões de vida
    V7616 = if_else(V7616 >= 999998, NA_real_, as.numeric(V7616)),
    rpc = (V7616 / V7100),                         # renda per capita nominal
    rpcr = rpc * (4370.12 / 1662.11),              # renda per capital real
    d31 = as.integer(rpcr < 418.92),               # renda < 1/2 SM (set/2015)
    d32 = as.integer(                              # bens duráveis insuficientes
      ((V0214 != 1 | is.na(V0214)) & (V0221 == 0 | is.na(V0221))) |  # sem rádio e TV
      (V0217 != 1 | is.na(V0217)) |                                  # sem lavarroupas
      (V0215 != 1 | is.na(V0215)) |                                  # sem geladeira
      ((V0219 != 1 | is.na(V0219)) & (V0220 != 1 | is.na(V0220)))    # sem telefone e computador
    ),
    d33 = as.integer((V0222 == 0 | is.na(V0222)))                   # sem veículo locomotor
  ) |>
  mutate(
    peso = P001,
    strata = AREAP,
    urbano = V1006,
    local = V1005,
    idade = V4752,
    sexo = V0401,
    raca = V0408
  ) |>
  dplyr::select(V0300, code_muni, peso, strata, urbano, local, idade, sexo, raca, rpc, rpcr,
         d11, d12, d13, d21, d22, d23, d31, d32, d33) |>
  as.data.table()

write_parquet(ind_dom, 'data/02_processed/dom_2000_processed.parquet')


# Indicadores D4:D5
ind_pes <- censo_2000 |>
  mutate(
    
    # Dimensão 4 — Educação
    d41a = case_when(  # frequência escolar (6-17 anos)                  
      V4752 < 6  | V4752 > 17 ~ NA_integer_,
      V0429 < 3               ~  0L,               
      TRUE                    ~  1L                
    ),
    d42a = case_when(  # atraso escolar (6-17 anos)
      V4752 < 6 | V4752 > 17                                          ~ NA_integer_,
      (V4752 - V4300) < 9 & !V4300 %in% c(20, 30)                     ~ 0L,         
      TRUE                                                            ~ 1L         
    ),
    d43a = case_when(  # educação adultos (20+ anos)
      V4752 < 20                                                      ~ NA_integer_,
      (V4752 >= 60 & (V4300 < 4 | V4300 %in% c(20, 30))) |
      (V4752 < 60 & (V4300 < 8 | V4300 %in% c(20, 30)))               ~ 1L,
      TRUE                                                            ~ 0L
    ),
    
    # Dimensão 5 — Emprego e proteção social
    d51a = case_when(  # trabalho assalariado
      V4752 < 15 | V4752 > 65                                                               ~ NA_integer_,
      (V0439 != 1 & V0440 != 1) | ((V0447 %in% c(2,4,6,8,9) | is.na(V0447)) & V0448 != 1)   ~ 1L,
      TRUE                                                                                  ~ 0L
    ),
    d52a = case_when(  # previdência ou programas sociais
      is.na(V0450) & is.na(V0456) & is.na(V4573) & is.na(V4593) & is.na(V4603) ~ NA_integer_,
      V0450 == 1 | V0456 == 1 | V4573 > 0 | V4593 > 0 | V4603 > 0              ~ 0L,
      TRUE                                                                     ~ 1L 
    )
  ) |>
  dplyr::select(V0300, d41a, d42a, d43a, d51a, d52a) |>
  as.data.table() |>
  (\(dt) dt[, .(
    d41 = if (all(is.na(d41a))) 0L else as.integer(any(d41a == 1, na.rm = TRUE)),
    d42 = if (all(is.na(d42a))) 0L else as.integer(any(d42a == 1, na.rm = TRUE)),
    d43 = if (all(is.na(d43a))) 0L else as.integer(any(d43a == 1, na.rm = TRUE)),
    d51 = if (all(is.na(d51a))) 0L else as.integer(any(d51a == 1, na.rm = TRUE)),
    d52 = if (all(is.na(d52a))) 0L else as.integer(all(d52a == 1, na.rm = TRUE))
  ), by = V0300])()

write_parquet(ind_pes, 'data/02_processed/pes_2000_processed.parquet')

# Arranjos domiciliares
arranjo <- censo_2000 |>
  dplyr::select(V0300, V0401, V0402) |>
  as.data.table() |>
  (\(dt) dt[, .(
    sexo_res  = fcase(
      any(V0402 == 1L & V0401 == 1L), 'H',
      any(V0402 == 1L & V0401 == 2L), 'M',
      default = NA_character_
    ),
    conjuge   = fifelse(any(V0402 == 2L), 'S', 'N'),
    filhos    = fifelse(any(V0402 == 3L), 'S', 'N'),
    agregados = fifelse(any(V0402 > 3L), 'S', 'N')
  ), by = V0300][, arranjo := paste0(sexo_res, conjuge, filhos)])()

write_parquet(arranjo, 'data/02_processed/arranjos_2000.parquet')

# Tabela final
mpi_2000 <- ind_dom |>
  left_join(ind_pes, by = 'V0300') |>
  left_join(arranjo, by = 'V0300') |>
  mutate(
    d1 = (d11 + d12 + d13)              / 3,
    d2 = (d21 + d22 + d23)              / 3,
    d3 = (2*d31 + 0.75*d32 + 0.25*d33)  / 3,
    d4 = (d41 + d42 + d43)              / 3,
    d5 = (2*d51 + d52)                  / 3
  ) |>
  mutate(
    score = (2*d1 + 2*d2 + 2*d3 + 2*d4 + d5) / 9
  ) |>
  mutate(
    pobre_10 = as.integer(score >= 1/10),
    pobre_20 = as.integer(score >= 1/5),
    pobre_25 = as.integer(score >= 1/4),
    pobre_33 = as.integer(score >= 1/3),
    pobre_40 = as.integer(score >= 2/5),
    pobre_50 = as.integer(score >= 1/2),
    pobre_67 = as.integer(score >= 2/3)
  )

write_parquet(mpi_2000, 'data/03_filtered/mpi_2000.parquet')
rm(list=ls()) ; gc()


## -- CENSO 1991 ----

# Base de Domicílios
dom_1991 <- read_households(
  year         = 1991,
  columns      = c('V0102',                                                                # ID domicílio
                   'V0201',                                                                # espécie (filtro)
                   'code_muni',                                                            # código do município
                   'V7300', #'AREAP',                                                      # peso e strata (AP)
                   'V1061',                                                                # urbano/rural
                   'V0211', 'V2111', 'V0208', 'V0203', 'V0204',                            # propriedade, cômodos, densidade, paredes e telhado
                   'V0205', 'V0206', 'V0207', 'V0213', 'V0214', 'V0221',                   # eletricidade, água e esgoto, banheiros, lixo
                   'V0220', 'V0226', 'V0222', 'V0217', 'V0218', 'V0219', 'V0223', 'V0224', # bens duráveis (rádio, roupa, geladeira, tel, carro, tv)
                   'V0111', 'V0112', 'V2012'),                                             # moradores (H+M), renda domiciliar
  add_labels   = NULL,
  cache        = FALSE,
  showProgress = TRUE
) |>
  filter(V0201 %in% c('1', '2')) |>
  collect() |>
  rename(V0300 = V0102)

write_parquet(dom_1991, 'data/01_raw/dom_1991.parquet')

# Base de Pessoas
pes_1991 <- read_population(
  year         = 1991,
  columns      = c('V0102', 'V0302',                               # ID domicílio (chave join) e posição no domicílio
                   'V3072', 'V0301', 'V0309',                      # idade, sexo, raça
                   'V0323', 'V0324', 'V0325', 'V0326',             # frequência e curso
                   'V0327', 'V0328', 'V3241',                      # instrução e anos de estudo
                   'V0345', 'V0349', 'V0350',                      # trabalho
                   'V0353', 'V0359', 'V0347'),                     # proteção social / benefícios
  add_labels   = NULL,
  cache        = FALSE,
  showProgress = TRUE
) |>
  collect() |>
  rename(V0300 = V0102)

write_parquet(pes_1991, 'data/01_raw/pes_1991.parquet')

# Join pelo ID do domicílio
gc()
censo_1991 <- dom_1991 |>
  mutate(V0300 = as.integer(V0300)) |>
  inner_join(pes_1991, by = 'V0300')

rm(dom_1991, pes_1991)
write_parquet(censo_1991, 'data/01_raw/raw_1991.parquet')
gc()


# Indicadores D1:D3
ind_dom <- censo_1991 |>
  filter(V0302 == 1) |>
  mutate(
    
    # Dimensão 1 — Moradia (d11 similar a 2010)
    d11 = as.integer((V0203 > 2 | is.na(V0203)) | (V0204 > 5 | is.na(V0204))),       # paredes/telhado inadequado
    d12 = as.integer(is.na(V0211) | V0211 == 0 | ((V0111 + V0112) / V0211) >= 3),    # densidade >= 3 pessoas/cômodo
    d13 = as.integer(is.na(V0208) | V0208 > 3),                                      # condição de ocupação
    
    # Dimensão 2 — Serviços básicos
    d21 = as.integer(                              # água inadequada
      (V1061 <= 3 & (!V0205 %in% c(1, 4) | is.na(V0205))) |
      (V1061 >= 4 & (!V0205 %in% c(1, 2, 4) | is.na(V0205)))
    ),
    d22 = as.integer(                              # esgoto inadequado
      (V0207 != 1 | is.na(V0207)) |
        (V0207 == 1 & (
          is.na(V0206) | V0206 == 0 | V0206 == 7 |
            (V1061 <= 3 & !V0206 %in% c(1L, 2L, 3L)) |
            (V1061 >= 4 & !V0206 %in% c(1L, 2L, 3L, 4L))
        ))
    ),
    d23 = as.integer(V0221 > 2 | is.na(V0221)),    # sem eletricidade (mesmo que sem medidor)
    
    # Dimensão 3 — Padrões de vida
    V2012 = if_else(V2012 >= 9999999998, NA_real_, as.numeric(V2012)),
    rpc = (V2012 / (V0111 + V0112)),                                                                           # renda per capita nominal
    rpcr = rpc * (4370.12/0.1709)/2750000,                                                                     # renda per capital real
    d31 = as.integer(rpcr < 418.92),                                                                           # renda < 1/2 SM (set/2015)
    d32 = as.integer(                                                                                          # bens duráveis insuficientes
      ((V0220 != 1 | is.na(V0220)) & ((V0223 != 1 | is.na(V0223)) & (!V0224 %in% c(1,2,3) | is.na(V0224)))) |    # sem rádio e TV
      (is.na(V0226) | V0226 != 1) |                                                                              # sem lavarroupas
      (is.na(V0222) | !V0222 %in% c(1,2)) |                                                                      # sem geladeira
      (is.na(V0217) | !V0217 %in% c(1,2))                                                                        # sem telefone e computador
    ),
    d33 = as.integer((V0218 == 0 | is.na(V0218)) & (V0219 == 0 | is.na(V0219)))                                # sem veículo locomotor
  ) |>
  mutate(
    peso    = V7300,
    strata  = -1,
    urbano  = fifelse(V1061 <= 3, 1, 2),
    local   = V1061,
    idade   = V3072,
    sexo    = V0301,
    raca    = V0309
  ) |>
  dplyr::select(V0300, code_muni, peso, strata, urbano, local, idade, sexo, raca, rpc, rpcr,
         d11, d12, d13, d21, d22, d23, d31, d32, d33) |>
  as.data.table()

write_parquet(ind_dom, 'data/02_processed/dom_1991_processed.parquet')


# Indicadores D4:D5
ind_pes <- censo_1991 |>
  mutate(
    
    # Dimensão 4 — Educação
    d41a = case_when(
      V3072 < 6  | V3072 > 17                                                                     ~ NA_integer_,
      (!is.na(V0324) & V0324 != 0) | (!is.na(V0325) & V0325 != 0) | (!is.na(V0326) & V0326 != 0)  ~ 0L,
      TRUE                                                                                        ~ 1L
    ),
    d42a = case_when(
      V3072 < 6 | V3072 > 17                                          ~ NA_integer_,
      (V3072 - V3241) < 9 & !V3241 %in% c(20, 30)                     ~ 0L,         
      TRUE                                                            ~ 1L         
    ),
    d43a = case_when(
      V3072 < 20                                                      ~ NA_integer_,
      (V3072 >= 60 & (V3241 < 4 | V3241 %in% c(20, 30))) |
      (V3072 < 60 & (V3241 < 8 | V3241 %in% c(20, 30)))               ~ 1L,
      TRUE                                                            ~ 0L
    ),
    
    # Dimensão 5 — Emprego e proteção social
    d51a = case_when(
      V3072 < 15 | V3072 > 65                                                        ~ NA_integer_,
      (!V0345 %in% c(1,2) | (V0349 %in% c(1,3,5,9,11) | is.na(V0349)))               ~ 1L,
      TRUE                                                                           ~ 0L
    ),
    d52a = case_when(
      V0353 == 2 | is.na(V0353) | is.na(V0359)           ~ NA_integer_,
      V0359 %in% c(1,2,3) | V0353 == 1                   ~ 0L,
      TRUE                                               ~ 1L 
    )
  ) |>
  dplyr::select(V0300, d41a, d42a, d43a, d51a, d52a) |>
  as.data.table() |>
  (\(dt) dt[, .(
    d41 = if (all(is.na(d41a))) 0L else as.integer(any(d41a == 1, na.rm = TRUE)),
    d42 = if (all(is.na(d42a))) 0L else as.integer(any(d42a == 1, na.rm = TRUE)),
    d43 = if (all(is.na(d43a))) 0L else as.integer(any(d43a == 1, na.rm = TRUE)),
    d51 = if (all(is.na(d51a))) 0L else as.integer(any(d51a == 1, na.rm = TRUE)),
    d52 = if (all(is.na(d52a))) 0L else as.integer(all(d52a == 1, na.rm = TRUE))
  ), by = V0300])()

write_parquet(ind_pes, 'data/02_processed/pes_1991_processed.parquet')


# Arranjos domiciliares
arranjo <- censo_1991 |>
  dplyr::select(V0300, V0301, V0302) |>
  as.data.table() |>
  (\(dt) dt[, .(
    sexo_res  = fcase(
      any(V0302 == 1L & V0301 == 1L), 'H',
      any(V0302 == 1L & V0301 == 2L), 'M',
      default = NA_character_
    ),
    conjuge   = fifelse(any(V0302 == 2L), 'S', 'N'),
    filhos    = fifelse(any(V0302 == 3L), 'S', 'N'),
    agregados = fifelse(any(V0302 > 3L), 'S', 'N')
  ), by = V0300][, arranjo := paste0(sexo_res, conjuge, filhos)])()

write_parquet(arranjo, 'data/02_processed/arranjos_1991.parquet')

# Tabela final
mpi_1991 <- ind_dom |>
  left_join(ind_pes, by = 'V0300') |>
  left_join(arranjo, by = 'V0300') |>
  mutate(
    d1 = (d11 + d12 + d13)              / 3,
    d2 = (d21 + d22 + d23)              / 3,
    d3 = (2*d31 + 0.75*d32 + 0.25*d33)  / 3,
    d4 = (d41 + d42 + d43)              / 3,
    d5 = (2*d51 + d52)                  / 3
  ) |>
  mutate(
    score = (2*d1 + 2*d2 + 2*d3 + 2*d4 + d5) / 9
  ) |>
  mutate(
    pobre_10 = as.integer(score >= 1/10),
    pobre_20 = as.integer(score >= 1/5),
    pobre_25 = as.integer(score >= 1/4),
    pobre_33 = as.integer(score >= 1/3),
    pobre_40 = as.integer(score >= 2/5),
    pobre_50 = as.integer(score >= 1/2),
    pobre_67 = as.integer(score >= 2/3)
  )

write_parquet(mpi_1991, 'data/03_filtered/mpi_1991.parquet')
rm(list=ls()) ; gc()


## -- CENSO 1980 ----

# Intermediário: calcular moradores por unidade e rendimento total
rendas <- c('V607', 'V608', 'V610', 'V611', 'V612', 'V613', 'V609')
auxdom_1980 <- read_population(year = 1980, cache = FALSE, showProgress = FALSE,
                               columns = c('V601', 'V607', 'V608', 'V610', 'V611', 'V612', 'V613', 'V609')) |>
  collect() |>
  rename(V0300 = V601) |>
  mutate(across(all_of(rendas), ~ if_else(. == 9999999, NA_real_, as.numeric(.)))) |> 
  mutate(
    renda_total = rowSums(across(all_of(rendas)), na.rm = TRUE),
    renda_total = if_else(if_all(all_of(rendas), is.na), NA_real_, renda_total)
  ) |>
  group_by(V0300) |>
  summarise(
    V001 = n(),                                                                        # Total de residentes
    V002 = if_else(all(is.na(renda_total)), NA_real_, sum(renda_total, na.rm = TRUE))  # Rendimento total do domicílio
  ) |>
  mutate(rpc = V002 / V001) |>
  dplyr::select(V0300, V001, V002, rpc)

# Base de Domicílios
dom_1980 <- read_households(
  year         = 1980,
  columns      = c('V601',                                         # ID domicílio
                   'V201',                                         # espécie (filtro)
                   'code_muni_1980',                               # código do município
                   'V603', #'AREAP',                               # peso e strata (AP)
                   'V198',                                         # urbano/rural
                   'V212', 'V209', 'V203', 'V205',                 # propriedade, cômodos, densidade, paredes e telhado
                   'V217', 'V206', 'V207', 'V208',                 # eletricidade, água e esgoto, banheiros, lixo
                   'V218', 'V214', 'V219', 'V216', 'V220', 'V221'  # bens duráveis (rádio, fogão, geladeira, tel, carro, tv)
  ),
  add_labels   = NULL,
  cache        = FALSE,
  showProgress = TRUE
) |>
  filter(V201 %in% c('1', '3')) |>
  collect() |>
  rename(
    V0300 = V601,
    code_muni = code_muni_1980) |>
  left_join(auxdom_1980, by = 'V0300') |>
  mutate(V010 = V001 / V212) #Densidade por cômodo calculada a posteriori

write_parquet(dom_1980, 'data/01_raw/dom_1980.parquet')
rm(auxdom_1980, rendas)

# Base de Pessoas
pes_1980 <- read_population(
  year         = 1980,
  columns      = c('V601', 'V503',                                           # ID domicílio (chave join) e posição no domicílio
                   'V606', 'V501', 'V509',                                   # idade, sexo, raça
                   'V519', 'V520', 'V521', 'V522',                           # frequência e curso
                   'V523', 'V524', 'V525',                                   # instrução e anos de estudo
                   'V528', 'V529', 'V530', 'V533',                           # trabalho
                   'V534', 'V610'),                                          # proteção social / benefícios
  add_labels   = NULL,
  cache        = FALSE,
  showProgress = TRUE
) |>
  collect() |>
  rename(V0300 = V601) |>
  mutate( #Criando atributo de anos de estudo
    # Limpeza de sem declaração
    V520c = if_else(V520 == 9 | is.na(V520), NA_integer_, as.integer(V520)),
    V521c = if_else(V521 == 9 | is.na(V521), NA_integer_, as.integer(V521)),
    V522c = if_else(V522 == 9 | is.na(V522), NA_integer_, as.integer(V522)),
    V523c = if_else(V523 == 9 | is.na(V523), NA_integer_, as.integer(V523)),
    V524c = if_else(V524 == 9 | is.na(V524), NA_integer_, as.integer(V524)),
    
    anos_estudo = case_when(
      
      # V521: Frequenta escola seriada
      V521c == 1 ~ pmin(coalesce(V520c, 0L), 4L),           # Primário
      V521c == 2 ~ pmin(4L + coalesce(V520c, 0L), 8L),      # Ginasial
      V521c == 3 ~ pmin(coalesce(V520c, 0L), 8L),           # 1° Grau
      V521c == 4 ~ pmin(8L + coalesce(V520c, 0L), 11L),     # 2° Grau
      V521c == 5 ~ pmin(8L + coalesce(V520c, 0L), 11L),     # Colegial
      V521c %in% c(6L, 7L) ~ 8L,                            # Supletivo
      V521c == 8 ~ pmin(11L + coalesce(V520c, 0L), 15L),    # Superior
      
      # V522: Frequenta curso não seriado
      V522c == 1 ~  0L,                  # Pré-escolar
      V522c == 2 ~  1L,                  # Alfabetização adultos
      V522c %in% c(3L, 5L) ~  8L,        # Supletivo 1° Grau
      V522c %in% c(4L, 6L) ~ 11L,        # Supletivo 2° Grau
      V522c == 7 ~ 11L,                  # Vestibular
      V522c == 8 ~ 15L,                  # Mestrado/Doutorado
      
      # V523 e V524: Quando não frequenta, usar concluído
      is.na(V524c) | V524c == 0 ~  0L,
      V524c == 1 ~  1L,                                      # Alfabetização
      V524c == 2 ~ pmin(coalesce(V523c, 0L), 4L),            # Primário
      V524c == 3 ~ pmin(4L + coalesce(V523c, 0L), 8L),       # Ginasial
      V524c == 4 ~ pmin(coalesce(V523c, 0L), 8L),            # 1° Grau
      V524c == 5 ~ pmin(8L + coalesce(V523c, 0L), 11L),      # 2° Grau
      V524c == 6 ~ pmin(8L + coalesce(V523c, 0L), 11L),      # Colegial
      V524c == 7 ~ pmin(11L + coalesce(V523c, 0L), 15L),     # Superior
      V524c == 8 ~ 15L,                                      # Mestrado/Doutorado
      
      TRUE ~ NA_integer_
    ),
    anos_estudo = pmin(anos_estudo, 15L)
  )

write_parquet(pes_1980, 'data/01_raw/pes_1980.parquet')
gc()

# Join pelo ID do domicílio
censo_1980 <- dom_1980 |>
  inner_join(pes_1980, by = 'V0300')

rm(dom_1980, pes_1980)
write_parquet(censo_1980, 'data/01_raw/raw_1980.parquet')
gc()

# Indicadores D1:D3
ind_dom <- censo_1980 |>
  filter(V503 %in% c(0,1)) |>
  mutate(
    
    # Dimensão 1 — Moradia (d11 similar a 2010)
    d11 = as.integer(                                             # paredes/telhado inadequado
      (!V203 %in% c(2L, 4L) | is.na(V203)) |
        (is.na(V205) | V205 > 5 | V205 == 0)
    ),       
    d12 = as.integer(is.na(V010) | V010 >= 3),                    # densidade >= 3 pessoas/cômodo
    d13 = as.integer(is.na(V209) | V209 == 0 | V209 > 5),         # condição de ocupação
    
    # Dimensão 2 — Serviços básicos
    d21 = as.integer(                              # água inadequada
      (V198 <= 3 & (is.na(V206) | !V206 %in% c(1, 6))) |
      (V198 >= 4 & (is.na(V206) | !V206 %in% c(1, 3, 6)))
    ),
    d22 = as.integer(                              # esgoto inadequado
      is.na(V207) |
        (V198 <= 3 & !V207 %in% c(2L, 4L)) |
        (V198 >= 4 & !V207 %in% c(2L, 4L, 6L)) |
        (V198 <= 3 & V207 %in% c(2L, 4L)    & (V208 != 1 | is.na(V208))) |
        (V198 >= 4 & V207 %in% c(2L, 4L, 6L) & (V208 != 1 | is.na(V208)))
    ),
    d23 = as.integer(V217 > 4 | is.na(V217)),                  # sem eletricidade (mesmo que sem medidor)
    
    # Dimensão 3 — Padrões de vida
    rpcr = rpc * (4370.12/0.0000000121913)/2750000000000,                    # renda per capital real
    d31 = as.integer(rpcr < 418.92),                                         # renda < 1/2 SM (set/2015)
    d32 = as.integer(                                                        # bens duráveis insuficientes
      ((V218 != 1 | is.na(V218)) & (is.na(V220) | !V220 %in% c(1,3,5))) |     # sem rádio e TV
        (is.na(V214) | V214 != 1) |                                           # sem fogão adequado (substitui máquina de lavar)
        (is.na(V219) | V219 != 1) |                                           # sem geladeira
        (is.na(V216) | V216 != 1)                                             # sem telefone
    ),
    d33 = as.integer(!V221 %in% c(1,3) | is.na(V221))                        # sem veículo locomotor
  ) |>
  mutate(
    peso      = V603,
    strata    = -1,
    urbano    = fifelse(V198 <= 3, 1, 2),
    local     = V198,
    idade     = V606,
    sexo      = V501,
    raca      = V509
  ) |>
  dplyr::select(V0300, code_muni, peso, strata, urbano, local, idade, sexo, raca, rpc, rpcr,
         d11, d12, d13, d21, d22, d23, d31, d32, d33) |>
  as.data.table()

write_parquet(ind_dom, 'data/02_processed/dom_1980_processed.parquet')

# Indicadores D4:D5
ind_pes <- censo_1980 |>
  mutate(
    
    # Dimensão 4 — Educação
    d41a = case_when(
      V606 < 6  | V606 > 17                                                                                            ~ NA_integer_,
      (!is.na(V520) & !V520 %in% c(0,9)) | (!is.na(V521) & !V521 %in% c(0,9)) | (!is.na(V522) & !V522 %in% c(0,9))     ~ 0L,
      TRUE                                                                                                             ~ 1L
    ),
    d42a = case_when(
      V606 < 6 | V606 > 17                                          ~ NA_integer_,
      is.na(anos_estudo)                                            ~ 1L,
      (V606 - anos_estudo) < 9 & V524c != 1 & V522c != 2            ~ 0L,
      TRUE                                                          ~ 1L         
    ),
    d43a = case_when(
      V606 < 20                                                         ~ NA_integer_,
      is.na(anos_estudo)                                                ~ 1L,
      (V606 >= 60 & anos_estudo < 4) | (V606 < 60 & anos_estudo < 8)    ~ 1L,
      TRUE                                                              ~ 0L
    ),

    # Dimensão 5 — Emprego e proteção social
    d51a = case_when(
      V606 < 15 | V606 > 65                                                        ~ NA_integer_,
      (!V528 %in% c(1,5) | (V533 %in% c(0,1,2,5,8,9) | is.na(V533)))               ~ 1L,
      TRUE                                                                         ~ 0L
    ),
    d52a = case_when(
      V534 %in% c(2,4,6) | V529 == 3                   ~ 0L,
      TRUE                                             ~ 1L 
    )
  ) |>
  dplyr::select(V0300, d41a, d42a, d43a, d51a, d52a) |>
  as.data.table() |>
  (\(dt) dt[, .(
    d41 = if (all(is.na(d41a))) 0L else as.integer(any(d41a == 1, na.rm = TRUE)),
    d42 = if (all(is.na(d42a))) 0L else as.integer(any(d42a == 1, na.rm = TRUE)),
    d43 = if (all(is.na(d43a))) 0L else as.integer(any(d43a == 1, na.rm = TRUE)),
    d51 = if (all(is.na(d51a))) 0L else as.integer(any(d51a == 1, na.rm = TRUE)),
    d52 = if (all(is.na(d52a))) 0L else as.integer(all(d52a == 1, na.rm = TRUE))
  ), by = V0300])()

write_parquet(ind_pes, 'data/02_processed/pes_1980_processed.parquet')

# Arranjos domiciliares
arranjo <- censo_1980 |>
  dplyr::select(V0300, V501, V503) |>
  as.data.table() |>
  (\(dt) dt[, .(
    sexo_res  = fcase(
      any(V503 %in% c(1L,0L) & V501 == 1L), 'H',
      any(V503 %in% c(1L,0L) & V501 == 3L), 'M',
      default = NA_character_
    ),
    conjuge   = fifelse(any(V503 == 2L), 'S', 'N'),
    filhos    = fifelse(any(V503 == 3L), 'S', 'N'),
    agregados = fifelse(any(V503 >= 4L), 'S', 'N')
  ), by = V0300][, arranjo := paste0(sexo_res, conjuge, filhos)])()

write_parquet(arranjo, 'data/02_processed/arranjos_1980.parquet')

# Tabela final
mpi_1980 <- ind_dom |>
  left_join(ind_pes, by = 'V0300') |>
  left_join(arranjo, by = 'V0300') |>
  mutate(
    d1 = (d11 + d12 + d13)              / 3,
    d2 = (d21 + d22 + d23)              / 3,
    d3 = (2*d31 + 0.75*d32 + 0.25*d33)  / 3,
    d4 = (d41 + d42 + d43)              / 3,
    d5 = (2*d51 + d52)                  / 3
  ) |>
  mutate(
    score = (2*d1 + 2*d2 + 2*d3 + 2*d4 + d5) / 9
  ) |>
  mutate(
    pobre_10 = as.integer(score >= 1/10),
    pobre_20 = as.integer(score >= 1/5),
    pobre_25 = as.integer(score >= 1/4),
    pobre_33 = as.integer(score >= 1/3),
    pobre_40 = as.integer(score >= 2/5),
    pobre_50 = as.integer(score >= 1/2),
    pobre_67 = as.integer(score >= 2/3)
  )

write_parquet(mpi_1980, 'data/03_filtered/mpi_1980.parquet')
rm(list=ls()) ; gc()


# -- Etapa 2: apensamento de bases ----------------------------------------------

# Carregamento
mpi_2010 <- read_parquet('data/03_filtered/mpi_2010.parquet')
mpi_2000 <- read_parquet('data/03_filtered/mpi_2000.parquet')
mpi_1991 <- read_parquet('data/03_filtered/mpi_1991.parquet')
mpi_1980 <- read_parquet('data/03_filtered/mpi_1980.parquet')
gc()

harmonizar <- function(df) {
  df |>
    mutate(
      V0300   = as.character(V0300),
      urbano  = as.integer(urbano),
      local   = as.character(local),
      sexo    = as.integer(sexo),
      raca    = as.integer(raca),
      strata  = as.character(strata)
    )
}

# Tratamento e junção 
mpi <- bind_rows(
  mpi_2010 |> harmonizar() |> mutate(ano = 2010L, uf = substr(code_muni, 1, 2)),
  mpi_2000 |> harmonizar() |> mutate(ano = 2000L, uf = substr(code_muni, 1, 2)),
  mpi_1991 |> harmonizar() |> mutate(ano = 1991L, uf = substr(code_muni, 1, 2)),
  mpi_1980 |> harmonizar() |> mutate(ano = 1980L, uf = substr(code_muni, 1, 2))
) |>
  mutate( #Padronizando códigos ao longo dos anos
    sexo = case_when(
      ano == 1980 & sexo == 1 ~ 1L,
      ano == 1980 & sexo == 3 ~ 2L,
      ano != 1980             ~ as.integer(sexo),
      TRUE                    ~ NA_integer_
    ),
    raca = case_when(
      ano != 1980 & raca == 9 ~ NA_integer_,    # N/A para todos os anos
      ano == 1980 & raca == 2 ~ 1L,             # branco
      ano == 1980 & raca == 4 ~ 2L,             # preto
      ano == 1980 & raca == 8 ~ 4L,             # pardo
      ano == 1980 & raca == 6 ~ 3L,             # amarelo
      ano == 1980 & raca == 9 ~ NA_integer_,    # N/A
      ano != 1980             ~ as.integer(raca),
      TRUE                    ~ NA_integer_
    )
  ) |>
  mutate(
    # Transformação de atributos em fatores
    ano      = factor(ano),
    uf       = as.integer(uf),
    regiao   = factor(as.integer(substr(uf, 1, 1))),
    urbano   = factor(urbano),
    sexo     = factor(sexo),
    raca     = factor(raca),
    
    # Criação de atributos complementares    
    log_rpcr = log1p(rpcr),
    arranjo2 = substr(arranjo, 2, 3),
    
    # Derivação de atributos de score
    score_c = score * pobre_33,              # Score censurado: metodologia Alkire-Foster (MPI = E[score * I(pobre)] = H × A)
    grupo_sc = cut(                          # Grupos de score
      score,
      breaks         = c(0, 1/5, 1/3, 1/2, 1.001),
      include.lowest = TRUE,
      right          = FALSE,
      labels         = FALSE
    )
  ) |>
  dplyr::select(-c(conjuge, filhos, agregados, sexo_res))

rm(mpi_1980, mpi_1991, mpi_2000, mpi_2010)
gc() 

write_parquet(mpi, 'data/04_output/mpi.parquet')

# Preparação do dict
dict <- list(
  regiao   = c('Norte', 'Nordeste', 'Sudeste', 'Sul', 'Centro-Oeste'),
  urbano   = c('Urbano', 'Rural'),
  sexo     = c('Homem', 'Mulher'),
  raca     = c('Branca', 'Preta', 'Amarela', 'Parda', 'Indígena'),
  arranjo  = c('HNN' = 'Unipessoal', 'HSN' = 'Casal Sem', 'HSS' = 'Casal Com',  'HNS' = 'Monoparental',
               'MNN' = 'Unipessoal', 'MSN' = 'Casal Sem', 'MSS' = 'Casal Com',  'MNS' = 'Monoparental'),
  arranjo2 = c('NN' = 'Unipessoal', 'SN' = 'Casal Sem', 'SS' = 'Casal Com',  'NS' = 'Monoparental'),
  grupo_sc = c('Não Pobre', 'Vulnerável', 'Pobre', 'Ext. Pobre')
)

saveRDS(dict, 'data/04_output/dict.rds')
