#-------------------------------------------------------------------------------#
#                                                                               #
#                  REVISITANDO A TESE DE DOUTORADO - PROJETO T                  #
#                                                                               #
#-------------------------------------------------------------------------------#


# ================ FASE 4 - Análise descritiva/gráfica de bases ================ #

source('R/00_packages.R')
mpi  <- read_parquet('data/04_output/mpi_simplificado.parquet')
dict <- readRDS('data/04_output/dict.rds')


# -- Etapa 0: Helpers e Funções -------------------------------------------------

# Pesos normalizados para densidades
norm_peso <- function(df, ...) {
  df |>
    group_by(...) |>
    mutate(w = peso / sum(peso)) |>
    ungroup()
}

# Médias ponderadas das dimensões
calc_dim <- function(df, var, nome) {
  df |>
    group_by(ano) |>
    summarise(media = weighted.mean({{ var }}, w = peso, na.rm = TRUE)) |>
    mutate(dim = nome)
}

# Contribuição por atributo
calc_contrib <- function(df, var, nome) {
  df |>
    group_by(regiao, ano) |>
    summarise(valor   = weighted.mean({{ var }} * pobre_33, w = peso, na.rm = TRUE),
              .groups = 'drop') |>
    mutate(dim = nome)
}

# Densidade do score por regiões
make_dens_score_reg <- function(ano_sel, titulo) {
  mpi |>
    filter(ano == ano_sel) |>
    norm_peso(ano, regiao) |>
    mutate(regiao = factor(dict$regiao[regiao], levels = dict$regiao)) |>
    ggplot(aes(x = score, weight = w,
               color = regiao, fill = regiao)) +
    geom_density(alpha = 0.15, linewidth = 0.7, bw = 1/27) +
    scale_color_manual(values = pal_regioes) +
    scale_fill_manual(values  = pal_regioes) +
    scale_x_continuous(limits = c(0, 1),
                       labels = percent_format(accuracy = 1)) +
    labs(title = titulo, x = 'Score', y = 'Densidade',
         color = NULL, fill = NULL)
}

# Densidade do MPI por regiões
make_dens_mpi_reg <- function(ano_sel, titulo) {
  mpi |>
    filter(pobre_33 == 1, ano == ano_sel, !is.na(score)) |>
    norm_peso(ano, regiao) |>
    mutate(regiao = factor(dict$regiao[regiao], levels = dict$regiao)) |>
    ggplot(aes(x = score, weight = w,
               color = regiao, fill = regiao)) +
    geom_density(alpha = 0.15, linewidth = 0.7, bw = 1/27) +
    scale_color_manual(values = pal_regioes) +
    scale_fill_manual(values  = pal_regioes) +
    scale_x_continuous(limits = c(1/3, 1),
                       labels = percent_format(accuracy = 1)) +
    labs(title = titulo, x = 'Score', y = 'Densidade',
         color = NULL, fill = NULL)
}

# ECDF ponderada em grade de n pontos
wecdf_tbl <- function(df, x_col, w_col, group_cols, n = 500) {
  df |>
    group_by(across(all_of(group_cols))) |>
    reframe(
      # grade de probabilidades acumuladas
      prob      = seq(0, 1, length.out = n),
      # quantis correspondentes (interpolação linear sobre a ECDF ponderada)
      score_val = {
        xv  <- .data[[x_col]]
        wv  <- .data[[w_col]]
        ord <- order(xv)
        xs  <- xv[ord]
        wc  <- cumsum(wv[ord]) / sum(wv)
        approx(wc, xs, xout = seq(0, 1, length.out = n), rule = 2)$y
      }
    )
}

# Dominância por Regiões
make_dom_reg <- function(ano_sel, titulo) {
  ecdf_reg <- wecdf_tbl(mpi, 'score', 'peso', c('ano', 'regiao')) |>
    mutate(
      ano    = factor(ano),
      regiao = factor(dict$regiao[regiao], levels = dict$regiao)
    )
  
  ecdf_reg |>
    filter(ano == ano_sel) |>
    ggplot(aes(x = score_val, y = prob, color = regiao)) +
    geom_line(linewidth = 0.85) +
    scale_color_manual(values = pal_regioes) +
    scale_x_continuous(limits = c(0, 1),
                       labels = percent_format(accuracy = 1)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(title = titulo, x = 'Score', y = 'F(score)', color = NULL)
}

# Dominância por Arranjo
make_dom_arr <- function(ano_sel, titulo) {
  ecdf_arr <- wecdf_tbl(mpi, 'score', 'peso', c('ano', 'arranjo')) |>
    mutate(
      ano     = factor(ano),
      arranjo = factor(dict$arranjo2[arranjo],
                       levels = unname(dict$arranjo2[c('NN', 'SN', 'SS', 'NS')]))
    )
  
  ecdf_arr |>
    filter(ano == ano_sel) |>
    ggplot(aes(x = score_val, y = prob, color = arranjo)) +
    geom_line(linewidth = 0.85) +
    scale_color_brewer(palette = 'Set2') +
    scale_x_continuous(limits = c(0, 1),
                       labels = percent_format(accuracy = 1)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(title = titulo, x = 'Score', y = 'F(score)', color = NULL)
}

#Gini ponderado (sem dependência de pacote externo)
gini_pond <- function(x, w) {
  ord <- order(x)
  x   <- x[ord]; w <- w[ord]
  w   <- w / sum(w)
  cw  <- cumsum(w)
  2 * sum(w * x * (cw - w / 2)) / sum(w * x) - 1
}

# Paletas, tema e dimensões
dim_nomes <- c(
  d1 = 'Moradia',
  d2 = 'Serviços Básicos',
  d3 = 'Padrão de Vida',
  d4 = 'Educação',
  d5 = 'Trabalho e Proteção'
)

pal_anos <- c(
  '1980' = '#2166AC', '1991' = '#4DAF4A',
  '2000' = '#FF7F00', '2010' = '#E41A1C'
)

pal_regioes <- c(
  'Norte'        = '#2166AC',
  'Nordeste'     = '#FF7F00',
  'Sudeste'      = '#7570B3',
  'Sul'          = '#E7298A',
  'Centro-Oeste' = '#4DAF4A'
)

pal_urbano <- c('Urbano' = '#1F78B4', 'Rural'  = '#33A02C')
pal_sexo   <- c('Homem'  = '#1F78B4', 'Mulher' = '#E7298A')

pal_arranjo <- c(
  'Unipessoal'   = '#1B7837', 'Casal Sem'    = '#4393C3',
  'Casal Com'    = '#D6604D', 'Monoparental' = '#8073AC'
)

theme_set(
  theme_minimal(base_size = 11) +
    theme(
      legend.position  = 'bottom',
      plot.title       = element_text(face = 'bold'),
      plot.subtitle    = element_text(color = 'grey40', size = 9),
      strip.text       = element_text(face = 'bold'),
      legend.key.width = unit(1.2, 'cm')
    )
)


# -- Etapa 1: Análise gráfica ---------------------------------------------------

mpi <- mpi |>
  dplyr::mutate( #Criando atributos que não estão nativamente no objeto
    pobre_33 = fifelse(score > 0.33, 1, 0),
    score_c  = fifelse(score > 0.33, score, 0),
    arranjo  = fcase(
      arranjo2 == 'NN', 1,
      arranjo2 == 'SN', 2,
      arranjo2 == 'SS', 3,
      arranjo2 == 'NS', 4,
      default = NA_real_
    ),
    grupo_sc = fcase(
      score < 0.20, 1,
      score < 0.33, 2,
      score < 0.50, 3,
      score < 1.01, 4,
      default = NA_real_
    ),
    regiao   = as.integer(substr(uf,1,1))
  )
  
## 1A: MPI e decomposição ----
mpi_br <- mpi |>
  dplyr::group_by(ano) |>
  dplyr::summarise(
    H   = weighted.mean(pobre_33, w = peso, na.rm = TRUE),
    MPI = weighted.mean(score_c,  w = peso, na.rm = TRUE)
  ) |>
  dplyr::mutate(A = MPI / H)

mpi_br_long <- mpi_br |>
  dplyr::select(ano, H, A, MPI) |>
  pivot_longer(-ano, names_to = 'indicador', values_to = 'valor') |>
  dplyr::mutate(
    indicador = factor(indicador,
                       levels = c('H','A','MPI'),
                       labels = c('H (Incidência)','A (Intensidade)','MPI = H × A'))
  )

p1_ham <- ggplot(mpi_br_long,
                  aes(x = ano, y = valor,
                      color = indicador, group = indicador)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 4) +
  scale_color_manual(
    values = c('H (Incidência)'  = '#E41A1C',
               'A (Intensidade)' = '#377EB8',
               'MPI = H × A'     = '#4DAF4A')
  ) +
  scale_y_continuous(limits = c(0, 1), labels = percent_format(accuracy = 1)) +
  labs(
    title    = 'Evolução de MPI e componentes, Brasil (1980-2010)',
    subtitle = 'k = 1/3 | pesos amostrais aplicados',
    x = NULL, y = NULL, color = NULL
  )

p1_ham


## 1B: H e A por Região ----
mpi_reg <- mpi |>
  dplyr::group_by(regiao, ano) |>
  dplyr::summarise(
    H       = weighted.mean(pobre_33, w = peso, na.rm = TRUE),
    MPI     = weighted.mean(score_c,  w = peso, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  dplyr::mutate(
    A      = MPI / H,
    regiao = factor(dict$regiao[regiao], levels = dict$regiao),
    ano    = factor(as.character(ano),   levels = names(pal_anos))
    
  )

p2_h <- ggplot(mpi_reg, aes(x = ano, y = H,
                            color = regiao, group = regiao)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 3) +
  scale_color_manual(values = pal_regioes) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = 'Incidência (H) por Região',
       x = NULL, y = NULL, color = NULL, fill = NULL)

p2_a <- ggplot(mpi_reg, aes(x = ano, y = A,
                            color = regiao, group = regiao)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 3) +
  scale_color_manual(values = pal_regioes) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = 'Intensidade (A) por Região',
       x = NULL, y = NULL, color = NULL, fill = NULL)

p2_ha_reg <- (p2_h / p2_a) +
  plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom')

p2_ha_reg


## 1C: Mapas de MPI por UF ----

# Fronteiras de 2010 para todos os anos (consistência visual).
shp_br <- geobr::read_state(year = 2010, showProgress = FALSE)

mpi_uf <- mpi |>
  dplyr::group_by(uf, ano) |>
  dplyr::summarise(MPI     = weighted.mean(score_c, w = peso, na.rm = TRUE),
            .groups = 'drop') |>
  dplyr::rename(code_state = uf)

mpi_uf_geo <- shp_br |>
  dplyr::left_join(mpi_uf, by = 'code_state')

lim_mpi <- range(mpi_uf_geo$MPI, na.rm = TRUE)

p3_mapas <- ggplot(mpi_uf_geo) +
  geom_sf(aes(fill = MPI), color = 'white', linewidth = 0.15) +
  scale_fill_gradientn(
    colours  = RColorBrewer::brewer.pal(9, 'YlOrRd'),
    values   = c(0, 0.04, 0.10, 0.20, 0.50, 0.80, 0.90, 0.96, 1),
    limits   = lim_mpi,   # piso determinado pelos limites reais da base
    na.value = 'grey85',
    labels   = percent_format(accuracy = 1),
    name     = 'MPI',
    guide    = guide_colorbar(
      barwidth = 10, barheight = 0.5,
      title.position = 'top', title.hjust = 0.5
    )
  ) +
  facet_wrap(~ano, ncol = 2) +
  labs(
    title    = 'MPI por Unidade da Federação',
    subtitle = 'k = 1/3 | NA = estado não existia ou sem dado'
  ) +
  theme_void(base_size = 10) +
  theme(
    plot.title      = element_text(face = 'bold', size = 12),
    plot.subtitle   = element_text(color = 'grey40', size = 8),
    legend.position = 'bottom',
    strip.text      = element_text(face = 'bold', size = 11)
  )

p3_mapas


## 1D: Privação média por dimensão ----
priv_dim <- bind_rows(
  calc_dim(mpi, d1, dim_nomes['d1']),
  calc_dim(mpi, d2, dim_nomes['d2']),
  calc_dim(mpi, d3, dim_nomes['d3']),
  calc_dim(mpi, d4, dim_nomes['d4']),
  calc_dim(mpi, d5, dim_nomes['d5'])
) |>
  dplyr::mutate(dim = factor(dim, levels = unname(dim_nomes)))

p4_priv <- ggplot(priv_dim,
                  aes(x = ano, y = media, color = dim, group = dim)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_color_brewer(palette = 'Set1') +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = 'Privação média por dimensão: Brasil (1980-2010)',
    subtitle = 'Média ponderada do score dimensional (0 = sem privação, 1 = privação máxima)',
    x = NULL, y = 'Score médio', color = NULL, fill = NULL
  )

p4_priv


## 1E: Contribuição dimensional por Região e Ano ----
contrib_reg <- bind_rows(
  calc_contrib(mpi, d1, dim_nomes['d1']),
  calc_contrib(mpi, d2, dim_nomes['d2']),
  calc_contrib(mpi, d3, dim_nomes['d3']),
  calc_contrib(mpi, d4, dim_nomes['d4']),
  calc_contrib(mpi, d5, dim_nomes['d5'])
) |>
  dplyr::group_by(regiao, ano) |>
  dplyr::mutate(contrib_pct = round(valor / sum(valor) * 100, 1)) |>
  dplyr::ungroup()

tab5_wide <- contrib_reg |>
  dplyr::select(regiao, ano, dim, contrib_pct) |>
  pivot_wider(names_from = dim, values_from = contrib_pct) |>
  dplyr::arrange(ano, regiao)

tab5_gt <- tab5_wide |>
  dplyr::mutate(regiao = dict$regiao[regiao]) |>
  gt(groupname_col = 'ano', rowname_col = 'regiao') |>
  tab_header(
    title    = 'Contribuição (%) de cada dimensão ao MPI',
    subtitle = 'Por Região e Ano Censitário | k = 1/3'
  ) |>
  tab_stubhead(label = 'Região') |>
  fmt_number(columns = where(is.numeric), decimals = 1) |>
  tab_options(
    row_group.font.weight    = 'bold',
    column_labels.font.weight = 'bold'
  )

tab5_gt


## 1F: Densidades do score contínuo ----

# Score: todos os domicílios, por ano
p6a_score_ano <- mpi |>
  dplyr::filter(!is.na(score)) |>
  norm_peso(ano) |>
  dplyr::mutate(ano = factor(ano)) |>
  ggplot(aes(x = score, weight = w,
             color = ano, fill = ano)) +
  geom_density(alpha = 0.15, linewidth = 0.9, bw = 1/27) +
  scale_color_manual(values = pal_anos) +
  scale_fill_manual(values  = pal_anos) +
  scale_x_continuous(limits = c(0, 1),
                     labels = percent_format(accuracy = 1)) +
  labs(
    title    = 'Distribuição do score contínuo de pobreza multidimensional, Brasil (1980-2010)',
    subtitle = 'Densidade ponderada | todos os domicílios',
    x = 'Score de privação', y = 'Densidade',
    color = NULL, fill = NULL
  )

p6a_score_ano

# Score: por região (malha 2×2)
p6b_score_reg <- wrap_plots(
  make_dens_score_reg('1980', '1980'),
  make_dens_score_reg('1991', '1991'),
  make_dens_score_reg('2000', '2000'),
  make_dens_score_reg('2010', '2010'),
  ncol = 2
) +
  plot_annotation(
    title    = 'Distribuição do score de pobreza multidimensional por Região e Ano',
    subtitle = 'Densidade ponderada | todos os domicílios',
    theme    = theme(plot.title    = element_text(face = 'bold'),
                     plot.subtitle = element_text(color = 'grey40', size = 9))
  ) +
  plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom')

p6b_score_reg


## 1G: Densidades do score entre os pobres ----

# Score entre pobres, por ano
p7a_mpi_ano <- mpi |>
  dplyr::filter(pobre_33 == 1, !is.na(score)) |>
  norm_peso(ano) |>
  dplyr::mutate(ano = factor(ano)) |>
  ggplot(aes(x = score, weight = w,
             color = ano, fill = ano)) +
  geom_density(alpha = 0.15, linewidth = 0.9, bw = 1/27) +
  scale_color_manual(values = pal_anos) +
  scale_fill_manual(values  = pal_anos) +
  scale_x_continuous(limits = c(1/3, 1),
                     labels = percent_format(accuracy = 1)) +
  labs(
    title    = 'Distribuição do score multidimensional entre pobres, Brasil (1980-2010)',
    subtitle = 'Domicílios com score ≥ 1/3 (pobre_33 = 1) | densidade ponderada',
    x = 'Score de privação', y = 'Densidade',
    color = NULL, fill = NULL
  )

p7a_mpi_ano

# Score entre pobres, por região (malha 2×2)
p7b_mpi_reg <- wrap_plots(
  make_dens_mpi_reg('1980', '1980'),
  make_dens_mpi_reg('1991', '1991'),
  make_dens_mpi_reg('2000', '2000'),
  make_dens_mpi_reg('2010', '2010'),
  ncol = 2
) +
  plot_annotation(
    title    = 'Distribuição do score multidimensional entre pobres por Região e Ano',
    subtitle = 'Domicílios com score ≥ 1/3 | densidade ponderada',
    theme    = theme(plot.title    = element_text(face = 'bold'),
                     plot.subtitle = element_text(color = 'grey40', size = 9))
  ) +
  plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom')

p7b_mpi_reg


## 1H: Distribuição do score por grupos: gráfico de barras ----
grupos_ano <- mpi |>
  dplyr::filter(!is.na(grupo_sc)) |>
  norm_peso(ano) |>
  dplyr::group_by(ano, grupo_sc) |>
  dplyr::summarise(prop = sum(w), .groups = 'drop') |>
  dplyr::mutate(
    grupo_sc = factor(dict$grupo_sc[grupo_sc], levels = dict$grupo_sc),
    ano      = factor(ano)
  )

p8_grupos <- ggplot(grupos_ano,
                    aes(x = ano, y = prop,
                        fill = grupo_sc, group = grupo_sc)) +
  geom_col(position = position_dodge(0.78), width = 0.68) +
  scale_fill_manual(
    values = c(
      'Não Pobre'  = '#1A9641',
      'Vulnerável' = '#A6D96A',
      'Pobre'      = '#FDAE61',
      'Ext. Pobre' = '#D7191C'
    )
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = 'Distribuição do score por grupos, Brasil (1980-2010)',
    subtitle = 'Proporção de domicílios em cada faixa',
    x = NULL, y = 'Proporção de domicílios', fill = 'Faixa de score'
  )

p8_grupos


## 1I: Dominância estocástica (ECDFs ponderadas) ----

# Dominância por ano
ecdf_anos <- wecdf_tbl(mpi, 'score', 'peso', 'ano') |>
  dplyr::mutate(ano = factor(ano))

p9a_dom_ano <- ggplot(ecdf_anos,
                       aes(x = score_val, y = prob, color = ano)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = pal_anos) +
  scale_x_continuous(limits = c(0, 1),
                     labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = 'Dominância estocástica por Ano, Brasil (1980-2010)',
    subtitle = 'ECDF ponderada do score | curva mais alta = menos privação (1ª ordem)',
    x = 'Score de privação', y = 'F(score)', color = NULL
  )

p9a_dom_ano

# Dominância por região (malha 2×2)
p9b_dom_reg <- wrap_plots(
  make_dom_reg('1980', '1980'), make_dom_reg('1991', '1991'),
  make_dom_reg('2000', '2000'), make_dom_reg('2010', '2010'),
  ncol = 2
) +
  plot_annotation(
    title    = 'Dominância estocástica por Região, Brasil (1980-2010)',
    subtitle = 'ECDF ponderada | um painel por ano',
    theme    = theme(plot.title    = element_text(face = 'bold'),
                     plot.subtitle = element_text(color = 'grey40', size = 9))
  ) +
  plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom')

p9b_dom_reg

# Dominância por arranjo familiar (malha 2×2)
p9c_dom_arr <- wrap_plots(
  make_dom_arr('1980', '1980'), make_dom_arr('1991', '1991'),
  make_dom_arr('2000', '2000'), make_dom_arr('2010', '2010'),
  ncol = 2
) +
  plot_annotation(
    title    = 'Dominância estocástica por Arranjo Familiar, Brasil (1980-2010)',
    subtitle = 'ECDF ponderada | um painel por ano | categoria \'Outros\' excluída',
    theme    = theme(plot.title    = element_text(face = 'bold'),
                     plot.subtitle = element_text(color = 'grey40', size = 9))
  ) +
  plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom')

p9c_dom_arr


## 1J: H, A, MPI por atributos sociodemográficos ----

# Localização (urbano / rural)
mpi_urb <- mpi |>
  dplyr::group_by(urbano, ano) |>
  dplyr::summarise(
    H   = weighted.mean(pobre_33, w = peso, na.rm = TRUE),
    MPI = weighted.mean(score_c,  w = peso, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  dplyr::mutate(
    A      = MPI / H,
    urbano = factor(dict$urbano[urbano], levels = dict$urbano),
    ano    = factor(ano)
  )

p_urb_h <- ggplot(mpi_urb, aes(x = ano, y = H,
                               color = urbano, group = urbano)) +
  geom_line(linewidth = 0.9) + geom_point(size = 3) +
  scale_color_manual(values = pal_urbano) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = 'Incidência (H)', x = NULL, y = NULL, color = NULL)

p_urb_a <- ggplot(mpi_urb, aes(x = ano, y = A,
                               color = urbano, group = urbano)) +
  geom_line(linewidth = 0.9) + geom_point(size = 3) +
  scale_color_manual(values = pal_urbano) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = 'Intensidade (A)', x = NULL, y = NULL, color = NULL)

p_urb_mpi <- ggplot(mpi_urb, aes(x = ano, y = MPI,
                                 color = urbano, group = urbano)) +
  geom_line(linewidth = 0.9) + geom_point(size = 3) +
  scale_color_manual(values = pal_urbano) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = 'MPI', x = NULL, y = NULL, color = NULL)

p10a_urb <- (p_urb_h | p_urb_a) / p_urb_mpi +
  plot_layout(guides = 'collect') +
  plot_annotation(
    title    = 'Evolução do MPI por situação de domicílio',
    subtitle = 'k = 1/3 | pesos amostrais aplicados'
  ) &
  theme(legend.position = 'bottom')

p10a_urb

#Sexo do responsável
mpi_sexo <- mpi |>
  dplyr::group_by(sexo, ano) |>
  dplyr::summarise(
    H   = weighted.mean(pobre_33, w = peso, na.rm = TRUE),
    MPI = weighted.mean(score_c,  w = peso, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  dplyr::mutate(
    A    = MPI / H,
    sexo = factor(dict$sexo[sexo], levels = dict$sexo),
    ano  = factor(ano)
  )

p10b_sexo <- mpi_sexo |>
  pivot_longer(c(H, A, MPI), names_to = 'indicador', values_to = 'valor') |>
  dplyr::mutate(indicador = factor(indicador,
                                   levels = c('H', 'A', 'MPI'),
                                   labels = c('H (Incidência)', 'A (Intensidade)', 'MPI'))) |>
  ggplot(aes(x = ano, y = valor, color = sexo, group = sexo)) +
  geom_line(linewidth = 0.9) + geom_point(size = 3) +
  scale_color_manual(values = pal_sexo) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  facet_wrap(~indicador, scales = 'free_y') +
  labs(
    title    = 'MPI por Sexo do Responsável',
    subtitle = 'k = 1/3 | pesos amostrais aplicados',
    x = NULL, y = NULL, color = NULL
  )

p10b_sexo

#Raça-cor do responsável
mpi_raca <- mpi |>
  dplyr::filter(!is.na(raca)) |>
  dplyr::group_by(raca, ano) |>
  dplyr::summarise(
    H   = weighted.mean(pobre_33, w = peso, na.rm = TRUE),
    MPI = weighted.mean(score_c,  w = peso, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  dplyr::mutate(
    A    = MPI / H,
    raca = factor(dict$raca[raca], levels = dict$raca),
    ano  = factor(ano)
  )

p10c_raca <- mpi_raca |>
  pivot_longer(c(H, A, MPI), names_to = 'indicador', values_to = 'valor') |>
  dplyr::mutate(indicador = factor(indicador,
                                   levels = c('H', 'A', 'MPI'),
                                   labels = c('H (Incidência)', 'A (Intensidade)', 'MPI'))) |>
  ggplot(aes(x = ano, y = valor, color = raca, group = raca)) +
  geom_line(linewidth = 0.9) + geom_point(size = 3) +
  scale_color_brewer(palette = 'Set1') +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  facet_wrap(~indicador, scales = 'free_y') +
  labs(
    title    = 'MPI por Raça/Cor',
    subtitle = 'k = 1/3 | domicílios com raça NA excluídos',
    x = NULL, y = NULL, color = NULL
  )

p10c_raca

#Arranjo domiciliar
mpi_arr <- mpi |>
  dplyr::filter(!is.na(arranjo)) |>
  dplyr::mutate(arranjo = dict$arranjo[arranjo]) |>
  dplyr::group_by(arranjo, ano) |>
  dplyr::summarise(
    H   = weighted.mean(pobre_33, w = peso, na.rm = TRUE),
    MPI = weighted.mean(score_c,  w = peso, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  dplyr::mutate(
    A       = MPI / H,
    arranjo = factor(arranjo, levels = c('Unipessoal','Casal Sem','Casal Com','Monoparental')),
    ano     = factor(ano)
  )

p10d_arr <- mpi_arr |>
  pivot_longer(c(H, A, MPI), names_to = 'indicador', values_to = 'valor') |>
  dplyr::mutate(indicador = factor(indicador,
                                   levels = c('H', 'A', 'MPI'),
                                   labels = c('H (Incidência)', 'A (Intensidade)', 'MPI'))) |>
  ggplot(aes(x = ano, y = valor, color = arranjo, group = arranjo)) +
  geom_line(linewidth = 0.9) + geom_point(size = 3) +
  scale_color_manual(values = pal_arranjo) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  facet_wrap(~indicador, scales = 'free_y') +
  labs(
    title    = 'MPI por Arranjo Domiciliar',
    subtitle = 'k = 1/3 | pesos amostrais aplicados',
    x = NULL, y = NULL, color = NULL
  )

p10d_arr


## 1K: Interseccionalidade - MPI por arranjo e sexo do responsável ----
mpi_intersec <- mpi |>
  dplyr::filter(!is.na(arranjo2)) |>
  dplyr::group_by(sexo, arranjo2, ano) |>
  dplyr::summarise(
    MPI = weighted.mean(score_c, w = peso, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  dplyr::mutate(
    sexo     = factor(dict$sexo[sexo],         levels = dict$sexo),
    arranjo2 = factor(dict$arranjo2[arranjo2], levels = dict$arranjo2),
    ano      = factor(ano)
  )

lim_intersec <- range(mpi_intersec$MPI, na.rm = TRUE)
mid_intersec <- mean(lim_intersec)

p11_intersec <- ggplot(mpi_intersec,
                       aes(x = ano, y = sexo, fill = MPI)) +
  geom_tile(color = 'white', linewidth = 0.5) +
  geom_text(aes(label = percent(MPI, accuracy = 0.1),
                color = MPI > mid_intersec),
            size = 3.2, fontface = 'bold') +
  scale_fill_distiller(palette   = 'YlOrRd',
                       direction = 1,
                       limits    = lim_intersec,
                       labels    = percent_format(accuracy = 1),
                       name      = 'MPI') +
  scale_color_manual(values = c('TRUE' = 'white', 'FALSE' = 'grey20'),
                     guide  = 'none') +
  facet_wrap(~arranjo2) +
  labs(
    title    = 'Interseccionalidade: MPI por Sexo do responsável e Arranjo Domiciliar',
    subtitle = 'k = 1/3 | pesos amostrais aplicados',
    x = NULL, y = NULL
  )

p11_intersec


## 1L: Decomposição da variação do MPI (ΔH e ΔA) ----

# ΔMPI = MPI_t − MPI_{t−1}; decomposto em:
#   contribuição de H: ΔH × A_{t−1}          (efeito incidência)
#   contribuição de A: H_t × ΔA              (efeito intensidade)
#   resíduo:           ΔH × ΔA

decomp_mpi <- mpi_br |>
  dplyr::arrange(ano) |>
  dplyr::mutate(ano = as.integer(as.character(ano))) |>
  dplyr::mutate(
    delta_MPI = MPI - dplyr::lag(MPI),
    contrib_H = (H - dplyr::lag(H)) * dplyr::lag(A),
    contrib_A = H * (A - dplyr::lag(A)),
    residuo   = (H - dplyr::lag(H)) * (A - dplyr::lag(A)),
    periodo   = if_else(!is.na(dplyr::lag(ano)),
                        paste0(dplyr::lag(ano), '\u2013', ano), NA_character_)
  ) |>
  dplyr::filter(!is.na(periodo)) |>
  dplyr::select(periodo, delta_MPI, contrib_H, contrib_A, residuo) |>
  pivot_longer(-periodo,
               names_to  = 'componente',
               values_to = 'valor') |>
  dplyr::mutate(componente = factor(componente,
                                    levels = c('contrib_H', 'contrib_A', 'residuo', 'delta_MPI'),
                                    labels = c('\u0394H \u00d7 A\u2080', 'H\u2081 \u00d7 \u0394A',
                                               '\u0394H \u00d7 \u0394A', '\u0394MPI total')))

p12_decomp <- ggplot(
  decomp_mpi |> filter(componente != '\u0394MPI total'),
  aes(x = periodo, y = valor, fill = componente)
) +
  geom_col(position = 'stack') +
  geom_point(
    data  = decomp_mpi |> filter(componente == '\u0394MPI total'),
    aes(y = valor),
    shape = 18, size = 5, color = 'black', inherit.aes = TRUE
  ) +
  scale_fill_manual(values = c(
    '\u0394H \u00d7 A\u2080' = '#D6604D',
    'H\u2081 \u00d7 \u0394A' = '#4393C3',
    '\u0394H \u00d7 \u0394A' = '#878787'
  )) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    title    = 'Decomposição da variação do MPI',
    subtitle = 'Contribuição de \u0394H e \u0394A ao \u0394MPI entre períodos | losango = ΔMPI total',
    x = NULL, y = 'Contribuição', fill = NULL
  )

p12_decomp


## 1M: Perfil dimensional por grupo de score ----

# Para cada grupo_sc: privação média por dimensão (d1–d5).
perfil_dim <- mpi |>
  dplyr::filter(!is.na(grupo_sc)) |>
  dplyr::group_by(ano, grupo_sc) |>
  dplyr::summarise(across(d1:d5,
                          ~ weighted.mean(.x, w = peso, na.rm = TRUE)),
                   .groups = 'drop') |>
  dplyr::mutate(ano      = factor(ano),
                grupo_sc = factor(dict$grupo_sc[grupo_sc],
                                  levels = dict$grupo_sc)) |>
  pivot_longer(d1:d5, names_to = 'dim', values_to = 'media') |>
  dplyr::mutate(dim = factor(dim_nomes[dim], levels = unname(dim_nomes)))

p13_perfil <- ggplot(perfil_dim,
                     aes(x = dim, y = media, fill = grupo_sc)) +
  geom_col(position = position_dodge(0.8), width = 0.72) +
  scale_fill_manual(values = c(
    'Não Pobre'  = '#1A9641',
    'Vulnerável' = '#A6D96A',
    'Pobre'      = '#FDAE61',
    'Ext. Pobre' = '#D7191C'
  )) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  facet_wrap(~ano) +
  labs(
    title    = 'Perfil dimensional por grupo de score, Brasil (1980-2010)',
    subtitle = 'Privação média por dimensão dentro de cada grupo, por ano',
    x = NULL, y = 'Privação média', fill = NULL
  ) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

p13_perfil


## 1N: Gini e Lorenz do score (geral e entre pobres) ----

# Índice de Gini
mpi <- mpi |>
  dplyr::mutate(pobre_50 = as.integer(score > 0.5))

gini_geral <- mpi |>
  dplyr::filter(!is.na(score)) |>
  dplyr::group_by(ano) |>
  dplyr::summarise(gini = gini_pond(score, peso), .groups = 'drop') |>
  dplyr::mutate(grupo = 'Geral (todos)')

gini_pobres <- mpi |>
  dplyr::filter(pobre_33 == 1, !is.na(score)) |>
  dplyr::group_by(ano) |>
  dplyr::summarise(gini = gini_pond(score, peso), .groups = 'drop') |>
  dplyr::mutate(grupo = 'Pobres (k > 1/3)')

gini_so_pobres <- mpi |>
  dplyr::filter(pobre_33 == 1, pobre_50 == 0, !is.na(score)) |>
  dplyr::group_by(ano) |>
  dplyr::summarise(gini = gini_pond(score, peso), .groups = 'drop') |>
  dplyr::mutate(grupo = 'Só Pobres (1/3 < k \u2264 1/2)')

gini_ext_pobres <- mpi |>
  dplyr::filter(pobre_50 == 1, !is.na(score)) |>
  dplyr::group_by(ano) |>
  dplyr::summarise(gini = gini_pond(score, peso), .groups = 'drop') |>
  dplyr::mutate(grupo = 'Ext. Pobres (k > 1/2)')

gini_comb <- dplyr::bind_rows(gini_geral, gini_pobres, gini_so_pobres, gini_ext_pobres) |>
  dplyr::mutate(grupo = factor(grupo,
                               levels = c('Geral (todos)', 'Pobres (k > 1/3)',
                                          'Só Pobres (1/3 < k \u2264 1/2)', 'Ext. Pobres (k > 1/2)')))

p14a_gini <- ggplot(gini_comb,
                    aes(x = factor(ano), y = gini, color = grupo, group = grupo)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  scale_color_manual(values = c(
    'Geral (todos)'                    = 'grey40',
    'Pobres (k > 1/3)'                 = '#FDAE61',
    'Só Pobres (1/3 < k \u2264 1/2)'   = '#74ADD1',
    'Ext. Pobres (k > 1/2)'            = '#D7191C'
  )) +
  labs(
    title    = 'Gini do score, Brasil (1980-2010)',
    subtitle = 'Desigualdade geral vs. desigualdade interna da pobreza, por subgrupo',
    x = NULL, y = 'Índice de Gini', color = NULL
  )

p14a_gini


# Curva de Lorenz: pobres
lorenz_pobres <- mpi |>
  filter(pobre_33 == 1, !is.na(score)) |>
  group_by(ano) |>
  arrange(score, .by_group = TRUE) |>
  mutate(
    w_norm = peso / sum(peso),
    cumpop = cumsum(w_norm),
    cumscr = cumsum(score * w_norm) / sum(score * w_norm)
  ) |>
  ungroup() |>
  mutate(ano = factor(ano))

p14b_lorenz <- ggplot(lorenz_pobres,
                   aes(x = cumpop, y = cumscr, color = ano)) +
  geom_line(linewidth = 0.9) +
  geom_abline(slope = 1, intercept = 0,
              linetype = 'dashed', color = 'grey50') +
  scale_color_manual(values = pal_anos) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = 'Curva de Lorenz do score: pobres',
    subtitle = 'Desigualdade interna da intensidade de privação | k = 1/3',
    x = 'Proporção acumulada de domicílios',
    y = 'Proporção acumulada do score',
    color = NULL
  )

p14b_lorenz


# -- Etapa 2: Exportação de objetos ---------------------------------------------

dir_out <- 'output/01_descriptive'
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

## Gráficos
fig_list <- list(
  fig01_ham_brasil         = list(p = p1_ham,          w = 16,   h = 12),
  fig02_ha_regiao          = list(p = p2_ha_reg,       w = 16,   h = 12),
  fig03_mapas_uf           = list(p = p3_mapas,        w = 16,   h = 12),
  fig04_priv_dim           = list(p = p4_priv,         w = 16,   h = 12),
  fig05_dens_score_ano     = list(p = p6a_score_ano,   w = 16,   h = 12),
  fig06_dens_score_reg     = list(p = p6b_score_reg,   w = 16,   h = 12),
  fig07_dens_mpi_ano       = list(p = p7a_mpi_ano,     w = 16,   h = 12),
  fig08_dens_mpi_reg       = list(p = p7b_mpi_reg,     w = 16,   h = 12),
  fig09_grupos_score       = list(p = p8_grupos,       w = 16,   h = 12),
  fig10_dom_ano            = list(p = p9a_dom_ano,     w = 16,   h = 12),
  fig11_dom_regiao         = list(p = p9b_dom_reg,     w = 16,   h = 12),
  fig12_dom_arranjo        = list(p = p9c_dom_arr,     w = 16,   h = 12),
  fig13_mpi_urbano         = list(p = p10a_urb,        w = 16,   h = 12),
  fig14_mpi_sexo           = list(p = p10b_sexo,       w = 16,   h = 12),
  fig15_mpi_raca           = list(p = p10c_raca,       w = 16,   h = 12),
  fig16_mpi_arranjo        = list(p = p10d_arr,        w = 16,   h = 12),
  fig17_intersec           = list(p = p11_intersec,    w = 16,   h = 12),
  fig18_decomp_delta_mpi   = list(p = p12_decomp,      w = 16,   h = 12),
  fig19_perfil_dim         = list(p = p13_perfil,      w = 16,   h = 12),
  fig20_gini_pobres        = list(p = p14a_gini,       w = 16,   h = 12),
  fig21_lorenz_pobres      = list(p = p14b_lorenz,     w = 16,   h = 12)
)

walk2(names(fig_list), fig_list, function(nome, cfg) {
  ggsave(
    filename = file.path(dir_out, paste0(nome, '.png')),
    plot     = cfg$p,
    width    = cfg$w,
    height   = cfg$h,
    dpi      = 300
  )
  message('Salvo: ', nome)
})

## Tabela dimensional
gtsave(tab5_gt, file.path(dir_out, 'tab01_contrib_dim.html'))
