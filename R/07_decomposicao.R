#-------------------------------------------------------------------------------#
#                                                                               #
#                  REVISITANDO A TESE DE DOUTORADO - PROJETO T                  #
#                                                                               #
#-------------------------------------------------------------------------------#

# ================== FASE 7 - Visões gráficas complementares ================== #

source('R/00_packages.R')
set.seed(2026)

mpi_original     <- read_parquet('data/04_output/mpi.parquet')
mpi_simplificado <- read_parquet('data/04_output/mpi_simplificado.parquet')
mpi_regioes      <- read_parquet('data/04_output/mpi_rims.parquet')
mpi_amostrado    <- read_parquet('data/04_output/mpi_sample.parquet')
dict             <- readRDS('data/04_output/dict.rds')

cutoffs <- list(pobre_10 = 1/10, pobre_20 = 1/5, pobre_25 = 1/4,
                pobre_33 = 1/3,  pobre_40 = 2/5, pobre_50 = 1/2, pobre_67 = 2/3)

pal_anos <- c( '1980' = '#2166AC', '1991' = '#4DAF4A', '2000' = '#FF7F00', '2010' = '#E41A1C' )

pal_regioes <- c('Norte' = '#2166AC', 'Nordeste' = '#FF7F00', 'Sudeste' = '#7570B3',
                 'Sul' = '#E7298A', 'Centro-Oeste' = '#4DAF4A')

dim_nomes <- c(
  'd1' = 'D1: Moradia',
  'd2' = 'D2: Infraestrutura',
  'd3' = 'D3: Renda',
  'd4' = 'D4: Educação',
  'd5' = 'D5: Trabalho'
)


# -- Etapa 1: Robustez ao cutoff -----------------------------------------------

# Robustez por ano
robus_mpi <- imap_dfr(cutoffs, function(k_val, k_var) {
  mpi_original |>
    group_by(ano) |>
    summarise(
      H   = weighted.mean(.data[[k_var]], w = peso, na.rm = TRUE),
      MPI = weighted.mean(score * .data[[k_var]], w = peso, na.rm = TRUE),
      .groups = 'drop'
    ) |>
    mutate(A = MPI / H, k = k_val)
})
gc()

p_rob_h <- ggplot(robus_mpi,
                  aes(x = k, y = H,
                      color = factor(ano), group = ano)) +
  geom_line(linewidth = 0.9) + geom_point(size = 2.5) +
  geom_vline(xintercept = 1/3, linetype = 'dashed', color = 'grey40') +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    breaks = unlist(cutoffs)
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = 'Incidência (H)',
       x = 'Cutoff k', y = 'H (Incidência)', color = NULL)

p_rob_a <- ggplot(robus_mpi,
                  aes(x = k, y = A,
                      color = factor(ano), group = ano)) +
  geom_line(linewidth = 0.9) + geom_point(size = 2.5) +
  geom_vline(xintercept = 1/3, linetype = 'dashed', color = 'grey40') +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    breaks = unlist(cutoffs)
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = 'Intensidade (A)',
       x = 'Cutoff k', y = 'A (Intensidade)', color = NULL)

p_rob_mpi <- ggplot(robus_mpi,
                    aes(x = k, y = MPI,
                        color = factor(ano), group = ano)) +
  geom_line(linewidth = 0.9) + geom_point(size = 2.5) +
  geom_vline(xintercept = 1/3, linetype = 'dashed', color = 'grey40') +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    breaks = unlist(cutoffs)
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = 'MPI',
       x = 'Cutoff k', y = 'MPI', color = NULL)

p_robust <- ((p_rob_h | p_rob_a) / p_rob_mpi) +
  plot_layout(guides = 'collect') +
  plot_annotation(
    title    = 'Análise de robustez ao cutoff k',
    subtitle = 'H, A e MPI em sete thresholds | linha tracejada = k = 1/3 (cutoff padrão)'
  ) &
  theme(legend.position = 'bottom')

# Robustez por região
anos_rob   <- c(1980, 1991, 2000, 2010)
plots_rob  <- list()

for (ano_i in anos_rob) {
  
  robus_reg <- imap_dfr(cutoffs, function(k_val, k_var) {
    mpi_original |>
      filter(ano == ano_i) |>
      group_by(regiao) |>
      summarise(
        MPI = weighted.mean(score * .data[[k_var]], w = peso, na.rm = TRUE),
        .groups = 'drop'
      ) |>
      mutate(
        regiao = factor(dict$regiao[regiao], levels = dict$regiao),
        k      = k_val
      )
  })
  
  plots_rob[[as.character(ano_i)]] <- ggplot(robus_reg,
                                             aes(x = k, y = MPI,
                                                 color = regiao, group = regiao)) +
    geom_line(linewidth = 0.9) + geom_point(size = 2.5) +
    geom_vline(xintercept = 1/3, linetype = 'dashed', color = 'grey40') +
    scale_color_manual(values = pal_regioes) +
    scale_x_continuous(
      labels = percent_format(accuracy = 1),
      breaks = unlist(cutoffs)
    ) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title    = as.character(ano_i),
      subtitle = 'Cruzamento indica inversão de ranking',
      x = 'Cutoff k', y = 'MPI', color = NULL
    )
}

p_regs <- ((
  (plots_rob[['1980']] | plots_rob[['1991']]) /
    (plots_rob[['2000']] | plots_rob[['2010']]) +
    plot_layout(guides = 'collect') +
    plot_annotation(
      title    = 'Robustez do ranking regional ao cutoff k (1980-2010)',
      subtitle = 'Cruzamento de curvas indica inversão de ranking'
    )
) &
  theme(legend.position = 'bottom'))

# Robustez por arranjo domiciliar (cutoff fixo)
tipos_rob <- imap_dfr(cutoffs['pobre_33'], function(k_val, k_var) {
  mpi_original |>
    mutate(tipo_fam = paste(sexo, arranjo2, sep = '_')) |>
    group_by(ano, tipo_fam) |>
    summarise(
      H   = weighted.mean(.data[[k_var]],          w = peso, na.rm = TRUE),
      MPI = weighted.mean(score * .data[[k_var]],  w = peso, na.rm = TRUE),
      .groups = 'drop'
    ) |>
    mutate(A = MPI / H)
}) |>
  pivot_longer(c(H, A, MPI), names_to = 'metrica', values_to = 'valor') |>
  mutate(
    metrica       = factor(metrica,
                           levels = c('H', 'A', 'MPI'),
                           labels = c('H (Incidência)', 'A (Intensidade)', 'MPI')),
    sexo_label    = if_else(str_starts(tipo_fam, '1_'), 'Homem', 'Mulher'),
    arranjo_code  = str_sub(tipo_fam, 3),
    arranjo_label = factor(case_when(
      arranjo_code == 'SS' ~ 'Casal Com Filhos',
      arranjo_code == 'SN' ~ 'Casal Sem Filhos',
      arranjo_code == 'NS' ~ 'Monoparental',
      arranjo_code == 'NN' ~ 'Unipessoal'
    ), levels = c('Monoparental', 'Casal Com filhos',
                  'Casal Sem filhos', 'Unipessoal'))
  )

p_fams <- ggplot(tipos_rob, aes(x = factor(ano), y = sexo_label, fill = valor)) +
  geom_tile(color = 'white', linewidth = 0.5) +
  geom_text(aes(label = scales::percent(valor, accuracy = 0.1),
                color = valor > 0.30),
            size = 2.8) +
  scale_color_manual(values = c('TRUE' = 'white', 'FALSE' = 'grey15'),
                     guide  = 'none') +
  facet_nested(arranjo_label ~ metrica,
               scales    = 'free',
               space     = 'free',
               nest_line = element_line(color = 'grey60')) +
  scale_fill_viridis_c(option = 'magma', direction = -1,
                       labels = percent_format(accuracy = 1)) +
  labs(
    title    = 'H, A e MPI por tipo de família (1980-2010)',
    subtitle = 'k = 1/3 | 2 sexos × 4 arranjos',
    x = NULL, y = NULL, fill = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid   = element_blank(),
    strip.text.y = element_text(angle = 0, hjust = 0, size = 8),
    axis.text.y  = element_text(size = 8)
  )


# -- Etapa 2: Co-ocorrência de Privações ---------------------------------------

# Matriz para o período inteiro
cooc_data <- mpi_amostrado |>
  filter(!is.na(score)) |>
  dplyr::select(d1:d5) |>
  mutate(across(everything(), as.numeric))

cor_mat <- cor(cooc_data, use = 'pairwise.complete.obs')
rownames(cor_mat) <- colnames(cor_mat) <- unname(dim_nomes)

cor_long <- cor_mat |>
  as.data.frame() |>
  rownames_to_column('dim1') |>
  pivot_longer(-dim1, names_to = 'dim2', values_to = 'cor') |>
  mutate(
    dim1 = factor(dim1, levels = unname(dim_nomes)),
    dim2 = factor(dim2, levels = rev(unname(dim_nomes)))
  )

p_cooc <- ggplot(cor_long, aes(x = dim1, y = dim2, fill = cor)) +
  geom_tile(color = 'white', linewidth = 0.6) +
  geom_text(aes(label = number(cor, accuracy = 0.01)), size = 3.5) +
  scale_fill_distiller(
    palette  = 'RdBu',
    direction = -1,
    limits   = c(-1, 1),
    name     = 'Correlação'
  ) +
  labs(
    title    = 'Co-ocorrência de privações dimensionais',
    subtitle = 'Correlação de Pearson entre indicadores binários d(i) | amostra 200K',
    x = NULL, y = NULL
  ) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# Evolução da co-ocorrência por ano
cooc_ano <- map_dfr(c(1980, 1991, 2000, 2010), function(a) {
  sub <- mpi_amostrado |>
    filter(ano == a, !is.na(score)) |>
    dplyr::select(d1:d5) |>
    mutate(across(everything(), as.numeric))
  
  cor(sub, use = 'pairwise.complete.obs') |>
    as.data.frame() |>
    rownames_to_column('dim1') |>
    pivot_longer(-dim1, names_to = 'dim2', values_to = 'cor') |>
    mutate(ano = a)
}) |>
  filter(dim1 < dim2) |>  # somente triângulo superior (pares únicos)
  mutate(
    par = paste(dim_nomes[dim1], '×', dim_nomes[dim2]),
    ano = factor(ano)
  )

p_cooc_ano <- ggplot(cooc_ano,
                     aes(x = ano, y = cor, color = par, group = par)) +
  geom_line(linewidth = 0.8) + geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey50') +
  scale_color_manual(values = colorRampPalette(RColorBrewer::brewer.pal(8, 'Set2'))(10)) +
  labs(
    title    = 'Evolução da co-ocorrência dimensional',
    subtitle = 'Correlação entre pares de privações por ano | amostra ~250K por ano',
    x = NULL, y = 'Correlação de Pearson', color = NULL
  )


# -- Etapa 3: Decomposição Oaxaca-Blinder --------------------------------------

# Decompõe a diferença média de score entre dois grupos em:
#   (a) Dotações (características observáveis — explicado)
#   (b) Coeficientes (retornos — não explicado / "discriminação")

# Preparando a base
oax_data <- mpi_amostrado |>
  filter(!is.na(score) & arranjo2 != 'NN') |>
  mutate(
    regiao       = as.integer(substr(uf, 1, 1)), 
    urbano_b     = as.integer(urbano   == 1),
    sexo_b       = as.integer(sexo     == 1),
    branco_b     = as.integer(raca     == 1),
    mono_b       = as.integer(arranjo2 == 'NS')
  )

# Comparações
comparacoes <- list(
  BxN = list( # Brancos X Negros
    formula   = score ~ factor(urbano) + factor(sexo) + factor(arranjo2) + log_rpcr + factor(regiao) | branco_b,
    filtro_fn = function(d) filter(d, raca %in% c(1L, 2L, 4L))  # exclui Amarela e Indígena
  ),
  UxR = list( # Urbano x Rural
    formula   = score ~ factor(raca) + factor(sexo) + factor(arranjo2) + log_rpcr + factor(regiao) | urbano_b,
    filtro_fn = function(d) filter(d, !is.na(urbano_b))
  ),
  CxM = list( # Casal com X Monoparental
    formula   = score ~ factor(urbano) + factor(sexo) + factor(raca) + log_rpcr + factor(regiao) | mono_b,
    filtro_fn = function(d) filter(d, arranjo2 %in% c('SS', 'NS'))  # exclui Unipessoal e casal sem filho
  )
)

# Computando as decomposições
anos_oax    <- c(1980, 1991, 2000, 2010)
oax_results <- list()

for (comp in names(comparacoes)) {
  
  spec       <- comparacoes[[comp]]
  dados_comp <- spec$filtro_fn(oax_data) |> na.omit()
  oax_results[[comp]] <- list()

  for (ano_i in anos_oax) {
    cat('\n====', comp, '—', ano_i, '====\n')
    oax_results[[comp]][[as.character(ano_i)]] <- oaxaca(
      formula = spec$formula,
      data    = filter(dados_comp, ano == ano_i),
      #weights = dados_comp$peso,
      R       = 10
    )
  }
}

# Extração de componentes twofold
extrair_oax <- function(mod, comp, ano) {
  tf    <- mod$twofold$overall
  # group.weight = -1: referência Oaxaca (1973), consistente com o plot original
  linha <- tf[tf[, 'group.weight'] == -1, ]
  
  explicado  <- linha['coef(explained)']
  nao_explic <- linha['coef(unexplained)']
  diferenca  <- explicado + nao_explic
  
  tibble(
    comparacao = comp,
    ano        = ano,
    diferenca  = diferenca,
    explicado  = explicado,
    nao_explic = nao_explic,
    pct_explic = explicado / diferenca,
    pct_nexp   = nao_explic / diferenca
  )
}

# Consolidando os resultados
res_oax <- imap_dfr(oax_results, function(mods_comp, comp) {
  imap_dfr(mods_comp, function(mod, ano) {
    extrair_oax(mod, comp, as.integer(ano))
  })
})

print(res_oax)

# Visualização gráfica
comp_labels <- c(
  'BxN'   = 'Branco × Preto/Pardo',
  'UxR'   = 'Urbano × Rural',
  'CxM'   = 'Biparental x Monoparental'
)

res_clean <- res_oax |>
  mutate(
    instavel = (comparacao == 'HxM' & ano == 1980) |
      (comparacao == 'CxM' & ano == 1991),
    pct_explic = if_else(instavel, NA_real_, pct_explic),
    pct_nexp   = if_else(instavel, NA_real_, pct_nexp),
    comparacao = factor(comp_labels[comparacao], levels = comp_labels)
  ) |>
  dplyr::select(-instavel)

# Gráfico de composição
p_barras <- res_clean |>
  dplyr::select(comparacao, ano, explicado, nao_explic) |>
  pivot_longer(c(explicado, nao_explic),
               names_to  = 'componente',
               values_to = 'valor') |>
  mutate(componente = factor(componente,
                             levels = c('nao_explic', 'explicado'),
                             labels = c('Não explicado', 'Explicado'))) |>
  ggplot(aes(x = factor(ano), y = valor, fill = componente)) +
  geom_col(width = 0.7, na.rm = TRUE) +
  geom_hline(yintercept = 0, color = 'grey30', linewidth = 0.3) +
  scale_fill_manual(values = c('Explicado'     = '#2166AC',
                               'Não explicado' = '#D7191C')) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  facet_wrap(~comparacao, scales = 'free_y', ncol = 1) +
  labs(
    title    = 'Decomposição Oaxaca-Blinder por grupo (1980–2010)',
    subtitle = 'HxM: diferença negativa = mulheres com maior MPI  |  HxM/1980 excluído',
    x = NULL, y = 'Diferença no score', fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = 'bottom')

# Heatmap
p_heatmap <- res_clean |>
  mutate(comparacao = factor(comp_labels[comparacao],
                             levels = rev(comp_labels))) |>
  ggplot(aes(x = comparacao, y = factor(ano, levels = c('1980','1991','2000','2010')),
             fill = pct_nexp)) +
  geom_tile(color = 'white', linewidth = 0.8) +
  geom_text(aes(label = if_else(is.na(pct_nexp), '—',
                                percent(pct_nexp, accuracy = 1)),
                color  = pct_nexp > 0.6),
            size = 3.5) +
  scale_fill_gradient2(
    low      = '#2166AC',
    mid      = 'white',
    high     = '#D7191C',
    midpoint = 0.5,
    labels   = percent_format(accuracy = 1),
    na.value = 'grey85'
  ) +
  scale_y_discrete(limits = rev) +
  scale_color_manual(values = c('TRUE' = 'white', 'FALSE' = 'grey20'),
                     guide  = 'none') +
  labs(
    title    = 'Proporção não explicada: Oaxaca-Blinder (1980–2010)',
    subtitle = 'Vermelho: componente estrutural dominante  |  cinza: estimativa instável',
    x = NULL, y = NULL,
    fill = '% não\nexplicado'
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())

p_oaxaca <- (p_barras | p_heatmap)


# -- Etapa 4: Decomposição dimensional do Delta MPI ----------------------------

pesos_dim <- c(d1 = 2/9, d2 = 2/9, d3 = 2/9, d4 = 2/9, d5 = 1/9)

# Avaliação de contribuição por ano e dimensão
contrib_dim_br <- mpi_original |>
  group_by(ano) |>
  summarise(
    across(
      d1:d5,
      ~ weighted.mean(.x * pesos_dim[cur_column()] * pobre_33,
                      w = peso, na.rm = TRUE),
      .names = 'c_{.col}'
    ),
    MPI = weighted.mean(score_c, w = peso, na.rm = TRUE),
    .groups = 'drop'
  )

# Variação entre períodos consecutivos (em pontos percentuais)
delta_contrib <- contrib_dim_br |>
  mutate(ano = as.integer(as.character(ano))) |>
  arrange(ano) |>
  mutate(
    across(starts_with('c_'), 
           ~ .x - dplyr::lag(.x), 
           .names = 'delta_{.col}'),
    periodo = if_else(
      !is.na(dplyr::lag(ano)),
      paste0(dplyr::lag(ano), '\u2013', ano),
      NA_character_
    )
  ) |>
  filter(!is.na(periodo)) |>
  dplyr::select(periodo, starts_with('delta_c_')) |>
  pivot_longer(-periodo, names_to = 'dim', values_to = 'delta') |>
  mutate(
    dim_key   = gsub('delta_c_', '', dim),
    dim_label = factor(dim_nomes[dim_key], levels = unname(dim_nomes))
  )

p_delta_dim <- ggplot(delta_contrib,
                      aes(x = periodo, y = delta, fill = dim_label)) +
  geom_col(position = 'stack') +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey40') +
  scale_fill_brewer(palette = 'Set1') +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    title    = 'Decomposição dimensional da variação do MPI',
    subtitle = 'Contribuição de cada dimensão à queda do MPI entre períodos (p.p.)',
    x = NULL, y = '\u0394MPI (p.p.)', fill = NULL
  )

# Contribuição relativa (%) de cada dimensão à variação total
p_delta_rel <- delta_contrib |>
  group_by(periodo) |>
  mutate(pct = delta / sum(delta) * 100) |>
  ggplot(aes(x = periodo, y = pct, fill = dim_label)) +
  geom_col(position = 'stack') +
  scale_fill_brewer(palette = 'Set1') +
  scale_y_continuous(labels = number_format(accuracy = 1, suffix = '%')) +
  labs(
    title    = 'Contribuição relativa por dimensão ao ΔMPI',
    subtitle = '% da queda total atribuída a cada dimensão',
    x = NULL, y = '% do ΔMPI', fill = NULL
  )


# -- EXPORTAÇÃO ----------------------------------------------------------------

# Exportando os gráficos
dir_out <- 'output/04_decomposition'
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

## Gráficos
fig_list <- list(
  fig01_robustez               = list(p = p_robust,       w = 16,   h = 12),
  fig01a_robustez_regioes      = list(p = p_regs,         w = 16,   h = 12),
  fig01b_robustez_arranjos     = list(p = p_fams,         w = 16,   h = 12),
  fig02a_coocorrencia_geral    = list(p = p_cooc,         w = 16,   h = 12),
  fig02b_coocorrencia_anos     = list(p = p_cooc_ano,     w = 16,   h = 12),
  fig03_oaxaca                 = list(p = p_oaxaca,       w = 16,   h = 12),
  fig04_decomposicao_delta     = list(p = p_delta_dim,    w = 16,   h = 12)
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
