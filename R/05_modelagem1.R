#-------------------------------------------------------------------------------#
#                                                                               #
#                  REVISITANDO A TESE DE DOUTORADO - PROJETO T                  #
#                                                                               #
#-------------------------------------------------------------------------------#


# ================= FASE 5 - Mogelagem (1): Machine Learning ================== #

source('R/00_packages.R')
mpi_original     <- read_parquet('data/04_output/mpi.parquet')
mpi_simplificado <- read_parquet('data/04_output/mpi_simplificado.parquet')
mpi_regioes      <- read_parquet('data/04_output/mpi_rims.parquet')
mpi_amostrado    <- read_parquet('data/04_output/mpi_sample.parquet')

set.seed(2026)


## -- Modelo 1: PCA sobre indicadores do MPI ----
 
# Objetivo: verificar independência das dimensões do MPI-LA.
# Resultado relevante para discussão metodológica na tese.

# Atributos para PCA
vars_pca <- mpi_original |> dplyr::select(d11:d52) |> names()

# Preparando PCA ponderado
pca_ponderado <- function(df, vars, peso_col = 'peso') {
  
  mat <- df |>
    dplyr::select(all_of(vars)) |>
    mutate(across(everything(), as.numeric)) |>
    as.matrix()
  
  ok  <- complete.cases(mat)
  mat <- mat[ok, ]
  w   <- df[[peso_col]][ok]
  w   <- w / sum(w)
  
  wmeans <- colSums(mat * w)
  wsd    <- sqrt(colSums(sweep(mat, 2, wmeans)^2 * w))
  
  # Remover indicadores constantes no período (variância zero)
  ok_cols <- wsd > 0
  if (any(!ok_cols))
    warning('Indicadores removidos por variância zero: ',
            paste(colnames(mat)[!ok_cols], collapse = ', '))
  
  mat    <- mat[, ok_cols]
  wmeans <- wmeans[ok_cols]
  wsd    <- wsd[ok_cols]
  
  mat_wt <- sweep(sweep(mat, 2, wmeans), 2, wsd, '/') * sqrt(w)
  res    <- prcomp(mat_wt, center = FALSE, scale. = FALSE)
  
  attr(res, 'wmeans') <- wmeans
  attr(res, 'wsd')    <- wsd
  res
}

# Projetando dados nos eixos do PCA 'global' (todos os anos)
pca_project <- function(pca_obj, df, vars) {
  mat <- df |>
    dplyr::select(all_of(vars)) |>
    mutate(across(everything(), as.numeric)) |>
    as.matrix()
  
  mat_std <- sweep(
    sweep(mat, 2, attr(pca_obj, 'wmeans')),
    2, attr(pca_obj, 'wsd'), '/'
  )
  mat_std %*% pca_obj$rotation
}

# Implementação do PCA ponderado
pca_global <- pca_ponderado(mpi_original, vars_pca, peso_col = 'peso')
gc()


# Análise gráfica geral

## Screeplot (variância explicadapor componente)
p_scree <- fviz_eig(pca_global, addlabels = TRUE,
                    barfill = '#4393C3', barcolor = 'white') +
  labs(title    = 'PCA global: variância por componente',
       subtitle = 'Dados pooled 1980-2010, ponderados')

## Biplot (correlação das variáveis originais com PC1 e PC2)
p_var_bi <- fviz_pca_var(
  pca_global,
  col.var       = 'contrib',
  gradient.cols = c('#92C5DE', '#F4A582', '#D6604D'),
  repel         = TRUE, labelsize = 4
) +
  labs(title    = 'Correlação das dimensões com PC1 e PC2',
       subtitle = 'Cor indica contribuição relativa')

## Gráficos de contribuições para principais componentes
p_contrib_pc1 <- fviz_contrib(pca_global, choice = 'var', axes = 1) +
  labs(title = 'Contribuição de cada indicador ao PC1')

p_contrib_pc2 <- fviz_contrib(pca_global, choice = 'var', axes = 2) +
  labs(title = 'Contribuição de cada indicador ao PC2')

p_contrib_pc3 <- fviz_contrib(pca_global, choice = 'var', axes = 3) +
  labs(title = 'Contribuição de cada indicador ao PC3')

# scores por ano (apresentaão para amostra de 50.000 linhas por ano)
sample_ponderado <- function(n, pesos) {
  cw <- cumsum(pesos)
  u  <- runif(n, 0, cw[length(cw)])
  findInterval(u, cw) + 1L
}

amostra <- mpi_original |>
  group_by(ano) |>
  group_modify(~ .x[sample_ponderado(50000, .x$peso), ]) |>
  ungroup()
gc()

scores_mat <- pca_project(pca_global, amostra, vars_pca)

amostra <- amostra |>
  bind_cols(
    as.data.frame(scores_mat[, 1:3]) |>
      setNames(c('PC1', 'PC2', 'PC3'))
  )

p_dens_pc1 <- ggplot(amostra, aes(x = PC1, fill = factor(ano))) +
  geom_density(alpha = 0.4) +
  scale_fill_brewer(palette = 'RdYlBu', direction = -1) +
  labs(title    = 'Distribuição do PC1 por ano (projeção no PCA global)',
       subtitle = 'Amostra ponderada: 50k domicílios por ano',
       x = 'Score PC1', y = 'Densidade', fill = 'Ano')

p_dens_pc2 <- ggplot(amostra, aes(x = PC2, fill = factor(ano))) +
  geom_density(alpha = 0.4) +
  scale_fill_brewer(palette = 'RdYlBu', direction = -1) +
  labs(title    = 'Distribuição do PC2 por ano (projeção no PCA global)',
       subtitle = 'Amostra ponderada: 50k domicílios por ano',
       x = 'Score PC2', y = 'Densidade', fill = 'Ano')

p_dens_pc3 <- ggplot(amostra, aes(x = PC3, fill = factor(ano))) +
  geom_density(alpha = 0.4) +
  scale_fill_brewer(palette = 'RdYlBu', direction = -1) +
  labs(title    = 'Distribuição do PC3 por ano (projeção no PCA global)',
       subtitle = 'Amostra ponderada: 50k domicílios por ano',
       x = 'Score PC2', y = 'Densidade', fill = 'Ano')

# Evolução da dimensionalidade por ano
var_temporal <- map_dfr(sort(unique(mpi_original$ano)), function(a) {
  res <- pca_ponderado(
    mpi_original |> filter(ano == a),
    vars_pca,
    peso_col = 'peso'
  )
  imp <- summary(res)$importance
  
  tibble(
    ano   = a,
    PC1   = imp[2, 1] * 100,
    PC2   = imp[2, 2] * 100,
    PC3   = imp[2, 3] * 100,
    acum3 = imp[3, 3] * 100
  )
})

gc()
print(var_temporal)

p_var_temporal <- var_temporal |>
  pivot_longer(c(PC1, PC2, PC3),
               names_to = 'componente', values_to = 'var_exp') |>
  ggplot(aes(x = ano, y = var_exp, color = componente)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c(PC1 = '#D6604D', PC2 = '#4393C3', PC3 = '#74C476')
  ) +
  labs(title    = 'Variância explicada por componente (PCA por ano)',
       subtitle = 'Evolução da dimensionalidade da privação',
       x = NULL, y = '% variância explicada', color = NULL)


# Exportando os gráficos
dir_out <- 'output/02_ml/pca'
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

## Gráficos
fig_list <- list(
  fig01_scree_plot         = list(p = p_scree,        w = 16,   h = 12),
  fig02_biplot_pca         = list(p = p_var_bi,       w = 16,   h = 12),
  fig03a_contribuicao_pc1  = list(p = p_contrib_pc1,  w = 16,   h = 12),
  fig03b_contribuicao_pc2  = list(p = p_contrib_pc2,  w = 16,   h = 12),
  fig03c_contribuicao_pc3  = list(p = p_contrib_pc3,  w = 16,   h = 12),
  fig04a_densidade_pc1     = list(p = p_dens_pc1,     w = 16,   h = 12),
  fig04b_densidade_pc2     = list(p = p_dens_pc2,     w = 16,   h = 12),
  fig04c_densidade_pc3     = list(p = p_dens_pc3,     w = 16,   h = 12),
  fig05_evolucao_temporal  = list(p = p_var_temporal, w = 16,   h = 12)
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


## -- Modelo 2: Random Forest sobre covariáveis ----
# Objetivo: quais atributos predizem mais a pobreza MPI?

# Loop principal para Random Forest anual
anos_rf <- sort(unique(mpi_amostrado$ano))

rf_ano <- map(anos_rf, function(a) {
  
  dados <- mpi_amostrado |>
    filter(ano == a) |>
    dplyr::select(pobre_33, sexo, uf, urbano, raca, arranjo2) |>
    mutate(
      pobre_33 = factor(pobre_33, labels = c('Nao_pobre', 'Pobre')),
      across(c(sexo, urbano, uf, raca, arranjo2), factor)
    )
  
  idx_treino <- c(
    sample(which(dados$pobre_33 == 'Pobre'),     floor(0.8 * sum(dados$pobre_33 == 'Pobre'))),
    sample(which(dados$pobre_33 == 'Nao_pobre'), floor(0.8 * sum(dados$pobre_33 == 'Nao_pobre')))
  )
  train <- dados[ idx_treino, ]
  test  <- dados[-idx_treino, ]
  
  fit <- ranger(
    pobre_33 ~ .,
    data          = train,
    num.trees     = 500,
    mtry          = 3,
    min.node.size = 10,
    importance    = 'permutation',
    probability   = TRUE,
    num.threads   = parallel::detectCores() - 1,
    seed          = 2026
  )
  
  prob   <- predict(fit, data = test)$predictions[, 'Pobre']
  classe <- factor(ifelse(prob >= 0.5, 'Pobre', 'Nao_pobre'),
                   levels = c('Nao_pobre', 'Pobre'))
  roc_obj <- roc(test$pobre_33, prob,
                 levels = c('Nao_pobre', 'Pobre'),
                 direction = '<', quiet = TRUE)
  
  message(a,
          ' | OOB: ',  round(fit$prediction.error, 4),
          ' | Acc: ',  round(mean(classe == test$pobre_33), 4),
          ' | AUC: ',  round(as.numeric(auc(roc_obj)), 4))
  
  list(
    fit    = fit,
    train  = train,
    test   = test,
    auc    = as.numeric(auc(roc_obj)),
    roc_df = tibble(
      ano = a,
      fpr = 1 - rev(roc_obj$specificities),
      tpr = rev(roc_obj$sensitivities)
    )
  )
}) |> setNames(as.character(anos_rf))
gc()


# Importância de atributos
imp_tab <- map_dfr(names(rf_ano), function(a){
  imp <- importance(rf_ano[[a]]$fit)
  tibble(ano     = as.integer(a),
         variavel = names(imp),
         imp_norm = imp / sum(imp))   # normalizado para comparar anos
})

p_imp <- ggplot(imp_tab,
                aes(x = factor(ano), y = imp_norm,
                    color = variavel, group = variavel)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 3.5) +
  geom_text_repel(
    data         = filter(imp_tab, ano == max(ano)),
    aes(label    = variavel),
    direction    = 'y',
    nudge_x      = 0.4,
    hjust        = 0,
    segment.size = 0.3,
    segment.color = 'grey70',
    size         = 4.0,
    box.padding  = 0.15
  ) + 
  theme(legend.position = 'none') +
  scale_x_discrete(expand = expansion(add = c(0.3, 0.5))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_brewer(palette = 'Dark2', guide = 'none') +
  labs(title    = 'Evolução da importância de variáveis: RF por ano',
       subtitle = 'Importância por permutação, normalizada | VD: pobre_33',
       x = NULL, y = 'Importância relativa')


# Estudo das curvas ROC
roc_all <- map_dfr(rf_ano, 'roc_df')

auc_labels <- tibble(
  ano   = as.integer(names(rf_ano)),
  auc   = map_dbl(rf_ano, 'auc'),
  label = paste0(names(rf_ano), ': AUC = ', round(map_dbl(rf_ano, 'auc'), 3))
)

p_roc <- ggplot(roc_all, aes(x = fpr, y = tpr, color = factor(ano))) +
  geom_line(linewidth = 1) +
  geom_abline(slope = 1, intercept = 0,
              linetype = 'dashed', color = 'grey50') +
  geom_label(data = auc_labels |> mutate(fpr = 0.58, tpr = seq(0.28, 0.10, length.out = 4)),
             aes(label = label), size = 3.2, fill = 'white', linewidth = 0) +
  scale_color_brewer(palette = 'RdYlBu', name = 'Ano') +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title    = 'Curvas ROC por ano: Random Forest',
       subtitle = 'VD: pobre_33 | teste 20% | sem log_rpcr',
       x = '1 − Especificidade (FPR)', y = 'Sensibilidade (TPR)')


# PDP bivariado, por ano
pred_fun <- function(model, newdata){
  predict(model, data = newdata)$predictions[, 'Pobre']
}

lbl_sexo    <- c('1' = 'Homem',      '2' = 'Mulher')
lbl_arranjo <- c('NN' = 'Unipessoal', 'SN' = 'Casal Sem',
                 'SS' = 'Casal Com',  'NS' = 'Monoparental')

p_pdp_lista <- imap(rf_ano, function(obj, a) {
  
  sub <- obj$train |>
    slice_sample(n = min(10000, nrow(obj$train))) |>
    dplyr::select(-pobre_33)
  
  pred <- Predictor$new(model = obj$fit, data = sub,
                        predict.function = pred_fun)
  
  pdp <- FeatureEffect$new(pred, method = 'pdp',
                           feature = c('sexo', 'arranjo2'))
  
  plot(pdp) +
    scale_x_discrete(labels = lbl_sexo) +
    scale_y_discrete(labels = lbl_arranjo) +
    labs(title = a, x = NULL, y = NULL)
})
gc() 

p_wrap <- wrap_plots(p_pdp_lista, nrow = 2) +
  plot_annotation(
    title    = 'PDP bivariado: sexo × arranjo domiciliar, por ano',
    subtitle = 'P(Pobre) | Random Forest | VD: pobre_33'
  )


# Exportando os gráficos
dir_out <- 'output/02_ml/rf'
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

## Gráficos
fig_list <- list(
  fig01_importancia        = list(p = p_imp,          w = 16,   h = 12),
  fig02_curvas_roc         = list(p = p_roc,          w = 16,   h = 12),
  fig03_pdp_bivariado      = list(p = p_wrap,         w = 16,   h = 12)
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


## -- Modelo 3: C-Means para análise de regiões ----

vars_cl <- paste0('d', c(11:13, 21:23, 31:33, 41:43, 51:52), '_med')
anos_cl <- sort(unique(mpi_regioes$ano))

# Clusterização hierárquica para identificação de K
agnes_ano <- map(anos_cl, function(a) {
  
  df <- mpi_regioes |> filter(ano == a, !is.na(rim))
  
  mat_sc <- df |>
    dplyr::select(all_of(vars_cl)) |>
    mutate(across(everything(), as.numeric)) |>
    scale()
  
  ag       <- agnes(mat_sc, method = 'ward')
  h_sorted <- sort(ag$height, decreasing = TRUE)
  k_max    <- 8
  
  gaps <- tibble(
    k   = 3:k_max,
    gap = h_sorted[3:k_max - 1] - h_sorted[3:k_max]  # sempre positivo
  )
  k_sug <- gaps |> slice_max(gap) |> pull(k)
  cat('Ano:', a, '| k sugerido:', k_sug, '\n')
  
  list(ano = a, df = df, mat = mat_sc, ag = ag,
       h_sorted = h_sorted, k_sug = k_sug, gaps = gaps)
  
}) |> setNames(as.character(anos_cl))

# Dendrogramas
make_dendro_plot_simples <- function(a) {
  obj   <- agnes_ano[[a]]
  k     <- obj$k_sug
  h_cut <- (obj$h_sorted[k - 1] + obj$h_sorted[k]) / 2
  
  dend_data <- ggdendro::dendro_data(as.hclust(obj$ag))
  
  ggplot() +
    geom_segment(data = ggdendro::segment(dend_data),
                 aes(x = x, y = y, xend = xend, yend = yend),
                 linewidth = 0.35) +
    geom_hline(yintercept = h_cut, color = '#D6604D',
               linetype = 'dashed', linewidth = 0.5) +
    labs(title = paste('AGNES:', a),
         subtitle = paste('k sugerido:', k),
         x = NULL, y = 'Height') +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
}

p_dendro <- wrap_plots(map(names(agnes_ano), make_dendro_plot_simples), nrow = 2) +
  plot_annotation(
    title    = 'Dendrogramas AGNES por ano',
    subtitle = 'Linkage: Ward | corte sugerido (linha tracejada)'
  )

# C-Means para obtenção de clusterização
k_por_ano <- c('1980' = 4L, '1991' = 4L, '2000' = 4L, '2010' = 4L)

cl_ano <- imap(agnes_ano, function(obj, a) {
  
  k <- k_por_ano[[a]]
  
  # C-means sobre a mesma matriz padronizada do AGNES
  set.seed(42)
  fit <- cmeans(obj$mat, centers = k, m = 2, iter.max = 200)
  U   <- fit$membership
  
  df <- obj$df |>
    mutate(
      cluster   = factor(apply(U, 1, which.max)),
      fuzzy_max = apply(U, 1, max)
    )
  
  # Centroides no espaço original
  centroides <- df |>
    dplyr::select(rim, cluster, all_of(vars_cl)) |>
    mutate(across(all_of(vars_cl), as.numeric)) |>
    group_by(cluster) |>
    summarise(across(all_of(vars_cl), mean), .groups = 'drop') |>
    mutate(ano = as.integer(a))
  
  # MDS
  mds <- cmdscale(dist(obj$mat), k = 2, eig = TRUE)
  cat('Ano:', a, '| GOF:', round(mds$GOF[1], 4), '\n')
  
  mds_df <- tibble(
    rim       = df$rim,
    Dim1      = mds$points[, 1],
    Dim2      = mds$points[, 2],
    cluster   = df$cluster,
    fuzzy_max = df$fuzzy_max
  )
  
  list(fit = fit, df = df, centroides = centroides, mds = mds_df, k = k)
})

# Centroides consolidados
centroides_all <- map_dfr(cl_ano, 'centroides')
print(centroides_all)


# Mapas
## Workaround para bug em geobr::read_immediate_region() (retorna só Rondônia).
## Constrói a malha de Regiões Imediatas a partir da API do IBGE + dissolve
## de municípios via geobr::read_municipality().

build_immediate_from_api <- function(sf_mun, cache_dir = here::here('data', '00_maps')) {
  path_sf  <- file.path(cache_dir, 'imm_regions_dissolved.rds')
  path_dtb <- file.path(cache_dir, 'dtb_api.rds')
  
  if (file.exists(path_sf)) {
    message('[cache] imm_regions_dissolved.rds')
    return(readRDS(path_sf))
  }
  
  if (!file.exists(path_dtb)) {
    message('[IBGE API] Baixando municípios + regiões imediatas...')
    resp <- httr::GET(
      'https://servicodados.ibge.gov.br/api/v1/localidades/municipios',
      query = list(view = 'nivelado')
    )
    httr::stop_for_status(resp)
    dtb <- httr::content(resp, as = 'text', encoding = 'UTF-8') |>
      jsonlite::fromJSON() |>
      dplyr::transmute(
        code_muni      = stringr::str_pad(as.character(`municipio-id`), 7L, 'left', '0'),
        code_immediate = as.character(`regiao-imediata-id`),
        name_immediate = as.character(`regiao-imediata-nome`),
        abbrev_state   = as.character(`UF-sigla`)
      ) |>
      dplyr::distinct()
    message(sprintf('[IBGE API] %d municípios recebidos', nrow(dtb)))
    saveRDS(dtb, path_dtb)
  } else {
    dtb <- readRDS(path_dtb)
    if (!'abbrev_state' %in% names(dtb)) {
      message('[cache inválido] dtb_api.rds sem abbrev_state — regenerando...')
      file.remove(path_dtb)
      file.remove(path_sf)
      return(build_immediate_from_api(sf_mun, cache_dir))
    }
  }
  
  sf_dissolved <- sf_mun |>
    dplyr::mutate(
      code_muni = stringr::str_pad(as.character(code_muni), 7L, 'left', '0')
    ) |>
    sf::st_make_valid() |>
    dplyr::left_join(dtb, by = 'code_muni') |>
    dplyr::filter(!is.na(code_immediate)) |>
    dplyr::group_by(code_immediate, name_immediate) |>   # <- sem abbrev_state
    dplyr::summarise(.groups = 'drop') |>
    sf::st_as_sf() |>
    sf::st_make_valid() |>
    dplyr::left_join(
      dtb |> dplyr::distinct(code_immediate, abbrev_state),
      by = 'code_immediate'
    )
  
  n <- nrow(sf_dissolved)
  message(sprintf('[dissolve] %d regiões imediatas construídas', n))
  if (n < 500L) stop(sprintf('Esperado ~509 regiões, obteve %d', n))
  
  saveRDS(sf_dissolved, path_sf)
  sf_dissolved
}

## Crosswalk município → região imediata via centroide espacial.
## Sempre recebe sf_mun nacional para garantir cobertura completa.
get_mun_to_immediate <- function(sf_mun, sf_imm, cache_dir = here::here('data', '00_maps')) {
  path <- file.path(cache_dir, 'mun_to_immediate.rds')
  if (file.exists(path)) { message(sprintf('[cache] %s', path)); return(readRDS(path)) }
  
  stopifnot('sf_mun deve ser nacional (>5000 municípios)' = nrow(sf_mun) > 5000L)
  stopifnot('sf_imm deve ter >500 regiões'                = nrow(sf_imm) > 500L)
  
  result <- sf_mun |>
    sf::st_centroid(of_largest_polygon = TRUE) |>
    sf::st_join(sf_imm |> dplyr::select(code_immediate, name_immediate)) |>
    sf::st_drop_geometry() |>
    dplyr::select(code_muni, code_immediate, name_immediate) |>
    dplyr::mutate(
      code_muni      = as.character(code_muni),
      code_immediate = as.character(code_immediate)
    )
  
  n_matched <- sum(!is.na(result$code_immediate))
  message(sprintf('[crosswalk] %d/%d municípios com região imediata', n_matched, nrow(result)))
  
  saveRDS(result, path)
  result
}

sf_mun_nacional <- geobr::read_municipality(year = 2020, showProgress = FALSE)

shp_rim <- build_immediate_from_api(sf_mun_nacional) |>
  dplyr::mutate(rim = as.integer(code_immediate))

cores_cl <- c('1' = '#E41A1C', '2' = '#377EB8',
              '3' = '#4DAF4A', '4' = '#984EA3')

p_mapas <- imap(cl_ano, function(obj, a) {
  shp_rim |>
    left_join(obj$df |> dplyr::select(rim, cluster), by = 'rim') |>
    ggplot() +
    geom_sf(aes(fill = cluster), color = NA) +
    scale_fill_manual(
      values   = cores_cl,
      name     = 'Cluster',
      limits   = c('1','2','3','4'),
      drop     = FALSE,
      na.value = 'grey80'
    ) +
    labs(title = as.character(a)) +
    theme_void(base_size = 9) +
    theme(plot.title = element_text(face = 'bold', hjust = 0.5))
})

p_mapas_clust <- wrap_plots(p_mapas, nrow = 2) +
  plot_layout(guides = 'collect') +
  plot_annotation(
    title    = 'Tipologia de privação por RIM',
    subtitle = 'Fuzzy C-Means | d11:d52 padronizados'
  ) &
  theme(legend.position = 'right')


# Escalonamento multidimensional
mds_all <- imap_dfr(cl_ano, function(obj, a)
  obj$mds |> mutate(ano = as.integer(a)))

p_mds_lista <- imap(cl_ano, function(obj, a) {
  obj$mds |>
    ggplot(aes(x = Dim1, y = Dim2, color = cluster)) +
    geom_point(aes(alpha = fuzzy_max), size = 1.5) +
    scale_color_manual(values = cores_cl, name = 'Cluster',
                       limits = c('1','2','3','4'), drop = FALSE) +
    scale_alpha_continuous(
      range  = c(0.3, 1),
      name   = 'Pertencimento',
      limits = c(0.3, 1),
      breaks = c(0.4, 0.6, 0.8)
    ) +
    labs(title = as.character(a), x = 'Dimensão 1', y = 'Dimensão 2') +
    theme(aspect.ratio = 1)
})

p_mds <- wrap_plots(p_mds_lista, nrow = 2) +
  plot_layout(guides = 'collect') +
  plot_annotation(
    title    = 'Escalonamento multidimensional por ano',
    subtitle = 'MDS clássico | cores = clusters c-means'
  ) &
  theme(legend.position = 'right')


# Exportando os gráficos
dir_out <- 'output/02_ml/cmeans'
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

## Gráficos
fig_list <- list(
  fig01_dendrogramas      = list(p = p_dendro,       w = 16,   h = 12),
  fig02_clusters_rim      = list(p = p_mapas_clust,  w = 16,   h = 12),
  fig03_escalonamento     = list(p = p_mds,          w = 16,   h = 12)
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
