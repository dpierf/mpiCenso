#-------------------------------------------------------------------------------#
#                                                                               #
#                  REVISITANDO A TESE DE DOUTORADO - PROJETO T                  #
#                                                                               #
#-------------------------------------------------------------------------------#

# =============== FASE 6 - Mogelagem (2): Análise Econométrica ================ #

source('R/00_packages.R')
mpi_original     <- read_parquet('data/04_output/mpi.parquet')
mpi_simplificado <- read_parquet('data/04_output/mpi_simplificado.parquet')
mpi_regioes      <- read_parquet('data/04_output/mpi_rims.parquet')
mpi_amostrado    <- read_parquet('data/04_output/mpi_sample.parquet')

mpi_amostrado <- mpi_amostrado |>
  mutate(regiao = factor(substr(uf,1,1)))

set.seed(2026)

# Função auxiliar: limpar nomes dos termos para gráficos
limpar_termos <- function(df) {
  df |>
    mutate(term = term |>
             # Com factor() na fórmula (compatibilidade)
             str_replace('factor\\(ano\\)',      'Ano: ') |>
             str_replace('factor\\(regiao\\)',   'Região: ') |>
             str_replace('factor\\(urbano\\)',   'Urbano: ') |>
             str_replace('factor\\(sexo\\)',     'Sexo: ') |>
             str_replace('factor\\(raca\\)',     'Raça: ') |>
             str_replace('factor\\(arranjo2\\)', 'Arranjo: ') |>
             # Sem factor() — variáveis já fatoradas no data
             str_replace('^ano(\\d+)',     'Ano: \\1') |>
             str_replace('^regiao(\\d+)', 'Região: \\1') |>
             str_replace('^urbano(\\d+)', 'Urbano: \\1') |>
             str_replace('^sexo(\\d+)',   'Sexo: \\1') |>
             str_replace('^raca(\\d+)',   'Raça: \\1') |>
             str_replace('^arranjo2',     'Arranjo: ') |>
             # Labels finais
             str_replace('Região: 2', 'Região: Nordeste') |>
             str_replace('Região: 3', 'Região: Sudeste') |>
             str_replace('Região: 4', 'Região: Sul') |>
             str_replace('Região: 5', 'Região: Centro-Oeste') |>
             str_replace('Urbano: 2', 'Rural') |>
             str_replace('Sexo: 2',   'Sexo: Mulher') |>
             str_replace('Raça: 2',   'Raça: Preta') |>
             str_replace('Raça: 3',   'Raça: Amarela') |>
             str_replace('Raça: 4',   'Raça: Parda') |>
             str_replace('Raça: 5',   'Raça: Indígena') |>
             str_replace('Arranjo: NS', 'Arranjo: Monoparental') |>
             str_replace('Arranjo: SN', 'Arranjo: Casal Sem') |>
             str_replace('Arranjo: SS', 'Arranjo: Casal Com') |>
             str_replace('log_rpcr',    'ln(Renda p.c.)')
    )
}


## -- Modelo 1: Regressão Logística ----

# Execução do modelo
mod_logit <- fixest::feglm(
  pobre_33 ~ ano + regiao + urbano + sexo + raca + arranjo2 + log_rpcr,
  data    = mpi_amostrado,
  family  = binomial(link = 'logit'),
  weights = ~peso,
  vcov    = ~rim
)

summary(mod_logit)
etable(mod_logit, digits = 4)

# Coefplot
coef_logit <- marginaleffects::tidy(mod_logit, conf.int = TRUE) |>
  filter(!term %in% c('(Intercept)','Constant')) |>
  limpar_termos()

p_coef_logit <- ggplot(coef_logit,
                       aes(x = reorder(term, estimate),
                           y = estimate,
                           ymin = conf.low,
                           ymax = conf.high,
                           color = estimate > 0)) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey50') +
  geom_pointrange(linewidth = 0.6, size = 0.4) +
  coord_flip() +
  scale_color_manual(values = c('TRUE'  = '#D6604D',
                                'FALSE' = '#4393C3'),
                     guide  = 'none') +
  labs(
    title    = 'Regressão logística para cut-off multidimensional de 33%',
    subtitle = 'Coeficientes com IC 95% | SE clusterizado por município',
    x = NULL, y = 'Coeficiente (log-odds)'
  )

# Efeitos marginais médios (AME)
extrair_termo_completo <- function(term, contrast) {
  if (contrast == 'dY/dX') return(term)  # variável contínua, sem nível a anexar
  nivel <- stringr::str_extract(contrast, '^[^ ]+')  # parte antes do " - "
  paste0(term, nivel)
}

ame_logit <- avg_slopes(mod_logit,
                        newdata = mpi_amostrado |>
                          dplyr::group_by(ano, regiao, urbano, sexo, raca, arranjo2) |>
                          slice_sample(n = 100) |>
                          ungroup()) |>
  as_tibble() |>
  dplyr::filter(!term %in% c('Constant','(Intercept)')) |>
  dplyr::mutate(term = purrr::map2_chr(term, contrast, extrair_termo_completo)) |>
  limpar_termos()

p_ame <- ggplot(ame_logit,
                aes(x = reorder(term, estimate),
                    y = estimate,
                    ymin = conf.low,
                    ymax = conf.high,
                    color = estimate > 0)) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey50') +
  geom_pointrange(linewidth = 0.6, size = 0.4) +
  coord_flip() +
  scale_color_manual(values = c('TRUE'  = '#D6604D',
                                'FALSE' = '#4393C3'),
                     guide  = 'none') +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    title    = 'Efeitos marginais médios (AME), modelo logístico',
    subtitle = 'Variação em P(score > 33%) | SE clusterizado por município',
    x = NULL, y = 'Efeito marginal médio'
  )


## -- Modelo 2: Regressão Fracionária ----

# Criação de atributo logit e modelagem
mpi_frac <- mpi_amostrado |>
  filter(score > 0, score < 1) |>
  mutate(logit_score = log(score / (1 - score)))

mod_frac <- fixest::feols(
  logit_score ~ ano + regiao + urbano + sexo + raca + arranjo2 + log_rpcr,
  data    = mpi_frac,
  weights = ~peso,
  vcov    = ~rim
)

summary(mod_frac)
etable(mod_frac, digits = 4)

# Visualização gráfica 
coef_frac <- marginaleffects::tidy(mod_frac, conf.int = TRUE) |>
  filter(!term %in% c('Constant','(Intercept)')) |>
  limpar_termos()

p_coef_frac <- ggplot(coef_frac,
                      aes(x = reorder(term, estimate),
                          y = estimate,
                          ymin = conf.low,
                          ymax = conf.high,
                          color = estimate > 0)) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey50') +
  geom_pointrange(linewidth = 0.6, size = 0.4) +
  coord_flip() +
  scale_color_manual(values = c('TRUE'  = '#D6604D',
                                'FALSE' = '#4393C3'),
                     guide  = 'none') +
  labs(
    title    = 'Regressão fracionária para logit(score)',
    subtitle = 'Coeficientes com IC 95% | SE clusterizado por município',
    x = NULL, y = 'Coeficiente'
  )

# Coefplot comparativo (logit x fracreg)
p_comp <- bind_rows(
  tidy(mod_logit, conf.int = TRUE) |> mutate(modelo = 'Logístico (pobre_33)'),
  tidy(mod_frac,  conf.int = TRUE) |> mutate(modelo = 'Fracionária (score)')
) |>
  filter(!term %in% c('(Intercept)', 'Constant')) |>
  limpar_termos() |>
  ggplot(aes(x = reorder(term, estimate), y = estimate,
             ymin = conf.low, ymax = conf.high,
             color = modelo, shape = modelo)) +
  geom_pointrange(position = position_dodge(0.5), linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey50') +
  coord_flip() +
  scale_color_manual(values = c('Logístico (pobre_33)' = '#1F78B4',
                                'Fracionária (score)'  = '#E41A1C')) +
  labs(
    title    = 'Comparação de coeficientes: modelos logístico vs fracionário',
    subtitle = 'IC 95% | SE clusterizado por RIM | ref.: 1980, Norte, Urbano, Homem, Branca, Unipessoal',
    x = NULL, y = 'Coeficiente', color = NULL, shape = NULL
  ) +
  theme(legend.position = 'bottom')


## -- Modelo 3: Regressão Quantílica ----

# Seleção de quantis e modelagem
tau_seq <- c(0.10, 0.25, 0.33, 0.50, 0.67, 0.75, 0.90)

mod_qr <- rq(
  score ~ ano + regiao + urbano + sexo + raca + arranjo2 + log_rpcr,
  tau     = tau_seq,
  data    = mpi_amostrado,
  weights = mpi_amostrado$peso,
  method  = 'fn'
)

summary(mod_qr, se = 'ker')

# Gráfico de coeficientes por quantil
coef_qr <- coef(mod_qr) |>
  t() |>
  as.data.frame() |>
  rownames_to_column('tau_str') |>
  mutate(tau = as.numeric(gsub('tau= ', '', tau_str))) |>
  dplyr::select(-tau_str) |>
  pivot_longer(-tau, names_to = 'variavel', values_to = 'coef') |>
  filter(!str_detect(variavel, 'factor|Intercept')) |>
  rename(term = variavel) |>
  limpar_termos() |>
  rename(variavel = term) |>
  mutate(
    atributo = case_when(
      str_starts(variavel, 'Ano')     ~ 'Ano',
      str_starts(variavel, 'Raça')    ~ 'Raça/Cor',
      str_starts(variavel, 'Região')  ~ 'Região',
      str_starts(variavel, 'Arranjo') ~ 'Arranjo',
      variavel == 'Rural'             ~ 'Urbano/Rural',
      str_starts(variavel, 'Sexo')    ~ 'Sexo e Renda',
      variavel == 'ln(Renda p.c.)'    ~ 'Sexo e Renda'
    )
  )

coef_qr_last <- coef_qr |>
  group_by(variavel) |>
  filter(tau == max(tau)) |>
  ungroup()

p_qr <- ggplot(coef_qr,
               aes(x = tau, y = coef, color = variavel, group = variavel)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey50') +
  geom_text_repel(
    data        = coef_qr_last,
    aes(label   = variavel),
    hjust       = 0,
    direction   = 'y',          # só afasta verticalmente
    nudge_x     = 0.02,
    segment.size = 0.3,
    segment.color = 'grey60',
    size        = 3,
    show.legend = FALSE
  ) +
  facet_wrap(~atributo, scales = 'free_y', ncol = 3) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0.05, 0.25))  # espaço à direita para os labels
  ) +
  labs(
    title    = 'Coeficientes da regressão quantílica',
    subtitle = 'Efeito de cada covariável por quantil da distribuição de privação',
    x = 'Quantil (τ)', y = 'Coeficiente'
  ) +
  theme(legend.position = 'none')


## -- Modelo 4A: Convergência Beta ----

# Preparação de dados
conv_data <- mpi_regioes |>
  filter(ano %in% c(1980, 2010)) |>
  pivot_wider(id_cols = rim,
              names_from  = ano,
              values_from = MPI,
              names_prefix = 'mpi_') |>
  filter(!is.na(mpi_1980), !is.na(mpi_2010), mpi_1980 > 0) |>
  left_join(
    mpi_regioes |> filter(ano == 1980) |> dplyr::select(rim, n_exp),
    by = 'rim'
  ) |>
  mutate(
    ln_mpi0  = log(mpi_1980),
    delta_ln = log(mpi_2010 / mpi_1980),
    mpi0_pp  = mpi_1980 * 100,
    delta_pp = (mpi_2010 - mpi_1980) * 100
  )

conv_club <- conv_data |>
  left_join(
    mpi_regioes |> filter(ano == 1980) |> mutate(regiao = substr(rim,1,1)) |> dplyr::select(rim, regiao),
    by = 'rim'
  )


# Estimação de modelos de convergência

## Modelos regionais (cada unidade tem o mesmo peso)
mod_loglog_reg <- lm(delta_ln ~ ln_mpi0, data = conv_data)
mod_pp_reg     <- lm(delta_pp ~ mpi0_pp, data = conv_data)

## Modelos domiciliares (cada unidade pesa diferente)
mod_loglog_dom <- lm(delta_ln ~ ln_mpi0, data = conv_data, weights = n_exp)
mod_pp_dom     <- lm(delta_pp ~ mpi0_pp, data = conv_data, weights = n_exp)

# Outputs (globais e por regiões)
modelsummary(
  list('Log-log (regional)' = mod_loglog_reg, 'Pts. (regional)' = mod_pp_reg,
       'Log-log (domiciliar)' = mod_loglog_dom, 'Pts. (domiciliar)' = mod_pp_dom),
  statistic = 'std.error',
  stars     = TRUE,
  gof_map   = c('nobs', 'r.squared', 'adj.r.squared')
)

beta_clube <- function(df, formula, term, weighted = FALSE) {
  df |>
    group_by(regiao) |>
    group_modify(~ {
      wts  <- if (weighted) .x$n_exp else NULL
      args <- list(formula = formula, data = .x, weights = wts)
      tidy(do.call(lm, args))
    }) |>
    filter(.data$term == {{ term }}) |>
    dplyr::select(regiao, estimate, std.error, p.value)
}

bind_rows(
  beta_clube(conv_club, delta_ln ~ ln_mpi0, 'ln_mpi0', weighted = FALSE) |> mutate(modelo = 'Log-log regional'),
  beta_clube(conv_club, delta_ln ~ ln_mpi0, 'ln_mpi0', weighted = TRUE)  |> mutate(modelo = 'Log-log domiciliar'),
  beta_clube(conv_club, delta_pp ~ mpi0_pp, 'mpi0_pp', weighted = FALSE) |> mutate(modelo = 'Pts. regional'),
  beta_clube(conv_club, delta_pp ~ mpi0_pp, 'mpi0_pp', weighted = TRUE)  |> mutate(modelo = 'Pts. domiciliar')
) |>
  arrange(regiao, modelo)


# Visualização via scatterplot (modelos globais)
scatter_conv <- function(df, x, y, xlab, ylab, titulo, beta, r2, weighted = FALSE) {
  w_aes <- if (weighted) aes(weight = n_exp) else NULL
  ggplot(df, aes(x = {{ x }}, y = {{ y }})) +
    geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey50') +
    geom_point(color = 'grey60', alpha = 0.5, size = 1.2) +
    geom_smooth(method = 'lm',    color = '#E8601C', fill = '#E8601C',
                alpha = 0.15, linewidth = 0.8, mapping = w_aes) +
    geom_smooth(method = 'loess', span = 0.75,
                color = '#2166AC', fill = '#2166AC',
                alpha = 0.15, linewidth = 0.8, mapping = w_aes) +
    annotate('text', x = Inf, y = Inf,
             label  = sprintf('β = %.3f  |  R² = %.3f', beta, r2),
             hjust  = 1.1, vjust = 1.5, size = 3, color = 'grey30') +
    labs(title = titulo, x = xlab, y = ylab)
}

p_ll_reg <- scatter_conv(conv_data, ln_mpi0, delta_ln,
                         'ln(MPI 1980)', 'Δ ln(MPI 2010/1980)', 'Regional (Log-log)',
                         coef(mod_loglog_reg)['ln_mpi0'], summary(mod_loglog_reg)$r.squared)

p_ll_dom <- scatter_conv(conv_data, ln_mpi0, delta_ln,
                         'ln(MPI 1980)', 'Δ ln(MPI 2010/1980)', 'Domiciliar (Log-log)',
                         coef(mod_loglog_dom)['ln_mpi0'], summary(mod_loglog_dom)$r.squared,
                         weighted = TRUE)

p_pp_reg <- scatter_conv(conv_data, mpi0_pp, delta_pp,
                         'MPI 1980 (%)', 'Δ MPI 2010/1980 (p.p.)', 'Regional (Pts. percentuais)',
                         coef(mod_pp_reg)['mpi0_pp'], summary(mod_pp_reg)$r.squared)

p_pp_dom <- scatter_conv(conv_data, mpi0_pp, delta_pp,
                         'MPI 1980 (%)', 'Δ MPI 2010/1980 (p.p.)', 'Domiciliar (Pts. percentuais)',
                         coef(mod_pp_dom)['mpi0_pp'], summary(mod_pp_dom)$r.squared,
                         weighted = TRUE)

p_global <- (p_ll_reg | p_ll_dom) / (p_pp_reg | p_pp_dom) +
  plot_annotation(
    title    = 'Convergência beta: MPI por Região imediata (1980-2010)',
    subtitle = 'Azul: LOESS  |  Vermelho: linear',
    theme    = theme(plot.title = element_text(face = 'bold'))
  )

print(p_global)


# Visualização de scatterplot (modelos por regiões)
scatter_clube <- function(df, x, y, xlab, ylab, titulo, weighted = FALSE) {
  w_aes <- if (weighted) aes(weight = n_exp) else NULL
  ggplot(df, aes(x = {{ x }}, y = {{ y }})) +
    geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey50') +
    geom_point(color = 'grey60', alpha = 0.45, size = 0.9) +
    geom_smooth(method = 'lm',    color = '#E8601C', fill = '#E8601C',
                alpha = 0.15, linewidth = 0.8, mapping = w_aes) +
    geom_smooth(method = 'loess', span = 0.75,
                color = '#2166AC', fill = '#2166AC',
                alpha = 0.15, linewidth = 0.8, mapping = w_aes) +
    facet_wrap(~regiao, nrow = 2, scales = 'free_x') +
    labs(title    = titulo,
         subtitle = 'Azul: LOESS  |  Vermelho: linear',
         x = xlab, y = ylab)
}

p_clube_ll_reg <- scatter_clube(conv_club, ln_mpi0, delta_ln,
                                'ln(MPI 1980)', 'Δ ln(MPI 2010/1980)', 'Regional (Log-log, 1980-2010)')

p_clube_ll_dom <- scatter_clube(conv_club, ln_mpi0, delta_ln,
                                'ln(MPI 1980)', 'Δ ln(MPI 2010/1980)', 'Domiciliar (Log-log, 1980-2010)',
                                weighted = TRUE)

p_clube_pp_reg <- scatter_clube(conv_club, mpi0_pp, delta_pp,
                                'MPI 1980 (%)', 'Δ MPI 2010/1980 (p.p.)', 'Regional (Pts. percentuais, 1980-2010)')

p_clube_pp_dom <- scatter_clube(conv_club, mpi0_pp, delta_pp,
                                'MPI 1980 (%)', 'Δ MPI 2010/1980 (p.p.)', 'Domiciliar (Pts. percentuais, 1980-2010)',
                                weighted = TRUE)

print(p_clube_ll_reg)
print(p_clube_ll_dom)
print(p_clube_pp_reg)
print(p_clube_pp_dom)


## -- Modelo 4B: Convergência Sigma ----
sigma_conv <- mpi_regioes |>
  group_by(ano) |>
  summarise(
    media    = mean(MPI,      na.rm = TRUE),
    dp       = sd(MPI,        na.rm = TRUE),
    cv       = dp / media,
    dp_log   = sd(log(MPI),   na.rm = TRUE),
    .groups  = 'drop'
  )

print(sigma_conv)

base_1980 <- sigma_conv |>
  filter(ano == 1980) |>
  dplyr::select(media, dp, cv, dp_log)

p_sigma <- sigma_conv |>
  mutate(
    ano        = as.numeric(as.character(ano)), 
    media_idx  = media  / base_1980$media,
    dp_idx     = dp     / base_1980$dp,
    cv_idx     = cv     / base_1980$cv
  ) |>
  pivot_longer(ends_with('_idx'),
               names_to  = 'metrica',
               values_to = 'valor') |>
  mutate(metrica = factor(metrica,
                          levels = c('media_idx', 'dp_idx', 'cv_idx'),
                          labels = c('Média', 'DP', 'CV'))) |>
  ggplot(aes(x = ano, y = valor, color = metrica, linetype = metrica)) +
  geom_hline(yintercept = 1, linetype = 'dashed', color = 'grey50') +
  geom_line(aes(group = metrica), linewidth = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c(
    'Média'  = '#E41A1C',
    'DP'     = '#377EB8',
    'CV'     = '#4DAF4A'
  )) +
  scale_linetype_manual(values = c(
    'Média'  = 'dashed',
    'DP'     = 'dashed',
    'CV'     = 'dashed'
  )) +
  scale_y_continuous(
    labels = scales::label_number(suffix = 'x', accuracy = 0.1)
  ) +
  labs(
    title    = 'Convergência sigma: MPI por região imediata (1980-2010)',
    subtitle = 'Índice 1980 = 1  |  valores > 1: divergência  |  valores < 1: convergência',
    x = NULL, y = 'Índice (1980 = 1)',
    color = NULL, linetype = NULL
  ) +
  theme(legend.position = 'bottom')

print(p_sigma)


## -- Modelo 4C: Convergência Condicional ----
cond_data <- mpi_regioes |>
  filter(ano %in% c(1980, 2010)) |>
  dplyr::select(rim, ano, MPI, pct_urb, pct_negra, pct_homem, pct_casal_com, n_exp) |>
  pivot_wider(names_from  = ano,
              values_from = c(MPI, pct_urb, pct_negra, pct_homem, pct_casal_com, n_exp)) |>
  filter(!is.na(MPI_1980), !is.na(MPI_2010)) |>
  mutate(
    ln_mpi0  = log(MPI_1980),
    delta_ln = log(MPI_2010) - log(MPI_1980),
    delta_pp = MPI_2010 - MPI_1980
  )

# Log-log condicional
mod_cond_loglog <- lm(
  delta_ln ~ ln_mpi0 + pct_urb_1980 + pct_negra_1980 + pct_homem_1980 + pct_casal_com_1980,
  data = cond_data, weights = n_exp_1980
)

# P.p. condicional
mod_cond_pp <- lm(
  delta_pp ~ MPI_1980 + pct_urb_1980 + pct_negra_1980 + pct_homem_1980 + pct_casal_com_1980,
  data = cond_data, weights = n_exp_1980
)

summary(mod_cond_loglog)
summary(mod_cond_pp)


## -- Modelo 5: Estatística e Econometria Espacial ----

# Shapefile de regiões imediatas
shp_rim <- readRDS(here::here('data', '00_maps', 'imm_regions_dissolved.rds')) |>
  mutate(code_immediate = as.character(code_immediate)) |>
  mutate(rim = as.integer(code_immediate))

mpi_rim_2010 <- mpi_regioes |>
  filter(ano == 2010) |>
  mutate(code_immediate = as.character(rim))

rim_sf <- shp_rim |>
  left_join(mpi_rim_2010, by = 'code_immediate') |>
  filter(!is.na(MPI))

# Matriz de pesos espaciais
nb_rim <- poly2nb(rim_sf, queen = TRUE)
W_rim  <- nb2listw(nb_rim, style = 'W', zero.policy = TRUE)

# Teste de Moran I global (autocorrelação espacial do MPI)
moran_res <- moran.test(rim_sf$MPI, W_rim, zero.policy = TRUE)
print(moran_res)

message('Moran I: ', round(moran_res$estimate['Moran I statistic'], 4),
        ' | p-valor: ', round(moran_res$p.value, 4))

# Modelos ingênuos para teste de modelos espaciais
vars_dep <- c('MPI', 'H', 'A')
mods_ols <- list()
mods_sem <- list()
mods_sar <- list()

for (vd in vars_dep) {
  cat('\n====', vd, '====\n')
  
  formula <- as.formula(paste(vd, '~ pct_urb + pct_negra + pct_homem + pct_casal_com'))
  
  mod_ols        <- lm(formula, data = rim_sf, weights = n_exp)
  mods_ols[[vd]] <- mod_ols
  
  cat('\nVIF:\n')
  print(car::vif(mod_ols))
  
  cat('\nMoran I (resíduos):\n')
  print(moran.test(residuals(mod_ols), W_rim, zero.policy = TRUE))
  
  cat('\nTestes LM:\n')
  map_dfr(lm.RStests(
    mod_ols, W_rim, zero.policy = TRUE,
    test = c('RSlag', 'adjRSlag', 'RSerr', 'adjRSerr')
  ), broom::tidy, .id = 'teste')[, -c(4:5)] |>
    t() |>
    {\(m) {
      dt <- as.data.table(m[-1, ])
      setnames(dt, m[1, ])
      dt[, Métrica := c('Estatística', 'p-valor')]
      setcolorder(dt, 'Métrica')[]
    }}() |>
    print()
  
  cat('\nSEM (ponderado):\n')
  mod_sem        <- errorsarlm(formula, data = rim_sf, listw = W_rim,
                               weights = rim_sf$n_exp, zero.policy = TRUE)
  mods_sem[[vd]] <- mod_sem
  print(summary(mod_sem))
  
  cat('\nSAR (robustez, sem pesos):\n')
  mod_sar        <- lagsarlm(formula, data = rim_sf, listw = W_rim,
                             zero.policy = TRUE)
  mods_sar[[vd]] <- mod_sar
  print(summary(mod_sar))
}


# LISA
bb      <- st_bbox(shp_rim)
br_xlim <- c(bb['xmin'], bb['xmax'])
br_ylim <- c(bb['ymin'], bb['ymax'])

anos_lisa  <- c(1980, 1991, 2000, 2010)
plots_lisa <- list()

for (ano_i in anos_lisa) {
  
  sf_ano <- shp_rim |>
    left_join(
      mpi_regioes |>
        filter(ano == ano_i) |>
        mutate(code_immediate = as.character(rim)),
      by = 'code_immediate'
    ) |>
    filter(!is.na(MPI))
  
  nb_ano <- poly2nb(sf_ano, queen = TRUE)
  W_ano  <- nb2listw(nb_ano, style = 'W', zero.policy = TRUE)
  lisa_res <- localmoran(sf_ano$MPI, W_ano, zero.policy = TRUE)
  
  sf_ano <- sf_ano |>
    mutate(
      lisa_p    = lisa_res[, 'Pr(z != E(Ii))'],
      MPI_std   = as.numeric(scale(MPI)),
      lag_MPI   = lag.listw(W_ano, MPI_std, zero.policy = TRUE),
      quadrante = case_when(
        MPI_std > 0 & lag_MPI > 0 & lisa_p < 0.05 ~ 'Alto-Alto',
        MPI_std < 0 & lag_MPI < 0 & lisa_p < 0.05 ~ 'Baixo-Baixo',
        MPI_std > 0 & lag_MPI < 0 & lisa_p < 0.05 ~ 'Alto-Baixo',
        MPI_std < 0 & lag_MPI > 0 & lisa_p < 0.05 ~ 'Baixo-Alto',
        TRUE                                        ~ 'Não significativo'
      ) |> factor(levels = c('Alto-Alto', 'Baixo-Baixo',
                             'Alto-Baixo', 'Baixo-Alto',
                             'Não significativo'))
    )
  
  sf_plot <- shp_rim |>
    left_join(
      sf_ano |>
        st_drop_geometry() |>
        dplyr::select(code_immediate, quadrante),
      by = 'code_immediate'
    ) |>
    mutate(quadrante = factor(
      replace_na(as.character(quadrante), 'Não significativo'),
      levels = c('Alto-Alto', 'Baixo-Baixo', 'Alto-Baixo',
                 'Baixo-Alto', 'Não significativo')
    ))
  
  plots_lisa[[as.character(ano_i)]] <- ggplot(sf_plot) +
    geom_sf(aes(fill = quadrante), color = NA) +
    scale_fill_manual(values = c(
      'Alto-Alto'         = '#D7191C',
      'Baixo-Baixo'       = '#2C7BB6',
      'Alto-Baixo'        = '#FDAE61',
      'Baixo-Alto'        = '#ABD9E9',
      'Não significativo' = 'grey85'
    )) +
    coord_sf(xlim = br_xlim, ylim = br_ylim, expand = FALSE) +
    labs(title = as.character(ano_i), fill = NULL) +
    theme_void(base_size = 10) +
    theme(plot.title = element_text(face = 'bold', hjust = 0.5))
}

p_lisa <- ((
  (plots_lisa[['1980']] | plots_lisa[['1991']]) /
    (plots_lisa[['2000']] | plots_lisa[['2010']])  +
    plot_layout(guides = 'collect') +
    plot_annotation(
      title    = 'Clusters espaciais de pobreza (LISA): 1980-2010',
      subtitle = 'MPI por RIM | Moran Local | p < 0.05'
    )
) & theme(legend.position = 'right'))


# Getis-Ord (hotspots)
lim_z    <- 8
anos_hs  <- c(1980, 1991, 2000, 2010)
plots_hs <- list()

for (ano_i in anos_hs) {
  
  sf_ano <- shp_rim |>
    left_join(
      mpi_regioes |>
        filter(ano == ano_i) |>
        mutate(code_immediate = as.character(rim)),
      by = 'code_immediate'
    ) |>
    filter(!is.na(MPI))
  
  nb_ano  <- poly2nb(sf_ano, queen = TRUE)
  W_gi    <- nb2listw(include.self(nb_ano), style = 'B', zero.policy = TRUE)
  gi_z    <- as.numeric(localG(sf_ano$MPI, W_gi, zero.policy = TRUE))
  
  sf_ano <- sf_ano |> mutate(gi_z = gi_z)
  
  sf_plot <- shp_rim |>
    left_join(
      sf_ano |>
        st_drop_geometry() |>
        dplyr::select(code_immediate, gi_z),
      by = 'code_immediate'
    )
  
  plots_hs[[as.character(ano_i)]] <- ggplot(sf_plot) +
    geom_sf(aes(fill = gi_z), color = NA) +
    scale_fill_gradient2(
      low      = '#2C7BB6',
      mid      = 'white',
      high     = '#D7191C',
      midpoint = 0,
      limits   = c(-lim_z, lim_z),
      oob      = scales::squish,
      name     = 'Gi* (z)',
      na.value = 'grey85'
    ) +
    coord_sf(xlim = br_xlim, ylim = br_ylim, expand = FALSE) +
    labs(title = as.character(ano_i)) +
    theme_void(base_size = 10) +
    theme(plot.title = element_text(face = 'bold', hjust = 0.5))
}

p_getis <- ((
  (plots_hs[['1980']] | plots_hs[['1991']]) /
    (plots_hs[['2000']] | plots_hs[['2010']]) +
    plot_layout(guides = 'collect') +
    plot_annotation(
      title    = 'Análise de hotspots (Gi*): MPI por RIM (1980-2010)',
      subtitle = 'Getis-Ord Gi* - Gradiente (z-scale)'
    )
) & theme(legend.position = 'right'))


# EXPORTAÇÃO -------------------------------------------------------------------

dir_out1 <- 'output/03_regressions/rds'
dir.create(dir_out1, recursive = TRUE, showWarnings = FALSE)

dir_out2 <- 'output/03_regressions/graphs'
dir.create(dir_out2, recursive = TRUE, showWarnings = FALSE)

# Objetos de modelo
mod_beta <- list(
  loglog_reg = mod_loglog_reg,
  pp_reg     = mod_pp_reg,
  loglog_dom = mod_loglog_dom,
  pp_dom     = mod_pp_dom
)

mod_cond <- list(
  loglog = mod_cond_loglog,
  pp     = mod_cond_pp
)

mod_spatial <- list(
  ols = mods_ols,
  sem = mods_sem,
  sar = mods_sar
)

write_rds(mod_logit,    file.path(dir_out1, 'mod_logit.rds'))
write_rds(ame_logit,    file.path(dir_out1, 'ame_logit.rds'))
write_rds(mod_frac,     file.path(dir_out1, 'mod_frac.rds'))
write_rds(mod_qr,       file.path(dir_out1, 'mod_qr.rds'))
write_rds(mod_beta,     file.path(dir_out1, 'mod_beta.rds'))
write_rds(mod_cond,     file.path(dir_out1, 'mod_cond.rds'))
write_rds(mod_spatial,  file.path(dir_out1, 'mod_spatial.rds'))

# Gráficos
fig_list <- list(
  fig01a_coeficientes_logit        = list(p = p_coef_logit,       w = 16,   h = 12),
  fig01b_coeficientes_fracreg      = list(p = p_coef_frac,        w = 16,   h = 12),
  fig01c_coeficientes_quantilica   = list(p = p_qr,               w = 16,   h = 12),
  fig02_efeitos_marginais_logit    = list(p = p_ame,              w = 16,   h = 12),
  fig03_comparativo_logitXfracreg  = list(p = p_comp,             w = 16,   h = 12),
  fig04_convergencia_beta_global   = list(p = p_global,           w = 16,   h = 12),
  fig05_convergencia_sigma_global  = list(p = p_sigma,            w = 16,   h = 12),
  fig06_clusters_lisa_rim          = list(p = p_lisa,             w = 16,   h = 12),
  fig07_hotspots_rim_getis         = list(p = p_getis,            w = 16,   h = 12)
)

walk2(names(fig_list), fig_list, function(nome, cfg) {
  ggsave(
    filename = file.path(dir_out2, paste0(nome, '.png')),
    plot     = cfg$p,
    width    = cfg$w,
    height   = cfg$h,
    dpi      = 300
  )
  message('Salvo: ', nome)
})

