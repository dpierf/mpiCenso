#-------------------------------------------------------------------------------#
#                                                                               #
#                  REVISITANDO A TESE DE DOUTORADO - PROJETO T                  #
#                                                                               #
#-------------------------------------------------------------------------------#

# ============= FASE 8 - Análise de Redes e Modelos Gravitacionais ============= #

source('R/00_packages.R')


# -- Carregamento de dados ----

# Bases (original e agregada por RIM) do MPI
mpi      <- read_parquet('data/04_output/mpi.parquet')
mpi_rim  <- read_parquet('data/04_output/mpi_rims.parquet')

# Base de-para de regiões imediatas
cw_rim <- read_parquet('data/04_output/crosswalk_mun_rim.parquet')

cw_rim <- cw_rim |>
  rename(code_muni7 = code_muni) |>
  mutate(code_muni6 = case_when(ano == 1980 ~ code_muni7, TRUE ~ code_muni7 %/% 10L)) |>
  dplyr::select(ano, code_muni7, code_muni6, rim)

# Shapefile de regiões imediatas
rim_sf <- readRDS(here::here('data', '00_maps', 'imm_regions_dissolved.rds')) |>
  rename(rim = code_immediate) |>
  mutate(rim = as.integer(rim))


# -- Construção de parâmetros ----
anos_grav  <- c(1991L, 2000L, 2010L)
grupos_mig <- c('nao_pobre', 'pobre_mod', 'pobre_grave')

vars_mig <- list(
  `1991` = c(munD     = 'code_muni', munO_uf  = 'V0321',  munO_mun = 'V3211',
             peso     = 'V7300',     V0300    = 'V0102'),
  `2000` = c(munD     = 'code_muni', munO     = 'V4250',
             peso     = 'P001',      V0300    = 'V0300'),
  `2010` = c(munD     = 'code_muni', munO     = 'V6264',
             peso     = 'V0010',     V0300    = 'V0300')
)

codigos_uf_2000 <- c(
  1100001L, 1200005L, 1300003L, 1400001L, 1500008L, 1600006L, 1700004L,
  2100006L, 2200004L, 2300002L, 2400000L, 2500007L, 2600005L, 2700003L,
  2800001L, 2900009L, 3100009L, 3200003L, 3300001L, 3500006L,
  4100004L, 4200002L, 4300000L, 5000005L, 5100003L, 5200001L, 5400007L
)


# -- Tratamento de dados ----

# Gerando tabela de fluxos
fluxos_raw <- map_dfr(anos_grav, function(a) {
  
  # grupo_mig via score do mpi_final: um registro distinto por domicílio
  grupo_lookup <- mpi |>
    dplyr::filter(ano == a) |>
    dplyr::mutate(
      grupo_mig = case_when(
        score < 1/3 ~ 'nao_pobre',
        score < 1/2 ~ 'pobre_mod',
        TRUE        ~ 'pobre_grave'
        )
      ) |>
    dplyr::distinct(ano, V0300, code_muni, score, grupo_mig)
  
  v <- vars_mig[[as.character(a)]]

  # Padronizando o código de município 5-anos atrás  
  if (a == 1991L) {
    pes <- censobr::read_population(
      year    = a,
      columns = unname(v)
    ) |>
      collect() |>
      rename(VAR_MUN_ATUAL = all_of(v[['munD']]),
             VAR_PESO      = all_of(v[['peso']]),
             VAR_V0300     = all_of(v[['V0300']])) |>
      dplyr::mutate(VAR_MUN_ANT = as.integer(.data[[v[['munO_uf']]]]) * 10000L +
                      as.integer(.data[[v[['munO_mun']]]]),
                    VAR_PESO    = VAR_PESO / 1e8
      )
  } else {
    pes <- censobr::read_population(
      year    = a,
      columns = unname(v)
    ) |>
      collect() |>
      rename(VAR_MUN_ATUAL = all_of(v[['munD']]),
             VAR_MUN_ANT   = all_of(v[['munO']]),
             VAR_PESO      = all_of(v[['peso']]),
             VAR_V0300     = all_of(v[['V0300']])) |>
      dplyr::mutate(VAR_MUN_ANT = as.integer(VAR_MUN_ANT))
  }
  
  # Filtros específicos por ano
  pes <- switch(as.character(a),
                `1991` = filter(pes,
                                !is.na(VAR_MUN_ANT),
                                VAR_MUN_ANT != 0L,
                                VAR_MUN_ANT %% 10000L  != 0L,          # UF0000, 700000, 540000, 990000
                                VAR_MUN_ANT <  800000L                 # internacionais
                ),
                `2000` = filter(pes,
                                !is.na(VAR_MUN_ANT),
                                VAR_MUN_ANT != 0L,
                                VAR_MUN_ANT != VAR_MUN_ATUAL,
                                !VAR_MUN_ANT %in% codigos_uf_2000
                ),
                `2010` = filter(pes,
                                !is.na(VAR_MUN_ANT),
                                VAR_MUN_ANT != 0L,
                                VAR_MUN_ANT != VAR_MUN_ATUAL,
                                VAR_MUN_ANT %% 100000L != 99999L,
                                !VAR_MUN_ANT %in% c(8888888L, 9999999L)
                )
  )
  
  pes |>
    dplyr::mutate(
      code_muni = as.integer(VAR_MUN_ATUAL),
      V0300     = as.character(VAR_V0300)
    ) |>
    dplyr::left_join(grupo_lookup, by = c('code_muni', 'V0300')) |>
    dplyr::filter(!is.na(grupo_mig)) |>
    dplyr::mutate(
      ano       = a,
      code_orig = as.integer(VAR_MUN_ANT),
      code_dest = as.integer(VAR_MUN_ATUAL)
    ) |>
    dplyr::group_by(ano, grupo_mig, code_orig, code_dest) |>
    summarise(volume = sum(VAR_PESO, na.rm = TRUE), .groups = 'drop')
})

# Junção de base com tabela de RIM
fluxos_rim <- fluxos_raw |>
  
  # origem 1991: município com 6 dígitos
  left_join(
    cw_rim |>
      filter(ano == 1991L) |>
      distinct(code_muni6, rim) |>
      rename(rim_o6 = rim),
    by = c('code_orig' = 'code_muni6')
  ) |>
  
  # origem 2000 e 2010: município com 7 dígitos
  left_join(
    cw_rim |>
      filter(ano %in% c(2000L, 2010L)) |>
      distinct(ano, code_muni7, rim) |>
      rename(rim_o7 = rim),
    by = c('ano', 'code_orig' = 'code_muni7')
  ) |>
  
  mutate(rim_origem = coalesce(rim_o6, rim_o7)) |>
  dplyr::select(-rim_o6, -rim_o7) |>
  
  # destino: município sempre com 7 dígitos
  left_join(
    cw_rim |>
      distinct(ano, code_muni7, rim) |>
      rename(rim_destino = rim),
    by = c('ano', 'code_dest' = 'code_muni7')
  ) |>
  
  dplyr::filter(!is.na(rim_origem), !is.na(rim_destino),
         rim_origem != rim_destino) |>
  group_by(ano, grupo_mig, rim_origem, rim_destino) |>
  dplyr::summarise(volume = sum(volume), .groups = 'drop')

# Construindo matriz de distâncias
centr    <- rim_sf |> st_centroid() |> dplyr::select(rim)
nb_rim   <- poly2nb(rim_sf, queen = TRUE)
rim_ids  <- centr$rim   # ordem idêntica à de nb_rim
dist_mat <- st_distance(centr, centr) |> units::drop_units()  # metros
rownames(dist_mat) <- rim_ids
colnames(dist_mat) <- rim_ids

dist_long <- as.data.frame(as.table(dist_mat), stringsAsFactors = FALSE) |>
  rename(rim_origem = Var1, rim_destino = Var2, dist_km = Freq) |>
  mutate(dist_km   = dist_km / 1000,
         rim_origem  = as.integer(rim_origem),
         rim_destino = as.integer(rim_destino)) |>
  filter(rim_origem != rim_destino)


# -- Controles geográficos ----
uf_rim <- tibble(rim = as.integer(rim_ids)) |>
  mutate(uf = as.integer(substr(as.character(rim), 1, 2)))

contig_long <- map_dfr(seq_along(nb_rim), function(i) {
  vizinhos <- nb_rim[[i]]
  vizinhos <- vizinhos[vizinhos != 0L]   # spdep usa 0 para "sem vizinho"
  if (length(vizinhos) == 0) return(tibble())
  tibble(
    rim_origem  = rim_ids[i],
    rim_destino = rim_ids[vizinhos]
  )
}) |> mutate(contiguidade = TRUE)

controles_geo <- dist_long |>
  dplyr::select(rim_origem, rim_destino) |>
  mutate(
    rim_origem  = as.integer(rim_origem),
    rim_destino = as.integer(rim_destino)
  ) |>
  left_join(uf_rim, by = c('rim_origem'  = 'rim')) |> rename(uf_orig = uf) |>
  left_join(uf_rim, by = c('rim_destino' = 'rim')) |> rename(uf_dest = uf) |>
  mutate(mesma_uf  = (uf_orig == uf_dest)) |>
  mutate(mesma_reg = (substr(uf_orig,1,1) == substr(uf_dest,1,1))) |>
  left_join(contig_long, by = c('rim_origem', 'rim_destino')) |>
  mutate(contiguidade = replace_na(contiguidade, FALSE)) |>
  dplyr::select(rim_origem, rim_destino, mesma_uf, mesma_reg, contiguidade)

# Atributos a nível de RIM e Ano
mpi_atb <- mpi_rim |>
  dplyr::select(ano, rim, MPI, score_med, H, A, 
                d1_med, d2_med, d3_med, d4_med, d5_med, 
                n_exp) |> 
  mutate(ano = as.integer(as.character(ano)))


# -- Edge list (fluxos interregionais) ----

# Valores a nível de grupo de privação multidimensional
fluxos_grav <- fluxos_rim |>
  left_join(dist_long |> 
              mutate(
                rim_origem  = as.integer(rim_origem),
                rim_destino = as.integer(rim_destino)
              ),     by = c('rim_origem', 'rim_destino')) |>
  left_join(controles_geo |>
              mutate(
                rim_origem  = as.integer(rim_origem),
                rim_destino = as.integer(rim_destino)
              ),     by = c('rim_origem', 'rim_destino')) |>
  left_join(mpi_atb, by = c('ano', 'rim_origem' = 'rim')) |>
  rename(MPI_orig   = MPI,       H_orig     = H,         A_orig     = A,
         n_exp_orig = n_exp,     score_orig = score_med,
         d1_orig    = d1_med,    d2_orig    = d2_med,    d3_orig    = d3_med,
         d4_orig    = d4_med,    d5_orig    = d5_med) |>
  left_join(mpi_atb, by = c('ano', 'rim_destino' = 'rim')) |>
  rename(MPI_dest   = MPI,       H_dest     = H,         A_dest     = A,
         n_exp_dest = n_exp,     score_dest = score_med,
         d1_dest    = d1_med,    d2_dest    = d2_med,    d3_dest    = d3_med,
         d4_dest    = d4_med,    d5_dest    = d5_med)

# Agregando o total para cada RIM-Ano
fluxos_grav_total <- fluxos_grav |>
  group_by(ano, rim_origem, rim_destino, dist_km,
           mesma_uf, mesma_reg, contiguidade) |>
  summarise(volume    = sum(volume),
            .groups   = 'drop') |>
  mutate(grupo_mig = 'total')

fluxos_grav_full <- bind_rows(fluxos_grav, fluxos_grav_total)


# -- Estatísticas descritivas de redes ----
metricas_rede <- fluxos_grav_full |>
  group_by(ano, grupo_mig) |>
  group_map(function(df, key) {
    
    nodes <- mpi_rim |>
      filter(ano == key$ano) |>
      dplyr::select(rim, MPI, H, A)
    
    g <- graph_from_data_frame(
      d        = dplyr::select(df, rim_origem, rim_destino, volume, dist_km),
      vertices = nodes,
      directed = TRUE
    )
    
    w_vol  <- E(g)$volume
    w_dist <- 1 / w_vol
    
    g_und <- as_undirected(
      g, mode = 'collapse',
      edge.attr.comb = list(volume = 'sum', dist_km = 'mean', 'ignore')
    )
    
    hits <- hits_scores(g, weights = w_vol)
    
    tibble(
      ano              = key$ano,
      grupo_mig        = key$grupo_mig,
      rim              = as.integer(V(g)$name),
      
      # Degree
      indegree         = degree(g, mode = 'in'),
      outdegree        = degree(g, mode = 'out'),
      degree           = degree(g, mode = 'all'),
      
      # Weighted degree (força)
      w_indegree       = strength(g, mode = 'in',  weights = w_vol),
      w_outdegree      = strength(g, mode = 'out', weights = w_vol),
      w_degree         = strength(g, mode = 'all', weights = w_vol),
      net_flow         = strength(g, mode = 'in',  weights = w_vol) -
        strength(g, mode = 'out', weights = w_vol),
      
      # Métricas de caminho (w_dist)
      eccentricity     = eccentricity(g, weights = w_dist),
      closeness        = closeness(g, mode = 'all',
                                   weights = w_dist, normalized = TRUE),
      harm_closeness   = harmonic_centrality(g, mode = 'all',
                                             weights = w_dist),
      betweenness      = betweenness(g, directed = TRUE,
                                     weights = w_dist, normalized = TRUE),
      
      # HITS (w_vol)
      authority        = hits$authority,
      hub              = hits$hub,           
      pagerank         = page_rank(g, weights = w_vol)$vector,           
      
      # Eigenvector centrality
      eigencentrality  = eigen_centrality(g, directed = TRUE,
                                          weights = w_vol)$vector,
      
      # Clustering local (não dirigido)
      clustering       = transitivity(g_und, type = 'local',
                                      weights  = E(g_und)$volume,
                                      isolates = 'zero'),
      
      coreness         = coreness(g_und),
      
      # Modularity class — Louvain (não dirigido, equivalente ao Gephi)
      modularity_class = as.integer(membership(cluster_louvain(
        g_und, weights = E(g_und)$volume
      )))
    )
  }) |>
  bind_rows()

# Visualização gráfica
pal_vibrante <- RColorBrewer::brewer.pal(10, 'Paired')

ordem_grupo_3 <- c('nao_pobre', 'pobre_mod', 'pobre_grave')
lbl_grupo_3   <- c(nao_pobre   = 'Não pobre',
                   pobre_mod   = 'Pobre moderado',
                   pobre_grave = 'Pobre grave')

mapa_metrica <- function(metrica,
                         categorico = FALSE,      # o atributo é categórico ou numérico?
                         divergente = FALSE,      # o atributo tem classes divergentes?
                         titulo     = NULL) {
  
  df_map <- rim_sf |>
    left_join(
      metricas_rede |>
        filter(grupo_mig %in% ordem_grupo_3) |>
        mutate(grupo_mig = factor(grupo_mig, levels = ordem_grupo_3,
                                  labels = lbl_grupo_3),
               ano       = factor(ano)),
      by = 'rim'
    )
  
  vals <- metricas_rede |>
    filter(grupo_mig %in% ordem_grupo_3) |>
    pull(all_of(metrica))
  
  if (categorico) {
    df_map <- df_map |> mutate(valor = factor(.data[[metrica]]))
    n_vals  <- n_distinct(df_map[[metrica]], na.rm = TRUE)
    escala  <- scale_fill_manual(values   = pal_vibrante[seq_len(n_vals)],
                                 na.value = 'grey80', name = NULL)
    
  } else if (divergente) {
    df_map  <- df_map |> mutate(valor = .data[[metrica]])
    lim_abs <- quantile(abs(vals), 0.95, na.rm = TRUE)   # cap nos extremos
    escala  <- scale_fill_gradient2(
      low      = '#E63946',   # vermelho — emissor líquido
      mid      = 'grey93',    # cinza claro — saldo ~0
      high     = '#4CAF50',   # verde — receptor líquido
      midpoint = 0,
      limits   = c(-lim_abs, lim_abs),
      oob      = scales::squish,   # valores fora dos limites → cor do extremo
      name     = NULL
    )
    
  } else {
    df_map <- df_map |> mutate(valor = .data[[metrica]])
    escala  <- scale_fill_viridis_c(na.value = 'grey80', name = NULL,
                                    option   = 'plasma', direction = -1)
  }
  
  ggplot(df_map) +
    geom_sf(aes(fill = valor), color = NA, linewidth = 0) +
    escala +
    facet_grid(grupo_mig ~ ano) +
    labs(title = if (is.null(titulo)) metrica else titulo) +
    theme_void(base_size = 9) +
    theme(strip.text      = element_text(face = 'bold', size = 8),
          legend.position = 'right',
          plot.title      = element_text(face = 'bold', hjust = 0.5),
          panel.spacing   = unit(0.3, 'lines'))
}

# Usos
mapas1a <- mapa_metrica('modularity_class', categorico = TRUE,  divergente = FALSE, titulo = 'Comunidades migratórias por grupo de pobreza')
mapas1b <- mapa_metrica('net_flow',         categorico = FALSE, divergente = TRUE,  titulo = 'Saldo migratório por região imediata e gupo')
mapas1c <- mapa_metrica('coreness',         categorico = FALSE, divergente = FALSE, titulo = 'Centralidade de núcleo por região imediata e grupo')
mapas1d <- mapa_metrica('clustering',       categorico = FALSE, divergente = FALSE, titulo = 'Nível de Clusterização por região imediata e grupo')


# -- Modelos gravitacionais ----

# RIMs que efetivamente aparecem nos dados de migração
rims_validos <- union(fluxos_rim$rim_origem, fluxos_rim$rim_destino)

# Construção do grid completo
todos_pares <- tidyr::crossing(
  rim_origem  = rim_ids,
  rim_destino = rim_ids
) |> filter(rim_origem != rim_destino)

# Criando tabela completa de fluxos
fluxos_ppml <- tidyr::crossing(
  tibble(ano       = anos_grav),
  tibble(grupo_mig = grupos_mig)
) |>
  cross_join(todos_pares) |>
  left_join(fluxos_rim,
            by = c('ano', 'grupo_mig', 'rim_origem', 'rim_destino')) |>
  mutate(volume = replace_na(volume, 0L)) |>
  left_join(dist_long |> 
              mutate(
                rim_origem  = as.integer(rim_origem),
                rim_destino = as.integer(rim_destino)
              ),     by = c('rim_origem', 'rim_destino')) |>
  left_join(controles_geo |>
              mutate(
                rim_origem  = as.integer(rim_origem),
                rim_destino = as.integer(rim_destino)
              ),     by = c('rim_origem', 'rim_destino')) |>
  left_join(mpi_atb |> mutate(ano = as.integer(as.character(ano))),
            by = c('ano', 'rim_origem' = 'rim')) |>
  rename(MPI_orig   = MPI,       H_orig     = H,       A_orig     = A,
         n_exp_orig = n_exp,     score_orig = score_med,
         d1_orig    = d1_med,    d2_orig    = d2_med,   d3_orig    = d3_med,
         d4_orig    = d4_med,    d5_orig    = d5_med) |>
  left_join(mpi_atb |> mutate(ano = as.integer(as.character(ano))),
            by = c('ano', 'rim_destino' = 'rim')) |>
  rename(MPI_dest   = MPI,       H_dest     = H,       A_dest     = A,
         n_exp_dest = n_exp,     score_dest = score_med,
         d1_dest    = d1_med,    d2_dest    = d2_med,   d3_dest    = d3_med,
         d4_dest    = d4_med,    d5_dest    = d5_med)

# Colunas a interpolar (n_exp incluído, pois é a massa populacional)
cols_interp <- c('MPI', 'score_med', 'H', 'A', 'd1_med', 'd2_med', 'd3_med', 'd4_med', 'd5_med', 'n_exp')

# Anos escolhidos para interpolação linear
pesos_interp <- list(
  `1986` = list(anos = c(1980L, 1991L), w = c(5/11, 6/11), ano_censo = 1991L),
  `1995` = list(anos = c(1991L, 2000L), w = c(5/9,  4/9),  ano_censo = 2000L),
  `2005` = list(anos = c(2000L, 2010L), w = c(1/2,  1/2),  ano_censo = 2010L)
)

mpi_wide <- mpi_atb |>
  dplyr::select(rim, ano, all_of(cols_interp)) |>
  pivot_wider(
    names_from  = ano,
    values_from = all_of(cols_interp),
    names_glue  = '{.value}_y{ano}'
  )

# Processo de interpolação para os três anos-alvo
mpi_interp <- imap_dfr(pesos_interp, function(p, alvo) { 
  
  vals <- map_dfc(cols_interp, function(col) {
    c1 <- paste0(col, '_y', p$anos[1])
    c2 <- paste0(col, '_y', p$anos[2])
    tibble(!!col := p$w[1] * mpi_wide[[c1]] +
             p$w[2] * mpi_wide[[c2]])
  })
  
  bind_cols(
    tibble(rim = mpi_wide$rim, ano_censo = p$ano_censo),
    vals
  )
})


# Tabela de fluxos com dados t-5 (origem e destino) e t0 (somente destino)
fluxos_ppml_interp <- fluxos_ppml |>
  
  # Destino em t0
  rename(
    MPI_dest0   = MPI_dest,   score_dest0 = score_dest,
    H_dest0     = H_dest,     A_dest0     = A_dest,
    d1_dest0    = d1_dest,    d2_dest0    = d2_dest,
    d3_dest0    = d3_dest,    d4_dest0    = d4_dest,
    d5_dest0    = d5_dest,    n_exp_dest0 = n_exp_dest
  ) |>
  
  # Origem em t0
  rename(
    MPI_orig0   = MPI_orig,   score_orig0 = score_orig,
    H_orig0     = H_orig,     A_orig0     = A_orig,
    d1_orig0    = d1_orig,    d2_orig0    = d2_orig,
    d3_orig0    = d3_orig,    d4_orig0    = d4_orig,
    d5_orig0    = d5_orig,    n_exp_orig0 = n_exp_orig
  ) |>
  
  # Origem em t-5
  left_join(mpi_interp,
            by = c('ano' = 'ano_censo', 'rim_origem' = 'rim')) |>
  rename(MPI_orig5   = MPI,       score_orig5 = score_med,
         H_orig5     = H,         A_orig5     = A,
         d1_orig5    = d1_med,    d2_orig5    = d2_med,
         d3_orig5    = d3_med,    d4_orig5    = d4_med,
         d5_orig5    = d5_med,    n_exp_orig5 = n_exp) |>
  
  # Destino em t-5
  left_join(mpi_interp,
            by = c('ano' = 'ano_censo', 'rim_destino' = 'rim')) |>
  rename(MPI_dest5   = MPI,       score_dest5 = score_med,
         H_dest5     = H,         A_dest5     = A,
         d1_dest5    = d1_med,    d2_dest5    = d2_med,
         d3_dest5    = d3_med,    d4_dest5    = d4_med,
         d5_dest5    = d5_med,    n_exp_dest5 = n_exp) |>
  
  # Coalesce para RIM não existentes no censo anterior
  mutate(
    MPI_orig5   = coalesce(MPI_orig5,   MPI_orig0),
    n_exp_orig5 = coalesce(n_exp_orig5, n_exp_orig0),
    H_orig5     = coalesce(H_orig5,     H_orig0),
    A_orig5     = coalesce(A_orig5,     A_orig0),
    across(matches('^d[1-5]_orig5$'),
           ~ coalesce(.x, get(sub('5$', '0', cur_column()))))
  )


## -- Modelo 1: MPI agregado ----

# Modelagem
mod1_coef <- fluxos_ppml_interp |>
  group_by(ano, grupo_mig) |>
  group_map(function(df, key) {
    fepois(
      volume ~ log(pmax(MPI_orig5,   1e-6)) + log(pmax(MPI_dest0,   1e-6)) +
        log(pmax(n_exp_orig5, 1))    + log(pmax(n_exp_dest0, 1))    +
        log(dist_km) + contiguidade + mesma_uf + mesma_reg,
      data  = df,
      vcov  = 'hetero'
    ) |>
      broom::tidy() |>
      mutate(ano = key$ano, grupo_mig = key$grupo_mig)
  }) |>
  bind_rows()

# Análise gráfica - helpers
lbl_grupo  <- c(nao_pobre   = 'Não pobre',
                pobre_mod   = 'Pobre moderado',
                pobre_grave = 'Pobre grave')

pal_grupos <- c(
  'Não pobre'      = '#2196F3',
  'Pobre moderado' = '#FF9800',
  'Pobre grave'    = '#E63946'
)

termos_foco <- c(
  'log(pmax(MPI_orig5, 1e-06))' = 'Push: MPI origem (t-5)',
  'log(pmax(MPI_dest0, 1e-06))' = 'Pull: MPI destino (t0)',
  'log(dist_km)'                = 'Distância (log)',
  'contiguidadeTRUE'            = 'Contiguidade',
  'mesma_regTRUE'               = 'Intra-Regional',
  'mesma_ufTRUE'                = 'Intra-Estadual'
)

df_coef <- mod1_coef |>
  filter(term %in% names(termos_foco)) |>
  mutate(
    term      = factor(term, levels = names(termos_foco),
                       labels = termos_foco),
    grupo_mig = factor(grupo_mig, levels = names(lbl_grupo),
                       labels = lbl_grupo),
    ano       = factor(ano),
    ci_low    = estimate - 1.96 * std.error,
    ci_high   = estimate + 1.96 * std.error
  )

# Coeficientes por categoria e ano
p_coef1 <- ggplot(df_coef,
                  aes(x = ano, y = estimate,
                      color = grupo_mig, group = grupo_mig)) +
  geom_hline(yintercept = 0, linetype = 'dashed',
             color = 'grey60', linewidth = 0.4) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.2, linewidth = 0.4,
                position = position_dodge(0.4)) +
  geom_point(size = 2.5, position = position_dodge(0.4)) +
  scale_color_manual(values = pal_grupos, name = NULL) +
  facet_wrap(~ term, scales = 'free_y', ncol = 2) +
  labs(title    = 'Coeficientes do modelo gravitacional - Modelo 1 (MPI)',
       subtitle = 'PPML | push t-5, pull t0 | IC 95%',
       x = NULL, y = 'Estimativa') +
  theme_minimal(base_size = 10) +
  theme(legend.position  = 'bottom',
        strip.text       = element_text(face = 'bold'),
        panel.grid.minor = element_blank(),
        panel.border     = element_rect(color = 'grey70', fill = NA, linewidth = 0.5))

# Trajetória anual dos coeficientes por grupo
p_trajetorias1 <- df_coef |>
  mutate(ano_int = as.integer(as.character(ano))) |>
  ggplot(aes(x = ano_int, y = estimate,
             color = grupo_mig, fill = grupo_mig)) +
  geom_hline(yintercept = 0, linetype = 'dashed',
             color = 'grey60', linewidth = 0.5) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 3.5) +
  scale_color_manual(values = pal_grupos, name = NULL) +
  scale_fill_manual(values  = pal_grupos, name = NULL) +
  scale_x_continuous(breaks = c(1991, 2000, 2010)) +
  facet_wrap(~ term, scales = 'free_y', ncol = 2) +
  labs(title    = 'Trajetória temporal dos coeficientes - Modelo 1 (MPI)',
       subtitle = 'PPML | push t-5, pull t0 | IC 95%',
       x = NULL, y = 'Estimativa') +
  theme_minimal(base_size = 10) +
  theme(legend.position  = 'bottom',
        strip.text       = element_text(face = 'bold'),
        panel.grid.minor = element_blank(),
        panel.border     = element_rect(color = 'grey70', fill = NA,
                                        linewidth = 0.5))

## -- Modelo 2: dimensões do MPI ----

# Modelagem
mod2_coef <- fluxos_ppml_interp |>
  mutate(across(c(d1_orig5:d5_orig5, d1_dest0:d5_dest0), ~ pmax(.x, 1e-6))) |>
  group_by(ano, grupo_mig) |>
  group_map(function(df, key) {
    fepois(
      volume ~ log(d1_orig5) + log(d2_orig5) + log(d3_orig5) +
        log(d4_orig5) + log(d5_orig5) +
        log(d1_dest0) + log(d2_dest0) + log(d3_dest0) +
        log(d4_dest0) + log(d5_dest0) +
        log(pmax(n_exp_orig5, 1)) + log(pmax(n_exp_dest0, 1)) +
        log(dist_km) + contiguidade + mesma_uf + mesma_reg,
      data  = df,
      vcov  = 'hetero'
    ) |>
      broom::tidy() |>
      mutate(ano = key$ano, grupo_mig = key$grupo_mig)
  }) |>
  bind_rows()

# Análise gráfica
df_mod2 <- mod2_coef |>
  filter(grepl('^log\\(d[1-5]_', term)) |>
  mutate(
    dim       = paste0('D', str_extract(term, '[1-5]')),
    direction = if_else(grepl('_orig', term),
                        'Push: origem (t-5)',
                        'Pull: destino (t0)'),
    grupo_mig = factor(grupo_mig, levels = names(lbl_grupo),
                       labels = lbl_grupo),
    sig       = p.value < 0.05
  )

p_coef2 <- ggplot(df_mod2, aes(x = factor(ano), y = grupo_mig, fill = estimate)) +
  geom_tile(aes(alpha = if_else(sig, 1, 0.35)),
            color = 'white', linewidth = 0.5) +
  geom_text(aes(label = round(estimate, 2),
                color  = if_else(abs(estimate) > 2, 'white', 'grey20')),
            size = 2.8) +
  scale_fill_gradient2(low      = '#E63946',
                       mid      = 'grey93',
                       high     = '#4CAF50',
                       midpoint = 0,
                       name     = 'Estimativa') +
  scale_color_identity() +
  scale_alpha_identity() +
  facet_grid(direction ~ dim) +
  labs(title    = 'Coeficientes por dimensão - Modelo 2 (dimensões MPI)',
       subtitle = 'Verde = amplifica fluxo | Vermelho = reduz | Transparente = não significativo',
       x = NULL, y = NULL) +
  theme_minimal(base_size = 9) +
  theme(strip.text       = element_text(face = 'bold'),
        panel.grid       = element_blank(),
        panel.border     = element_rect(color = 'grey70',
                                        fill = NA, linewidth = 0.5),
        legend.position  = 'right')


## -- Modelo 3: trocando MPI por H*A ----

# Modelagem
mod3_coef <- fluxos_ppml_interp |>
  group_by(ano, grupo_mig) |>
  group_map(function(df, key) {
    fepois(
      volume ~ log(pmax(H_orig5, 1e-6)) + log(pmax(A_orig5, 1e-6)) +
        log(pmax(H_dest0, 1e-6)) + log(pmax(A_dest0, 1e-6)) +
        log(pmax(n_exp_orig5, 1)) + log(pmax(n_exp_dest0, 1)) +
        log(dist_km) + contiguidade + mesma_uf + mesma_reg,
      data  = df,
      vcov  = 'hetero'
    ) |>
      broom::tidy() |>
      mutate(ano = key$ano, grupo_mig = key$grupo_mig)
  }) |>
  bind_rows()

# Visualização gráfica
termos_mod3 <- c(
  'log(pmax(H_orig5, 1e-06))' = 'Push: Incidência H (t-5)',
  'log(pmax(A_orig5, 1e-06))' = 'Push: Intensidade A (t-5)',
  'log(pmax(H_dest0, 1e-06))' = 'Pull: Incidência H (t0)',
  'log(pmax(A_dest0, 1e-06))' = 'Pull: Intensidade A (t0)'
)

df_mod3 <- mod3_coef |>
  filter(term %in% names(termos_mod3)) |>
  mutate(
    term      = factor(term, levels = names(termos_mod3),
                       labels = termos_mod3),
    grupo_mig = factor(grupo_mig, levels = names(lbl_grupo),
                       labels = lbl_grupo),
    ano       = as.integer(ano),
    ci_low    = estimate - 1.96 * std.error,
    ci_high   = estimate + 1.96 * std.error
  )

p_trajetorias3 <- ggplot(df_mod3,
       aes(x = ano, y = estimate,
           color = grupo_mig, fill = grupo_mig)) +
  geom_hline(yintercept = 0, linetype = 'dashed',
             color = 'grey60', linewidth = 0.5) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 3.5) +
  scale_color_manual(values = pal_grupos, name = NULL) +
  scale_fill_manual(values  = pal_grupos, name = NULL) +
  scale_x_continuous(breaks = c(1991, 2000, 2010)) +
  facet_wrap(~ term, scales = 'free_y', ncol = 2) +
  labs(title    = 'Decomposição H × A - Modelo 3',
       subtitle = 'PPML | push t-5, pull t0 | IC 95%',
       x = NULL, y = 'Estimativa') +
  theme_minimal(base_size = 10) +
  theme(legend.position  = 'bottom',
        strip.text       = element_text(face = 'bold'),
        panel.grid.minor = element_blank(),
        panel.border     = element_rect(color = 'grey70',
                                        fill = NA, linewidth = 0.5))


# EXPORTAÇÃO -------------------------------------------------------------------

dir_out <- 'output/05_spatial'
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

## Gráficos
fig_list <- list(
  fig01a_comunidades        = list(p = mapas1a,         w = 16,   h = 12),
  fig01b_saldo_migratorio   = list(p = mapas1b,         w = 16,   h = 12),
  fig01c_centralidades      = list(p = mapas1c,         w = 16,   h = 12),
  fig01d_clusterizacao      = list(p = mapas1d,         w = 16,   h = 12),
  fig02a_coeficientes_mod1  = list(p = p_coef1,         w = 16,   h = 12),
  fig02b_coeficientes_mod2  = list(p = p_coef2,         w = 16,   h = 12),
  fig03a_trajetorias_mod1   = list(p = p_trajetorias1,  w = 16,   h = 12),
  fig03b_trajetorias_mod3   = list(p = p_trajetorias3,  w = 16,   h = 12)  
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
