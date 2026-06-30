#-------------------------------------------------------------------------------#
#                                                                               #
#                  REVISITANDO A TESE DE DOUTORADO - PROJETO T                  #
#                                                                               #
#-------------------------------------------------------------------------------#

# ================ FASE 0 - Instalação de pacotes necessários ================= #

if (!'here' %in% installed.packages()[, 'Package']) install.packages('here')
require(here)

setwd(here::here())

pacotes <- c('censobr', 'arrow', 'dplyr', 'data.table', 'tidyverse', 'srvyr', 'cluster', 'igraph', 'ggh4x',
             'geobr', 'patchwork', 'scales', 'gt', 'ranger', 'glmnet', 'e1071', 'purrr', 'modelsummary',
             'poLCA', 'factoextra', 'iml', 'treeshap', 'shapviz', 'pROC', 'ggrepel', 'broom', 'oaxaca', 'sf',
             'fixest', 'quantreg', 'frontier', 'plm', 'spdep', 'spatialreg', 'marginaleffects', 'units')

ausentes <- pacotes[!pacotes %in% installed.packages()[, 'Package']]
if (length(ausentes) > 0) install.packages(ausentes)
invisible(lapply(pacotes, library, character.only = TRUE))

rm(ausentes, pacotes) ; gc() ; cat('\014')
