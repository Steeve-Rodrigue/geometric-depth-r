plot_density_court <- function(data, col = "red", sigma =4) {
  
  # 1. Extraction des données du joueur
  df_shots <- data
  
  # 2. Calcul de la densité spatiale (spatstat)
  # On fixe les limites exactes pour correspondre au repère du terrain (-27.5 à 27.5, -47.5 à 7.5)
  fenetre_terrain <- owin(c(-25, 25), c(-47.1, 0))
  ppp_shots <- ppp(x = df_shots$LOC_X, y = df_shots$LOC_Y, window = fenetre_terrain)
  densite_shots <- density(ppp_shots, sigma = sigma)
  
  # 3. Conversion en data.frame pour ggplot
  df_densite <- as.data.frame(densite_shots)
  colnames(df_densite)[3] <- "v" # Sécurisation du nom de la colonne d'intensité
  
  p <- ggplot(data = data.frame(x = 0, y = 0), aes(x, y))
  p <- drawNBAcourt(p, full = FALSE, size = 0.75, col = "black")
  p <- p + geom_raster(data = df_densite, 
                       aes(x = x, y = y, fill = v), 
                       interpolate = TRUE, 
                       alpha = 0.8) + # alpha < 1 permet de voir le terrain à travers
    
    # Palette de couleurs : du blanc transparent au rouge opaque
    scale_fill_gradient(low = "transparent", high = col) +
    scale_x_continuous(limits = c(-27.5, 27.5)) +
    scale_y_continuous(limits = c(-47.5, 7.5)) +
    theme_void() +
    theme(
      legend.position = "none", # Masquer la légende pour garder le terrain pur
      plot.margin = margin(0, 0, 0, 0, "cm"),
      panel.spacing = unit(0, "lines"),
      panel.background = element_rect(fill = "white", colour = "white"),
      plot.background  = element_rect(fill = "white", colour = "white")
    ) 
  
  return(p)
}

# ====================================================================================
# EXEMPLE D'UTILISATION :
# ====================================================================================
plot_density_court(shots_made_by_player[[3]] , sigma = 4)
#=====================================================================================================

# ---------------------------------------------------
# 2. CALCUL DE LA MATRICE DE DISTANCE
# ---------------------------------------------------
n <- length(shots_made_by_player)
D <- matrix(0, n, n)

for(i in 1:n){
  for(j in i:n){
    
    # Utilisation directe de ta fonction sur les dataframes d'origine
    dij <- evaluer_distance(
      shots_made_by_player[[i]],
      shots_made_by_player[[j]],
      sigma = 3
    )
    
    D[i, j] <- dij
    D[j, i] <- dij
  }
}

rownames(D) <- names(shots_by_player)
colnames(D) <- names(shots_by_player)

# Affichage de la matrice et du Heatmap
D
heatmap(D)


# ---------------------------------------------------
# 3. VISUALISATION ISO-MDS
# ---------------------------------------------------
# Un petit ajustement au cas où de légères approximations numériques de spatstat 
# donneraient des distances négatives infimes (ex: -1e-16), on force le plancher à 0.
D[D < 0] <- 0

mds_iso <- isoMDS(D, k = 2)

plot(mds_iso$points,
     asp = 1,
     pch = 19,
     xlab = "Dim 1",
     ylab = "Dim 2",
     main = "NBA Players - Spatial KDE Distance Matrix")

text(mds_iso$points,
     labels = rownames(D),
     pos = 3,
     cex = 0.8)

#==================================================================

# ==============================================================================
# SCÉNARIO 1 : Étude de puissance — Perturbation d'intensité globale
# ==============================================================================

library(spatstat)

# --- paramètres globaux -------------------------------------------------------
W_GLOBAL          <- owin(c(0, 1), c(0, 1))
LAMBDA_0_GLOBAL   <- 20
N_MATCHES_GLOBAL  <- 15

# ==============================================================================
# FONCTIONS DE SIMULATION
# ==============================================================================

#' Simule un échantillon de "matchs" sous H0 (Processus de Poisson Homogène)
simulate_sample_H0 <- function(n_matches = N_MATCHES_GLOBAL,
                               lambda0   = LAMBDA_0_GLOBAL,
                               W         = W_GLOBAL) {
  replicate(n = n_matches, expr = {
    sim_ppp <- rpoispp(lambda = lambda0, win = W)
    list(coords = cbind(sim_ppp$x, sim_ppp$y))
  }, simplify = FALSE)
}

#' Simule un échantillon sous H1, scénario 1 (intensité perturbée)
simulate_sample_H1_sc1 <- function(delta,
                                   n_matches = N_MATCHES_GLOBAL,
                                   lambda0   = LAMBDA_0_GLOBAL,
                                   W         = W_GLOBAL) {
  lambda_alternative <- lambda0 * (1 + delta)
  replicate(n = n_matches, expr = {
    sim_ppp <- rpoispp(lambda = lambda_alternative, win = W)
    list(coords = cbind(sim_ppp$x, sim_ppp$y))
  }, simplify = FALSE)
}

# ==============================================================================
# UNE ITÉRATION DU TEST
# ==============================================================================

#' Une itération Monte-Carlo : génère H0 et H1, applique le test, retourne le rejet
#'
#' @param test_fun Fonction de test prenant (pl_H0, pl_H1) et retournant
#'                 une p-valeur (ex: methode_1, methode_2)
mc_iteration_sc1 <- function(delta,
                             test_fun,
                             alpha     = 0.05,
                             n_matches = N_MATCHES_GLOBAL,
                             lambda0   = LAMBDA_0_GLOBAL,
                             W         = W_GLOBAL) {
  
  pl_H0 <- simulate_sample_H0(n_matches = n_matches, lambda0 = lambda0, W = W)
  pl_H1 <- simulate_sample_H1_sc1(delta = delta, n_matches = n_matches,
                                  lambda0 = lambda0, W = W)
  
  p_value <- test_fun(pl_H0, pl_H1)
  p_value <= alpha
}

# ==============================================================================
# CALCUL DE LA PUISSANCE POUR UN DELTA DONNÉ
# ==============================================================================

#' Estime la puissance du test pour un delta fixé, par répétition Monte-Carlo
power_sc1 <- function(delta,
                      test_fun,
                      B         = 500,
                      alpha     = 0.05,
                      n_matches = N_MATCHES_GLOBAL,
                      lambda0   = LAMBDA_0_GLOBAL,
                      W         = W_GLOBAL) {
  
  reject <- logical(B)
  for (b in seq_len(B)) {
    reject[b] <- mc_iteration_sc1(
      delta     = delta,
      test_fun  = test_fun,
      alpha     = alpha,
      n_matches = n_matches,
      lambda0   = lambda0,
      W         = W)
  }
  mean(reject)
}

# ==============================================================================
# COURBE DE PUISSANCE
# ==============================================================================

delta_grid <- seq(0.5, 0.52, by = 0.01)

power <- sapply(delta_grid, function(d) {
  cat("delta =", d, "\n")
  power_sc1(delta = d, test_fun = wil_test, B = 500)
})

# ==============================================================================
# VISUALISATION
# ==============================================================================

plot(delta_grid,
     power,
     type = "b",
     pch  = 19,
     col  = "steelblue",
     xlab = expression(delta),
     ylab = "Puissance estimée",
     main = "Courbe de puissance — Scénario 1 (intensité globale)",
     ylim = c(0, 1))
abline(h = 0.05, lty = 2, col = "red")
grid()

#---------------------------------------------------------------------------------------


delta_grid <- seq(1.2, 1.23, by = 0.01) 
# On applique la simulation de puissance pour chaque valeur de delta
puissance_par_delta <- sapply(delta_grid, function(d) {
  B <- 10 
  rejets <- replicate(B, {
    mc_iteration_sc1(
      delta     = d,
      test_fun  = test_fun,
      alpha     = alpha,
      n_matches = n_matches,
      lambda0   = lambda0,
      W         = W
    )
  })
  mean(rejets)
})
print(puissance_par_delta)

d = 1
test_fun = wil_test

test_fun = methode_1

test_fun = methode_2
mc_iteration_sc1(
  delta     = d,
  test_fun  = test_fun,
  alpha     = alpha,
  n_matches = N_MATCHES_GLOBAL,
  lambda0   = LAMBDA_0_GLOBAL,
  W         = W
)




















