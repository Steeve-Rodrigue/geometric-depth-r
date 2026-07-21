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


# ===========================================================================================================================
library(cluster)

# 1. Définir la grille des valeurs de 'probs' à tester
v_probs <- seq(0.1, 0.9, by = 0.05) 
scores_silhouette <- numeric(length(v_probs))

# Nombre de joueurs
n <- length(data_by_player)

# Initialisation d'une barre de progression pour suivre l'avancement
pb <- txtProgressBar(min = 0, max = length(v_probs), style = 3)

# 2. Boucle sur les différentes valeurs de probs
for (p_idx in seq_along(v_probs)) {
  p <- v_probs[p_idx]
  
  # --- Étape A : Filtrage Tukey avec le probs actuel ---
  data_typic_made_all_players <- lapply(data_made_by_player, function(joueur_data) {
    prof_matchs <- depths.Tukey(
      joueur_data, joueur_data,
      nDirs = 250, subs = FALSE, exactEst = FALSE,
      parConst1 = -2, parConst2 = 5
    )
    seuil_matchs <- quantile(prof_matchs, probs = p, na.rm = TRUE)
    joueur_filtre <- joueur_data[prof_matchs >= seuil_matchs]
    return(joueur_filtre)
  })
  
  # --- Étape B : Reconstruction des dataframes de tirs ---
  shots_made_by_player <- lapply(data_typic_made_all_players, function(joueur_list) {
    joueur_df <- do.call(rbind, lapply(names(joueur_list), function(id) {
      match <- joueur_list[[id]]
      if (is.null(match$coords) || nrow(match$coords) == 0) return(NULL)
      data.frame(
        idGame    = id,
        dateGame  = match$date,
        LOC_X     = match$coords[, 1],
        LOC_Y     = match$coords[, 2],
        SHOT_MADE = TRUE
      )
    }))
    return(joueur_df)
  })
  
  # --- Étape C : Calcul de la matrice de distance D ---
  D <- matrix(0, n, n)
  for(i in 1:n){
    for(j in i:n){
      dij <- evaluer_distance(
        shots_made_by_player[[i]],
        shots_made_by_player[[j]]
      )
      D[i, j] <- dij
      D[j, i] <- dij
    }
  }
  
  # --- Étape D : CAH et calcul du score de Silhouette ---
  # Ajout d'un tryCatch au cas où un cas extrême génère des erreurs de clustering
  tryCatch({
    cah_core <- hclust(as.dist(D), method = "ward.D2")
    cluster_assignments <- cutree(cah_core, k = 2) # Tu as mis k=2 (le commentaire disait 3)
    
    sil <- silhouette(cluster_assignments, as.dist(D))
    scores_silhouette[p_idx] <- mean(sil[, 3])
  }, error = function(e) {
    scores_silhouette[p_idx] <- NA # En cas de problème (ex: plus assez de tirs)
  })
  
  setTxtProgressBar(pb, p_idx)
}
close(pb)

# 3. Création du graphique de la courbe
plot(v_probs, scores_silhouette, type = "b", pch = 19, col = "blue",
     xlab = "Seuil de quantile (probs)",
     ylab = "Score de silhouette moyen",
     main = "Évolution du Silhouette Score en fonction du filtrage Tukey",
     panel.first = grid())

# Optionnel : Ajouter une ligne rouge pour repérer le maximum
opt_p <- v_probs[which.max(scores_silhouette)]
abline(v = opt_p, col = "red", lty = 2)
text(opt_p, min(scores_silhouette, na.rm=TRUE), labels = paste("Optimum =", opt_p), pos = 4, col = "red")

#===================================================================================================================================



# --- paramètres globaux -------------------------------------------------------
set.seed(0)
W <- W_GLOBAL <- owin(c(0, 1), c(0, 1))
lambda0 <- LAMBDA_0_GLOBAL   <- 20
n_matches <- N_MATCHES_GLOBAL  <- 15
alpha <- 0.05
B <- 150

delta <- 2

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
  
  p_value <- test_fun(pl_H0, pl_H1)$p_value
  p_value <= alpha
}

# ==============================================================================
# CALCUL DE LA PUISSANCE POUR UN DELTA DONNÉ
# ==============================================================================

#' Estime la puissance du test pour un delta fixé, par répétition Monte-Carlo
power_sc1 <- function(delta,
                      test_fun,
                      B         = 50,
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

delta_grid <- seq(0, 1, by = 0.1)

power_wil_test <- sapply(1:1, function(d) {
  cat("delta =", d, "\n")
  power_sc1(delta = d, test_fun = wil_test, B = 100)
})

power_pool_method <- sapply(delta_grid, function(d) {
  cat("delta =", d, "\n")
  power_sc1(delta = d, test_fun = pool_method, B = 100)
})
# 
power_no_pool_method <- sapply(delta_grid, function(d) {
  cat("delta =", d, "\n")
  power_sc1(delta = d, test_fun = no_pool_method, B = 100)
})
# ==============================================================================
# VISUALISATION MULTI-MÉTHODES
# ==============================================================================

# 1. Première méthode (ex: wil_test) - Initialise le graphique
plot(delta_grid,
     power_wil_test,          
     type = "b",
     pch  = 19,
     cex  = 0.6,              # Réduit la taille des ronds
     col  = "darkviolet",
     xlab = expression(delta),
     ylab = "Puissance estimée",
     main = "Courbe de puissance — Scénario 1 (intensité globale)",
     ylim = c(0, 1))

# 2. Deuxième méthode - Superposition
lines(delta_grid,
      power_pool_method,         
      type = "b",
      pch  = 19,
      cex  = 0.6,              # Même taille réduite
      col  = "blue")

# 3. Troisième méthode - Superposition
lines(delta_grid,
      power_no_pool_method,         
      type = "b",
      pch  = 19,
      cex  = 0.6,              # Même taille réduite
      col  = "green")

# --- Éléments de légende et repères -------------------------------------------
abline(h = 0.05, lty = 2, col = "red") # Ligne de niveau alpha = 5%
grid()

# Ajout d' une légende pour distinguer les 3 méthodes
legend("topleft",
       legend = c("Wilcoxon (Tukey)", "pool_method_ks", "no_pool_ks"),
       col = c("darkviolet", "blue", "green"),
       lty = 1,
       pch = 19,
       pt.cex = 0.6,
       cex = 0.6,# Réduit aussi la taille des ronds dans la légende
       bty = "n")              # Enlève le cadre de la légende






# --- paramètres globaux -------------------------------------------------------

A <- replicate(n = 1, expr = {
    sim_ppp <- rpoispp(lambda = lambda0, win = W)
    list(coords = cbind(sim_ppp$x, sim_ppp$y))
  }, simplify = FALSE)



set.seed(0)
W <- W_GLOBAL <- owin(c(0, 1), c(0, 1))
lambda0 <- LAMBDA_0_GLOBAL   <- 20
n_matches <- N_MATCHES_GLOBAL  <- 75
alpha <- 0.05
B <- 100
delta <- 1


kappa <- 10
r_max <-0.05


test_fun <- wil_test #no permutation so no histogram
test_fun <- pool_method
test_fun <- no_pool_method_ref_fix 
test_fun <- no_pool_method_ref_perm
test_fun <- pool_method_two_ref
test_fun <- no_pool_method_two_test_ref_fix
test_fun <- ddplot_bootstrap_t_fixed_ref




pl_H0 <- replicate(n = n_matches, expr = {
                  sim_ppp <- rpoispp(lambda = lambda0, win = W)
                  list(coords = cbind(sim_ppp$x, sim_ppp$y))
                }, simplify = FALSE)
#=================================================================================================
#Scenario 1 : 
set.seed(42)
delta <- 0.5
lambda_alternative <- lambda0 * (1 + delta)

pl_H1 <- replicate(n = n_matches, expr = {
          sim_ppp <- rpoispp(lambda = lambda_alternative, win = W)
          list(coords = cbind(sim_ppp$x, sim_ppp$y))
        }, simplify = FALSE)

#Scenario 2 : 
delta <- 4.8
f_gradient <- function(x, y) 2 * lambda0 * (delta * x + (1 - delta) * 0.5)

pl_H1 <- replicate(n = n_matches, expr = {
                  sim_ppp <- rpoispp(lambda = f_gradient, win = W)
                  list(coords = cbind(sim_ppp$x, sim_ppp$y))
                }, simplify = FALSE)
test_fun(pl_H0, pl_H1)$p_value
#Scenario 3 :
delta <- 1
kappa <- 5
mu_thomas <- lambda0 / kappa
sigma_max <- 0.10
sigma_min <- 0.02
sigma     <- sigma_max * (1 - delta) + sigma_min * delta

pl_H1 <- replicate(n = n_matches, expr = {
          sim_ppp <- rThomas(kappa = kappa, scale = sigma, mu = mu_thomas, win = W)
          list(coords = cbind(sim_ppp$x, sim_ppp$y))
        }, simplify = FALSE)

#Scenario 4 :
delta <- 4
r_max <-0.05
r_hc       <- delta * r_max
lambda_mat <- lambda0 * 3

pl_H1 <-replicate(n = n_matches, expr = {
          if (r_hc == 0) {
            sim_ppp <- rpoispp(lambda = lambda0, win = W)
          } else {
            sim_ppp <- rMaternII(kappa = lambda_mat, r = r_hc, win = W)
          }
          list(coords = cbind(sim_ppp$x, sim_ppp$y))
        }, simplify = FALSE)

#=================================================================================================
plot(pl_H0[[1]]$coords, xlim = c(0, 1), ylim = c(0, 1), main = "Sample H0", xlab = "X", ylab = "Y")

dev.new()
plot(pl_H1[[1]]$coords, xlim = c(0, 1), ylim = c(0, 1), main = "Sample H1", xlab = "X", ylab = "Y")

set.seed(0)

pl_H0 <- replicate(n = n_matches, expr = {
  sim_ppp <- rpoispp(lambda = lambda0, win = W)
  list(coords = cbind(sim_ppp$x, sim_ppp$y))
}, simplify = FALSE)

pl_H1 <- replicate(n = n_matches, expr = {
  sim_ppp <- rpoispp(lambda = f_gradient, win = W)
  list(coords = cbind(sim_ppp$x, sim_ppp$y))
}, simplify = FALSE)

resultat <- test_fun(pl_H0, pl_H1)


p_value <- resultat$p_value
p_value
p_value <= alpha


hist(resultat$T_perm,
     col    = "steelblue",
     border = "black",
     main   = "Distribution de T sous H0",
     xlab   = "T",
     xlim   = range(c(resultat$T_perm, resultat$T_obs)))

abline(v = resultat$T_obs, col = "red", lwd = 2)
legend("topright", legend = c("T_obs"), col = "red", lwd = 2)


reject <- logical(B)
for (b in seq_len(B)) {
  reject[b] <- mc_iteration_sc2(
    delta     = delta,
    test_fun  = test_fun,
    alpha     = alpha,
    n_matches = n_matches,
    lambda0   = lambda0,
    W         = W)
}
mean(reject)

pl_1 = nba_data_depth_made(data_by_player[[13]])
pl_2 = nba_data_depth_made(data_by_player[[14]])
set.seed(0)
figure <- ddplot_nba(pl_1, pl_2, Ndirs = 250,
                                          parConst1 = -2, parConst2 = 5)






