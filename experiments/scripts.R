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





set.seed(0)
W_GLOBAL          <- owin(c(0, 1), c(0, 1))
LAMBDA_0_GLOBAL   <- 20
N_MATCHES_GLOBAL  <- 15

delta = 1

f_gradient <- function(x, y) 2 * 20 * (delta * x + (1 - delta) * 0.5)

replicate(n = 15, expr = {
  sim_ppp <- rpoispp(lambda = f_gradient, win =  owin(c(0, 1), c(0, 1)))
  list(coords = cbind(sim_ppp$x, sim_ppp$y))
}, simplify = FALSE)



plot(sim_ppp[[1]])


str(sim_pp)



a <-rpoispp(lambda = f_gradient, win =  owin(c(0, 1), c(0, 1)))
list(coords = cbind(sim_ppp$x, sim_ppp$y))

str(a)
plot(a)








