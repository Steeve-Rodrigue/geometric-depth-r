# ============================================================
# Comparaison de deux clusterings de joueurs NBA
# à partir des positions de tirs réussis (saison 2018-19)
#
# Fichiers requis :
#   - doc.RData      : cluster_assignments (partition 1),
#                      partition_finale    (partition 2)
#   - all_tirs.RData : data_typic_made_all_players
#                      (liste joueur -> match -> $coords [x, y])
# ============================================================

load("all_tirs.RData")
load("doc.RData")
d <- data_typic_made_all_players

# Position du panier dans le repère des données
# (terrain de 50 ft de large, x dans [-25, 25] ;
#  cercle à 5.25 ft de la ligne de fond -> y = -41.75)
HX <- 0; HY <- -41.75

# ------------------------------------------------------------
# 1) Features spatiales par joueur
# ------------------------------------------------------------
feats <- t(sapply(d, function(p){
  co   <- do.call(rbind, lapply(p, function(g) g$coords))
  x    <- co[, 1]; y <- co[, 2]
  dist <- sqrt((x - HX)^2 + (y - HY)^2)

  # 3 points : au-delà de l'arc (23.75 ft) ou dans les corners
  # (|x| > 22 ft, dans les 14 premiers ft depuis la ligne de fond)
  is3 <- (dist > 23.75) | (abs(x) > 22 & y < HY + 14)

  c(n             = length(x),
    mean_dist     = mean(dist),
    rim           = mean(dist < 8),                    # près du cercle
    short_mid     = mean(dist >= 8 & dist < 16 & !is3),
    long_mid      = mean(dist >= 16 & !is3),
    three         = mean(is3),
    corner3_share = if (sum(is3) > 4) mean(abs(x[is3]) > 20) else NA)
}))
feats <- as.data.frame(feats)
feats$player <- names(d)

# Les deux vecteurs de clusters sont dans le même ordre que la liste
stopifnot(all(names(d) == names(cluster_assignments)))
feats$p1 <- as.integer(cluster_assignments)
feats$p2 <- as.integer(partition_finale)

# ------------------------------------------------------------
# 2) Profils moyens par cluster
# ------------------------------------------------------------
prof <- function(part){
  ag <- aggregate(feats[, c("mean_dist","rim","short_mid","long_mid","three")],
                  by = list(cluster = feats[[part]]), FUN = mean)
  ag$n_players <- as.vector(table(feats[[part]]))
  ag
}
cat("=== PROFILS PARTITION 1 (cluster_assignments) ===\n")
print(round(prof("p1"), 3))
cat("\n=== PROFILS PARTITION 2 (partition_finale) ===\n")
print(round(prof("p2"), 3))

# ------------------------------------------------------------
# 3) Silhouette et inertie intra sur features standardisées
#    (implémentation manuelle, sans package externe)
# ------------------------------------------------------------
X <- scale(feats[, c("rim","short_mid","long_mid","three","mean_dist")])
D <- dist(X)

sil <- function(cl, D){
  Dm <- as.matrix(D); n <- nrow(Dm); s <- numeric(n)
  for (i in 1:n){
    a <- mean(Dm[i, cl == cl[i] & seq_len(n) != i])                 # cohésion
    b <- min(sapply(setdiff(unique(cl), cl[i]),                     # séparation
                    function(k) mean(Dm[i, cl == k])))
    s[i] <- (b - a) / max(a, b)
  }
  s
}
s1 <- sil(feats$p1, D)
s2 <- sil(feats$p2, D)

cat("\nSilhouette moyenne  P1:", round(mean(s1), 3),
    " | P2:", round(mean(s2), 3), "\n")
cat("Silhouette par cluster P1:\n"); print(round(tapply(s1, feats$p1, mean), 3))
cat("Silhouette par cluster P2:\n"); print(round(tapply(s2, feats$p2, mean), 3))

# Inertie intra-cluster (somme des carrés aux centroïdes)
wss <- function(cl) sum(sapply(unique(cl), function(k){
  Xi <- X[cl == k, , drop = FALSE]
  sum(scale(Xi, scale = FALSE)^2)
}))
cat("\nInertie intra P1:", round(wss(feats$p1), 1),
    " | P2:", round(wss(feats$p2), 1),
    " | Totale:", round(sum(scale(X, scale = FALSE)^2), 1), "\n")

cat("\nJoueurs à silhouette négative P1:\n"); print(feats$player[s1 < 0])
cat("Joueurs à silhouette négative P2:\n");  print(feats$player[s2 < 0])

# ------------------------------------------------------------
# 4) Indices de concordance entre les deux partitions
#    (Rand Index et Adjusted Rand Index)
# ------------------------------------------------------------
n   <- nrow(feats)
tab <- table(feats$p1, feats$p2)
c2  <- function(x) sum(choose(x, 2))
a        <- c2(tab)
expected <- c2(rowSums(tab)) * c2(colSums(tab)) / choose(n, 2)
maxidx   <- 0.5 * (c2(rowSums(tab)) + c2(colSums(tab)))
ari      <- (a - expected) / (maxidx - expected)

pairs   <- t(combn(n, 2))
same_p1 <- feats$p1[pairs[,1]] == feats$p1[pairs[,2]]
same_p2 <- feats$p2[pairs[,1]] == feats$p2[pairs[,2]]
cat("\nRand Index:", round(mean(same_p1 == same_p2), 3),
    " | Adjusted Rand Index:", round(ari, 3), "\n")

# ------------------------------------------------------------
# 5) Figure : les deux partitions dans le plan rim% x 3pts%
# ------------------------------------------------------------
cols <- c("#E4572E", "#2E86AB", "#7B2D8B", "#3E8E41")

png("comparaison_clusterings.png", width = 2200, height = 1200, res = 160)
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
for (p in c("p1", "p2")){
  cl <- feats[[p]]
  plot(feats$rim * 100, feats$three * 100, col = cols[cl], pch = 19, cex = 1.4,
       xlab = "% tirs près du cercle (< 8 ft)",
       ylab = "% tirs à 3 points",
       main = ifelse(p == "p1",
                     "Partition 1 (cluster_assignments)\nsilhouette moy. = 0.07",
                     "Partition 2 (partition_finale)\nsilhouette moy. = 0.22"),
       xlim = c(0, 100), ylim = c(0, 75))

  # Étiquettes de quelques joueurs repères
  lab <- c("Stephen Curry", "James Harden", "Giannis Antetokounmpo",
           "Kyle Korver", "LeBron James", "Kevin Durant",
           "DeMar DeRozan", "Jarrett Allen", "Anthony Davis", "Trae Young")
  idx <- match(lab, feats$player)
  text(feats$rim[idx] * 100, feats$three[idx] * 100,
       labels = sapply(strsplit(lab, " "), tail, 1),
       pos = 3, cex = 0.75, col = "grey25")

  legend("topright", legend = paste("Cluster", 1:4),
         col = cols, pch = 19, bty = "n", cex = 0.9)
}
dev.off()
