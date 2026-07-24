# ==============================================================================
# DDClust adapté aux matchs NBA — structure fidèle à l'original

#deltametric n°10
# ==============================================================================

library(curveDepth)
library(spatstat)
library(snowFT)
library(parallel)

# --- données -----------------------------------------------------------------
matchs_joueur <- nba_data_depth_made(data_by_player[[1]])
points        <- matchs_joueur
n             <- length(points)

# --- paramètres --------------------------------------------------------------
nDirs     <- 100
lambda    <- 0.6
nClusters <- 2
M         <- 5
L         <- 5
Tval      <- 0

# ==============================================================================
# WORKERS
# ==============================================================================

#' Worker pour le calcul partiel des distances
worker.dist <- function(inp, ...) {
  library(spatstat)
  args            <- list(...)[[1]]
  curves          <- args$curves
  fenetre_terrain <- args$fenetre_terrain
  
  match_i <- curves[[inp$iRows]]
  match_j <- curves[[inp$iCols]]
  
  coords_i <- match_i$coords
  coords_j <- match_j$coords
  
  ppp_i <- ppp(x = coords_i[, 1], y = coords_i[, 2], window = fenetre_terrain)
  ppp_j <- ppp(x = coords_j[, 1], y = coords_j[, 2], window = fenetre_terrain)
  
  # Calcul des densités de kernel (KDE)
  kde_i <- density(ppp_i, sigma = 3, at = "pixels")
  kde_j <- density(ppp_j, sigma = 3, at = "pixels", dimyx = dim(kde_i)) 
  
  # Calcul et retour de la distance L2 (Intégration de la différence au carré)
  #return(integral((kde_i - kde_j)^2))
  
  deltametric(ppp_i, ppp_j, p = 1, c = 5)
}

#' Worker pour le calcul partiel des profondeurs
worker.depth <- function(inp, ...) {
  library(curveDepth)
  args      <- list(...)[[1]]
  points    <- args$points
  cluster_j <- args$cluster_j
  nDirs     <- args$nDirs
  
  depths.Tukey(list(inp), cluster_j,
               nDirs     = nDirs,
               subs      = FALSE,
               exactEst  = FALSE,
               parConst1 = -2,
               parConst2 = 5)
}

# ==============================================================================
# CALCUL PARALLÈLE DES DISTANCES
# ==============================================================================

dist.matchs.parallel <- function(curves, fenetre_terrain,
                                  nproc = 2, cltype = "SOCK") {
  
  parInput <- list(curves = curves, fenetre_terrain = fenetre_terrain)
  
  inputs <- list()
  for (i in seq_len(length(curves))) {
    for (j in seq_len(length(curves))) {
      inputs[[(i - 1) * length(curves) + j]] <- list(iRows = i, iCols = j)
    }
  }
  
  cat("Starting calculating distances (in parallel) ... ")
  res <- performParallel(nproc, inputs, cltype = cltype,
                         worker.dist, ... = parInput)
  cat("done.\n")
  
  dists <- matrix(0, nrow = length(curves), ncol = length(curves))
  for (i in seq_len(length(res))) {
    dists[inputs[[i]]$iRows, inputs[[i]]$iCols] <- res[[i]]
  }
  
  dists
}

# ==============================================================================
# CALCUL PARALLÈLE DES PROFONDEURS
# ==============================================================================

depths.Tukey.parallel <- function(points, cluster_j, nDirs,
                                   nproc = 2, cltype = "SOCK") {
  
  parInput <- list(points = points, cluster_j = cluster_j, nDirs = nDirs)
  
  cat("Computing depths in parallel ... ")
  res <- performParallel(nproc, points, cltype = cltype,
                         worker.depth, ... = parInput)
  cat("done.\n")
  
  unlist(res)
}

# ==============================================================================
# FONCTION calcReDs
# ==============================================================================

calcReDs <- function(points, labels, nDirs, lambda = 0.6,
                     nproc = 2, cltype = "SOCK") {
  
  n            <- length(labels)
  labelsUnique <- unique(labels)
  nClusters    <- length(labelsUnique)
  
  # --- profondeurs -----------------------------------------------------------
  depthsAll <- matrix(NA, nrow = n, ncol = nClusters)
  for (j in seq_len(nClusters)) {
    cluster_j      <- points[labels == labelsUnique[j]]  # points, pas curves
    depthsAll[, j] <- depths.Tukey.parallel(points, cluster_j, nDirs,
                                             nproc = nproc, cltype = cltype)
  }
  
  # --- distances moyennes ----------------------------------------------------
  distsAll <- matrix(NA, nrow = n, ncol = nClusters)
  for (i in seq_len(n)) {
    for (j in seq_len(nClusters)) {
      idx_j          <- which(labels == labelsUnique[j])
      distsAll[i, j] <- mean(dists[i, idx_j])
    }
  }
  
  # --- ReDs, silhouettes, cluster concurrent ---------------------------------
  pureReDs     <- numeric(n)
  pureSils     <- numeric(n)
  ReDs         <- numeric(n)
  nearestClust <- numeric(n)
  
  for (i in seq_len(n)) {
    
    labelOwn  <- labels[i]
    j_own     <- which(labelsUnique == labelOwn)
    j_foreign <- which(labelsUnique != labelOwn)
    
    depthOwn      <- depthsAll[i, j_own]
    idx_own       <- setdiff(which(labels == labelOwn), i)
    silhdOwn      <- mean(dists[i, idx_own])
    depthsForeign <- depthsAll[i, j_foreign]
    silhdsForeign <- distsAll[i, j_foreign]
    
    pureSils[i] <- (min(silhdsForeign) - silhdOwn) /
                    max(silhdOwn, min(silhdsForeign))
    pureReDs[i] <- depthOwn - max(depthsForeign)
    ReDs[i]     <- (1 - lambda) * pureSils[i] + lambda * pureReDs[i]
    
    nearestClust[i] <- labelsUnique[j_foreign[which.max(
      (1 - lambda) * (-silhdsForeign) + lambda * depthsForeign
    )]]
  }
  
  list(ReDs         = ReDs,
       nearestClust = nearestClust,
       pureReDs     = pureReDs,
       pureSils     = pureSils,
       depths       = depthsAll)
}
# ==============================================================================
# CALCUL DE LA MATRICE DE DISTANCES
# ==============================================================================

fenetre_terrain <- owin(c(-25, 25), c(-47.1, 0))
dists <- dist.matchs.parallel(points, fenetre_terrain,
                               nproc = 20, cltype = "SOCK")

# ==============================================================================
# PARTITION INITIALE
# ==============================================================================

cah <- hclust(as.dist(dists), method = "ward.D2")
# plot(cah, main = "Dendrogramme des Matchs du Core", xlab = "Matchs", sub = "")
cluster_assignments <- cutree(cah, k = nClusters)
names(cluster_assignments) <- names(matchs_joueur) 
labels <- cluster_assignments

#labels <- partition_ddclust
cat("Partition initiale :\n")
print(table(labels))


# ==============================================================================
# BOUCLE PRINCIPALE DDClust
# ==============================================================================

iter <- 1
withoutMoves <- 0
movesThisStep <- FALSE
temp <- -1
Tval <- 0

while (TRUE) {

  res <- calcReDs(points, labels, nDirs, lambda,
                  nproc = 20, cltype = "SOCK")

  ReDs <- res$ReDs
  nearestClust <- res$nearestClust
  crit <- mean(ReDs)

  cat("\n")
  cat("Labels:\n")
  cat(labels, "\n")
  cat("\n")
  cat("Pure ReDs:\n")
  cat(res$pureReDs, "\n")
  cat("Pure sils:\n")
  cat(res$pureSils, "\n")
  cat("Pure ReDs - pure sils:\n")
  cat(res$pureReDs - res$pureSils, "\n")

  indicesWrong <- which(ReDs <= Tval)

  cat("Wrongly classified:", length(indicesWrong), "\n")
  cat(indicesWrong, "\n")
  cat(labels[indicesWrong], "\n")

  movesThisStep <- FALSE
  newCrit <- crit

  while (length(indicesWrong) > 0) {

    nCurIndices <- sample(1:M, 1)

    if (nCurIndices > length(indicesWrong)) {
      curIndices <- indicesWrong
    } else {
      curIndices <- sample(indicesWrong,
                           nCurIndices,
                           replace = TRUE)
    }

    indicesWrong <- indicesWrong[-which(indicesWrong %in% curIndices)]

    newLabels <- labels
    newLabels[curIndices] <- nearestClust[curIndices]

    if (length(table(newLabels)) < nClusters ||
        min(table(newLabels)) < 2) {
      cat("Partial: size =", nCurIndices,
          ", refused due to emptying a cluster\n")
      next
    }

    newRes <- calcReDs(points,
                       newLabels,
                       nDirs,
                       lambda,
                       nproc = 20,
                       cltype = "SOCK")

    newCrit <- mean(newRes$ReDs)

    if (newCrit > crit) {

      movesThisStep <- TRUE
      crit <- newCrit
      labels <- newLabels
      nearestClust <- newRes$nearestClust

      cat("Partial: size =", nCurIndices,
          ", C =", newCrit, "\n")

    } else {

      p <- exp(temp * (crit - newCrit)) / 2
      cat("p =", p, "\n")

      if (rbinom(1, 1, p) > 0.5) {

        movesThisStep <- TRUE
        crit <- newCrit
        labels <- newLabels
        nearestClust <- newRes$nearestClust

        cat("Partial: size =", nCurIndices,
            ", C =", newCrit, "\n")

      } else {

        cat("Partial: size =", nCurIndices,
            ", refused\n")

      }
    }
  }

  if (movesThisStep) {
    withoutMoves <- 0
  } else {
    withoutMoves <- withoutMoves + 1
  }

  cat("Iteration", iter, ": C =", crit, "\n")
  cat("Clusters are:\n")
  print(table(labels))
  cat("Wrongly classified:",
      length(labels[res$ReDs <= Tval]), "\n")
  cat("\n")

  iter <- iter + 1

  if (withoutMoves >= L) {
    cat("Convergence after", iter, "iterations.\n")
    break
  } else {
    temp <- temp * 2
  }
}
# ==============================================================================
# PARTITION FINALE
# ==============================================================================

cat("\nPartition finale :\n")
print(table(labels))
partition_finale <- labels











