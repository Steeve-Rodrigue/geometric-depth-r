  # ==============================================================================
  # DDClust adapté : unité d'observation = JOUEUR (tirs poolés sur matchs typiques)
  # ==============================================================================

  library(curveDepth)
  library(spatstat)
  library(snowFT)
  library(parallel)

  fenetre_terrain <- owin(c(-25, 25), c(-47.1, 0))

  # ==============================================================================
  # ÉTAPE PRÉALABLE (FIXE) : filtrage des matchs typiques + construction des points
  # ==============================================================================

  # --- 1. Point patterns de tirs marqués, par joueur/match ---
  data_made_by_player <- lapply(data_by_player, function(joueur) {
    nba_data_depth_made(joueur)
  })
  names(data_made_by_player) <- names(data_by_player)

  # --- 2. Ne garder que les matchs les plus typiques (profondeur de Tukey) ---
  data_typic_made_all_players <- lapply(data_made_by_player, function(joueur_data) {
    prof_matchs <- depths.Tukey(
      joueur_data,
      joueur_data,
      nDirs     = 250,
      subs      = FALSE,
      exactEst  = FALSE,
      parConst1 = -2,
      parConst2 = 5
    )
    seuil_matchs <- quantile(prof_matchs, probs = 0.4, na.rm = TRUE)
    joueur_data[prof_matchs >= seuil_matchs]
  })
  names(data_typic_made_all_players) <- names(data_made_by_player)

  # --- 3. Un SEUL ppp par joueur : pooling de tous les tirs des matchs typiques ---
  points <- lapply(data_typic_made_all_players, function(joueur_list) {
    all_coords <- do.call(rbind, lapply(joueur_list, function(match) match$coords))
    ppp(x = all_coords[, 1], y = all_coords[, 2], window = fenetre_terrain)
  })
  names(points) <- names(data_typic_made_all_players)

  n <- length(points)   # nombre de JOUEURS (plus nombre de matchs)

  # --- paramètres ----------------------------------------------------------------
  nDirs     <- 250
  lambda    <- 0.6
  nClusters <- 3
  M         <- 6
  L         <- 5
  Tval      <- 0

  # ==============================================================================
  # WORKERS
  # ==============================================================================

  #' Worker pour le calcul partiel des distances (entre JOUEURS, déjà ppp)
  worker.dist <- function(inp, ...) {
    library(spatstat)
    args   <- list(...)[[1]]
    points <- args$points   # liste de ppp
    sigma  <- args$sigma
    
    ppp_i <- points[[inp$iRows]]
    ppp_j <- points[[inp$iCols]]
    
    kde_i <- density(ppp_i, sigma = sigma, at = "pixels")
    kde_j <- density(ppp_j, sigma = sigma, at = "pixels", dimyx = dim(kde_i))
    
    kde_i_norm <- kde_i / integral(kde_i)
    kde_j_norm <- kde_j / integral(kde_j)
    
    integral((kde_i_norm - kde_j_norm)^2)
  }

  #' Worker pour le calcul partiel des profondeurs
  worker.depth <- function(inp, ...) {
    library(curveDepth)
    args      <- list(...)[[1]]
    cluster_j <- args$cluster_j
    nDirs     <- args$nDirs
    
    # inp est un objet ppp -> on le convertit au format attendu par depths.Tukey
    inp_coords <- list(coords = cbind(inp$x, inp$y))
    
    # cluster_j est une liste de ppp -> conversion de chaque élément également
    cluster_j_coords <- lapply(cluster_j, function(p) list(coords = cbind(p$x, p$y)))
    
    depths.Tukey(list(inp_coords), cluster_j_coords,
                nDirs     = nDirs,
                subs      = FALSE,
                exactEst  = FALSE,
                parConst1 = -2,
                parConst2 = 5)
  }

  # ==============================================================================
  # CALCUL PARALLÈLE DES DISTANCES (entre joueurs)
  # ==============================================================================

  dist.matchs.parallel <- function(points, sigma,
                                    nproc = 2, cltype = "SOCK") {
    
    parInput <- list(points = points, sigma = sigma)
    
    inputs <- list()
    for (i in seq_len(length(points))) {
      for (j in seq_len(length(points))) {
        inputs[[(i - 1) * length(points) + j]] <- list(iRows = i, iCols = j)
      }
    }
    
    cat("Starting calculating distances (in parallel) ... ")
    res <- performParallel(nproc, inputs, cltype = cltype,
                          worker.dist, ... = parInput)
    cat("done.\n")
    
    dists <- matrix(0, nrow = length(points), ncol = length(points))
    for (i in seq_len(length(res))) {
      dists[inputs[[i]]$iRows, inputs[[i]]$iCols] <- res[[i]]
    }
    
    dists
  }

  # ==============================================================================
  # CALCUL PARALLÈLE DES PROFONDEURS (des joueurs par rapport aux clusters)
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
  # FONCTION calcReDs (inchangée)
  # ==============================================================================

  calcReDs <- function(points, labels, nDirs, lambda = 0.6,
                      nproc = 2, cltype = "SOCK") {
    
    n            <- length(labels)
    labelsUnique <- unique(labels)
    nClusters    <- length(labelsUnique)
    
    depthsAll <- matrix(NA, nrow = n, ncol = nClusters)
    for (j in seq_len(nClusters)) {
      cluster_j      <- points[labels == labelsUnique[j]]
      depthsAll[, j] <- depths.Tukey.parallel(points, cluster_j, nDirs,
                                              nproc = nproc, cltype = cltype)
    }
    
    distsAll <- matrix(NA, nrow = n, ncol = nClusters)
    for (i in seq_len(n)) {
      for (j in seq_len(nClusters)) {
        idx_j          <- which(labels == labelsUnique[j])
        distsAll[i, j] <- mean(dists[i, idx_j])
      }
    }
    
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
  # CALCUL DE LA MATRICE DE DISTANCES (entre joueurs)
  # ==============================================================================

  sigma_kde <- 2
  #set.seed(42)
  dists <- dist.matchs.parallel(points, sigma_kde,
                                nproc = 20, cltype = "SOCK")

# ==============================================================================
# PARTITION INITIALE
# ==============================================================================
# set.seed(42)

# cah <- hclust(as.dist(D_joueur), method = "ward.D2")
# cluster_assignments <- cutree(cah, k = nClusters)
# names(cluster_assignments) <- names(points)   # noms des JOUEURS
# labels <- cluster_assignments

#set.seed(42)
poids <- runif(nClusters)
labels <- sample(x = 1:nClusters, size = length(points), replace = TRUE, prob = poids)
names(labels) <- names(points)

cat("Partition initiale :\n")
print(table(labels))

#labels <- partition_finale

# ==============================================================================
# BOUCLE PRINCIPALE DDClust (inchangée)
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
      coef <- ifelse(iter%%10==0, 2, 1)
      temp <- temp * coef
    }
  }

  # ==============================================================================
  # PARTITION FINALE
  # ==============================================================================

  cat("\nPartition finale :\n")
  print(table(labels))
  partition_finale <- labels
  print(partition_finale)   # noms des joueurs + cluster assigné
