
cv.gglasso2 <- function (x, y, group, lambda = NULL,
                         pred.loss = c("misclass", "loss", "L1", "L2"),
                         nfolds = 5, foldid, delta, nlambda, ...) {
  if (missing(pred.loss))
    pred.loss <- "default"
  else pred.loss <- match.arg(pred.loss)
  N <- nrow(x)
  p <- length(unique(group))
  y <- drop(y)
  if (missing(delta))
    delta <- 1
  if (delta < 0)
    stop("delta must be non-negtive")
  gglasso.object <- gglasso(x, y, group, lambda = lambda,
                            delta = delta, ...)
  lambda <- gglasso.object$lambda
  survived <- vector()
  for (j in 1:length(lambda)) {
    survived[j] <- sum(unlist(lapply(1:p, function(i) {sum(group == i)*(sum(gglasso.object$beta[group == i, j] == 0) == 0)})))
  }
  smallest_lambda <- which(survived > N)
  if (length(smallest_lambda) != 0) {
    lambda <- exp(seq(log(lambda[1]), log(lambda[min(smallest_lambda)]), length.out = nlambda))
    message(paste("lambda.factor changed to", min(lambda)/max(lambda)))
  }
  if (missing(foldid))
    foldid <- sample(rep(seq(nfolds), length = N))
  else nfolds <- max(foldid)
  if (nfolds < 3)
    stop("nfolds must be bigger than 3; nfolds=10 recommended")
  outlist <- as.list(seq(nfolds))
  for (i in seq(nfolds)) {
    which <- foldid == i
    y_sub <- y[!which]
    outlist[[i]] <- gglasso(x = x[!which, , drop = FALSE],
                            y = y_sub, group = group, lambda = lambda, delta = delta,
                            ...)
  }
  fun <- paste("cv", class(gglasso.object)[[2]], sep = ".")
  cvstuff <- do.call(fun, list(outlist, lambda, x, y, foldid,
                               pred.loss, delta))
  cvm <- cvstuff$cvm
  cvsd <- cvstuff$cvsd
  cvname <- cvstuff$name
  out <- list(lambda = lambda, cvm = cvm, cvsd = cvsd, cvupper = cvm +
                cvsd, cvlo = cvm - cvsd, name = cvname, gglasso.fit = gglasso.object)
  lamin <- getmin(lambda, cvm, cvsd)
  obj <- c(out, as.list(lamin))
  class(obj) <- "cv.gglasso"
  obj
}

getmin <- function(lambda, cvm, cvsd) {
  cvmin <- min(cvm)
  idmin <- cvm <= cvmin
  lambda.min <- max(lambda[idmin])
  idmin <- match(lambda.min, lambda)
  semin <- (cvm + cvsd)[idmin]
  idmin <- cvm <= semin
  lambda.1se <- max(lambda[idmin])
  list(lambda.min = lambda.min, lambda.1se = lambda.1se)
}

normalize_embeddings <- function(embedding_mat) {
  normalizing_factor <- apply(embedding_mat, 1, function(x) {
    1/sqrt(sum(x^2))
  })
  embedding_mat <- embedding_mat * normalizing_factor
}

embeddings_to_list <- function(embedding_mat, embedding_words) {
  word_embeddings <- apply(embedding_mat, 1, function(x) list(as.numeric(x)))
  word_embeddings <- lapply(word_embeddings, unlist)
  names(word_embeddings) <- embedding_words
  return(word_embeddings)
}
