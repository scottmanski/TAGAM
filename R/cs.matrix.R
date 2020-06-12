#' Cosine Similarity Matrix
#'
#' Creates the design matrix of cosine similarities from textual observations and a
#' vector of words.
#'
#' @param x a tibble containing 2 columns; line and word.  The 'line' column contains the observation number
#' that the word from the 'word' column appears in.  See 'Examples'.
#' @param words a character vector of words that will represent the columns of the resulting matrix.
#' @param word_embeddings named list of word embeddings.  See \code{\link{formatWordEmbeddings}}.
#' @param method function to apply across each column.  Options include \code{c("max", "sum", "mean")}.
#' @param parallel logical, indicating if the matrix should be calculated in parallel.
#' @param n.cluster integer, the number of clusters to use if \code{parallel=TRUE}.
#' @param sparse logical, indicating if a sparse matrix should be returned.
#'
#' @details A function to create a design matrix of cosine similarities from textual observations
#' and a vector of words.  The resulting matrix will be of dimension \eqn{unique(x$line) \times length(words)}.
#'
#' Consider 2 words with word embedding representations \eqn{a} and \eqn{b}.  Then the cosine similarity
#' is defined as \deqn{sim_cos(a,b)=\frac{a \cdot b}{|| a ||_2 \cdot || b ||_2}}{sim_cos(a,b)=(a \cdot b)/(|| a ||_2 \cdot || b ||_2)}.
#'
#' If \code{method = "max"}, for a given line with \eqn{m} words, each row of the returned matrix is defined as \eqn{max_{i=1,...,m}(sim_cos(a_j, b_i))}.
#' \code{method = "sum"} or \code{method = "mean"} are defined
#' in a similar fashion.
#'
#' @return a (sparse) matrix of cosine similarities
#'
#' @seealso \code{\link{cs}}, \code{\link{formatWordEmbeddings}}
#'
#' @references Goldberg, Y. (2017) \emph{Neural Network Methods for Natural Language Processing.} San Rafael, CA: Morgan & Claypool Publishers.
#'
#' @importFrom foreach `%dopar%`
#'
#' @examples
#' \dontrun{
#' require(dplyr)
#' require(tidytext)
#'
#'
#' sentences <- data.frame("Description" = c("Statistics is great!",
#'                                           "My dog is fluffy.",
#'                                           "What is your favorite class?"),
#'                         stringsAsFactors = FALSE)
#' x <- tibble(line = 1:nrow(sentences), text = sentences$Description) %>%
#'   unnest_tokens(word, text)
#'
#' cs.matrix(x, words = c("stats", "cat"), word_embeddings)
#' }
#'
#' @export

cs.matrix <- function(x, words, word_embeddings, method = "max", parallel = FALSE, n.cluster = NULL, sparse = FALSE) {
  if (missing(word_embeddings)) {
    stop("No word_embeddings found. Be sure to run download.GloVe")
  }
  i <- NULL
  lines <- unique(x$line)

  if (parallel == TRUE & is.null(n.cluster)) {
    parallel <- FALSE
    warning("n.cluster not specified so parallel set to FALSE")
  }

  all_words <- unique(c(x$word, words))

  if (mean(all_words %in% names(word_embeddings)) != 1) {
    warning(all_words[which(!all_words %in% names(word_embeddings))], " not in word_embeddings and was removed from all_words")
    all_words <- all_words[which(all_words %in% names(word_embeddings))]
  }

  CS <- TAGAM::cs(words, unique(x$word), word_embeddings[all_words])

  if (parallel) {
    cl <- parallel::makeCluster(n.cluster)
    doParallel::registerDoParallel(cl)
    mat <- foreach::foreach(i = 1:length(lines)) %dopar% {
      apply(CS[unique(x$word) %in% x$word[x$line == lines[i]], , drop = FALSE], 2, method)
    }
    parallel::stopCluster(cl)
  } else {
    mat <- lapply(1:length(lines), function(i) {
      apply(CS[unique(x$word) %in% x$word[x$line == lines[i]], , drop = FALSE], 2, method)
    })
  }

  CS.Mat <- do.call(rbind, mat)
  colnames(CS.Mat) <- words

  return(Matrix::Matrix(CS.Mat, sparse = sparse))
}






