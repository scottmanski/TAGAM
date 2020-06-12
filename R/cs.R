#' Cosine Similarity
#'
#' This function finds the cosine similarity between two vectors of words.
#'
#' @param a,b characters or character vectors containing words in \code{word_embeddings}.
#' @param word_embeddings named list of word embeddings.  See \code{\link{formatWordEmbeddings}}.
#'
#' @details Consider 2 words with word embedding representations \eqn{a} and \eqn{b}.  Then the cosine similarity
#' is defined as \deqn{sim_cos(a,b)=\frac{a \cdot b}{|| a ||_2 \cdot || b ||_2}}{sim_cos(a,b)=(a \cdot b)/(|| a ||_2 \cdot || b ||_2)}
#'
#' If \eqn{A = (a_1,...,a_n)} and \eqn{B = (b_1,...,b_m)}, then the result
#' is a matrix of \eqn{m \times n} dimension with each entry in cell (i, j) defined as \eqn{sim_cos(a_j, b_i)}.
#'
#' @return a matrix of cosine similarities
#'
#' @seealso \code{\link{formatWordEmbeddings}}
#'
#' @references Goldberg, Y. (2017) \emph{Neural Network Methods for Natural Language Processing.} San Rafael, CA: Morgan & Claypool Publishers.
#'
#' @examples
#' \dontrun{
#'
#' a <- "home"
#' b <- "house"
#' cs(a, b, word_embeddings)
#'
#' a <- c("home", "apartment", "mansion")
#' b <- c("my", "dog", "sleeps", "in", "her", "dog", "house")
#' cs(a, b, word_embeddings)
#' }
#'
#' @export


cs <- function(a, b, word_embeddings) {
  if (mean(c(a, b) %in% names(word_embeddings)) != 1) {
    stop(c(a, b)[which(!c(a, b) %in% names(word_embeddings))], " not in word_embeddings")
  }
  t(do.call(cbind, word_embeddings[b])) %*% do.call(cbind, word_embeddings[a])
}






