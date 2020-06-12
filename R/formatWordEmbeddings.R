#' Format Word Embeddings
#'
#' This function formats the word embeddings.
#'
#' @param embedding_matrix word embedding matrix. For a matrix containing information on \eqn{n} words, with
#' each word being represented by a \eqn{d} dimensional vector, \code{embedding_matrix} should have
#' \eqn{n} rows and \eqn{d+1} columns where the first column contains the words.
#' @param normalize logical; should the word embeddings be normalized.
#'
#' @details This function downloads GloVe (https://nlp.stanford.edu/projects/glove/)
#' and formats the word embeddings.  The result is a named list of word embeddings.  Each
#' entry in the list is a numeric vector of length \code{dimension} representing the word
#' embedding for that entry's name (see examples).
#'
#' @return A named list of word embeddings.
#'
#' @import utils
#'
#' @references Jeffrey Pennington, Richard Socher, and Christopher D. Manning. 2014. GloVe: Global Vectors for Word Representation. \url{https://nlp.stanford.edu/projects/glove/}.
#'
#' @examples
#' \dontrun{
#' download.file("http://nlp.stanford.edu/data/wordvecs/glove.6B.zip", "glove.6B.zip")
#' unzip("glove.6B.zip")
#' embedding_matrix <- read_table2("glove.6B.300d.txt", col_names = FALSE)
#' word_embeddings <- formatWordEmbeddings(embedding_matrix, normalize = TRUE)
#'
#' # Extract the word embedding for "the"
#' word_embeddings[["the"]]
#' }
#'
#' @export

formatWordEmbeddings <- function(embedding_matrix, normalize = TRUE) {
  embedding_words <- embedding_matrix[, 1, drop = TRUE]
  embedding_mat <- embedding_matrix[, -1]
  if (normalize) {
    message("Normalizing word embeddings.")
    embedding_mat <- normalize_embeddings(embedding_mat)
  }
  message("Formatting word embeddings.")
  word_embeddings <- embeddings_to_list(embedding_mat, embedding_words)
  return(word_embeddings)
}


