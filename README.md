
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TAGAM

<!-- badges: start -->

<!-- badges: end -->

The goal of TAGAM is to streamline the use of word embeddings in text
analysis. The package is designed to take raw textual descriptions and
transform the information for use in your analysis. The package also
includes functions for running the generalized additive model framework
described in XXX.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("scottmanski/TAGAM")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(TAGAM)
temp <- tempfile()
download.file("http://nlp.stanford.edu/data/wordvecs/glove.6B.zip", temp)

embedding_matrix <- read.table(unz(temp, "glove.6B.300d.txt"), quote = "",
                               comment.char = "", stringsAsFactors = FALSE)
word_embeddings <- formatWordEmbeddings(embedding_matrix, normalize = TRUE)
```

``` r
library(TAGAM)
a <- "home"
b <- "house"
cs(a, b, word_embeddings_subset)
#>            home
#> house 0.5005335
```

``` r
library(tidyverse)
#> ── Attaching packages ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──
#> ✓ ggplot2 3.3.1     ✓ purrr   0.3.3
#> ✓ tibble  3.0.1     ✓ dplyr   0.8.3
#> ✓ tidyr   1.0.0     ✓ stringr 1.4.0
#> ✓ readr   1.3.1     ✓ forcats 0.4.0
#> ── Conflicts ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(tidytext)
D <- c("Statistics is great!",
       "My dog is fluffy",
       "What is your favorite class?")
x <- tibble(line = 1:length(D), text = D) %>%
  unnest_tokens(word, text)

w <- c("statistics", "dog", "fluffy", "favorite", "class")

cs.matrix(x, words = w, word_embeddings_subset)
#> 3 x 5 Matrix of class "dgeMatrix"
#>      statistics       dog     fluffy  favorite     class
#> [1,]  1.0000000 0.2670640 0.01818783 0.3341204 0.2700382
#> [2,]  0.1796945 1.0000000 1.00000000 0.4486072 0.2700382
#> [3,]  0.2170957 0.3788118 0.13183853 1.0000000 1.0000000
```
