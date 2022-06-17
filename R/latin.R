#' Latin square designs and its generalisations as an array
#' @param n,nt The number of treatments
#' @param nc The number of columns
#' @param nr The number of rows
#' @param dim A vector of integers to indicate the number of elements in each dimension.
#' @param randomise A logical value to indicate whether the treatment allocation should be randomised. The default value is `TRUE`.
#' @name latin
#' @examples
#' latin_square(n = 3)
#' latin_rectangle(3, 3, 3)
#' latin_array(3, c(3, 3, 3))
NULL

#' @describeIn latin Latin square design
#' @export
latin_square <- function(n, randomise = TRUE) {
  out <- matrix(nrow = n, ncol = n)
  out[1,] <- if(randomise) sample(1:n) else 1:n
  for(i in 2:n) {
    out[i,] <- lag_vector(out[i - 1, ])
  }
  if(randomise) {
    out <- out[, sample(1:n)]
    out <- out[sample(1:n), ]
  }
  out
}

#' @describeIn latin Like a Latin square design but allow different number of rows and columns
#' @export
latin_rectangle <- function(nr, nc, nt, randomise = TRUE) {
  r1 <- ceiling(nr/nt)
  r2 <- ceiling(nc/nt)
  out <- do.call("rbind", lapply(1:r1, function(i) {
    do.call("cbind",
      lapply(1:r2, function(j) {
        latin_square(nt, randomise = randomise)
      }
    ))
  }))

  out <- out[1:nr, 1:nc]

  res1 <- apply(out, 1, function(x) table(factor(x, 1:nt)))
  res2 <- apply(out, 2, function(x) table(factor(x, 1:nt)))
  diff1 <- apply(res1, 2, function(x) max(x) - min(x))
  diff2 <- apply(res2, 2, function(x) max(x) - min(x))

  if(randomise && (any(diff1 > 1) | any(diff2 > 1))) return(latin_rectangle(nr, nc, nt, randomise))

  out
}

#' @describeIn latin Returns an array where it stitches up multiple Latin square/rectangle design
#' @export
latin_array <- function(dim, nt, randomise = TRUE) {
  ndim <- length(dim)
  if(ndim == 2) return(latin_rectangle(dim[1], dim[2], nt, randomise))
  r <- ceiling(dim[ndim]/nt)
  dim_ext <- c(dim[-ndim], r * nt)
  out <- array(dim = dim_ext)
  out <- assign_array(out, 1, 1, latin_array(dim_ext[-1], nt, randomise))
  for(i in 2:dim[1]) {
    out <- assign_array(out, 1, i, lag_array(index_array(out, 1, i - 1), 1))
  }
  if(randomise) {
    for(i in seq_along(dim[-ndim])) {
      out <- sample_array(out, i)
    }
  }
  out
}

lag_vector <- function(x) {
  x[c(2:length(x), 1L)]
}

lag_array <- function(array, i) {
  da <- dim(array)
  index <- lapply(seq(length(da)), function(x) bquote())
  index[[i]] <- c(2:da[i], 1L)
  do.call("[", c(list(array), index))
}

assign_array <- function(array, m, i, value) {
  da <- dim(array)
  index <- lapply(seq(length(da)), function(x) ifelse(x %in% m, i, bquote()))
  do.call("[<-", c(list(array), index, list(value)))
}

sample_array <- function(array, i) {
  da <- dim(array)
  index <- lapply(seq(length(da)), function(x) if(x==i) sample(1:da[i]) else bquote())
  do.call("[", c(list(array), index))
}

index_array <- function(array, m, i) {
  da <- dim(array)
  index <- lapply(seq(length(da)), function(x) if(x %in% m) i else bquote())
  do.call("[", c(list(array), index))
}

