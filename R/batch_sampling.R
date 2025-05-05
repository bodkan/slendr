batch_sampling <- function(model, pairs) {
  twos <- vapply(pairs, function(x) length(x) == 2, FUN.VALUE = logical(1))
  pops <- lapply(pairs, function(x) x[[1]], FUN.VALUE = logical(1))
  times <- lapply(pairs, function(x) x[[2]], FUN.VALUE = logical(1))
}