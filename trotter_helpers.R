# Returns the kth permutation of elements
perm.worker <- function(k, elements) {
  n <- length(elements)
  if (n == 1) elements
  else {
    group <- as.integer((k - 1) / n)
    item <- (k - 1) %% n
    position <- if (group %% 2 == 0) n - item - 1 else item
    append(
      perm.worker(
        group + 1,
        elements[1:(n - 1)]
      ),
      elements[n],
      after = position
    )
  }
}

# Returns the kth r-combination of elements 
combination <- function(k, r, elements) {
  n <- length(elements)
  position <- 0
  d <- choose(n - position - 1, r - 1)
  
  while ((k - 1) >= d) {
    k <- k - d
    position <- position + 1
    d <- choose(n - position - 1, r - 1)
  }
  
  if (r <= 1) elements[position + 1]
  else {
    right.tail <- elements[(position + 2):length(elements)]
    c(elements[position + 1], combination(k, r - 1, right.tail))
  }
}

# Returns the kth r-selection of elements
selection <- function(k, r, elements) {
  n <- length(elements)
  position <- 0
  d <- choose(n + r - position - 2, r - 1)
  
  while ((k - 1) >= d) {
    k <- k - d
    position <- position + 1
    d <- choose(n + r - position - 2, r - 1)
  }
  
  if (r <= 1) elements[position + 1] ###
  else {
    tail <- elements[(position + 1):length(elements)]
    c(elements[position + 1], selection(k, r - 1, tail))
  }
}

# Returns the kth r-permutation of elements
permutation <- function(k, r, elements) {
  n <- length(elements)
  f <- factorial(r)
  group <- as.integer((k - 1) / f)
  item <- (k - 1) %% f
  
  comb <- combination(group + 1, r, elements)
  perm.worker(item + 1, comb)
}

# Returns the kth r-amalgam of elements
amalgam <- function(k, r, elements) {
  k <- k - 1
  sapply(
    1:r,
    function (i) {
      p <- length(elements) ^ (r - i)
      index <- as.integer(k / p)
      k <<- k %% p
      elements[index + 1]
    }
  )
}

# Returns the kth subset of elements
k.subset <- function(k, elements) {
  r <- c()
  for (i in 0:(length(elements) - 1)) 
    if (bitwAnd(k - 1, 2 ^ i) != 0) r <- c(r, elements[i + 1])
  r
}

# Index checks and adjustments
index.check <- function(i, n) {
  if (missing(i) || length(i) == 0) {
    cat("Warning: Missing an index. First combination returned.\n")
    i = 1
  }
  
  if (!is.numeric(i)) {
    cat("Warning: Numerical index expected. Index 1 used.\n")
    i = 1
  }
  
  if (length(i) > 1) {
    cat("Warning: A single index expected. Only the first element used.\n")
    i <- i[1]
  }
  
  if (i < 1 || i > n) {
    cat("Warning: Index out of bounds. Wrap-around used.\n")
    while (i < 1) i <- i + n
    i <- i %% (n + 1)
  }
  
  i
}