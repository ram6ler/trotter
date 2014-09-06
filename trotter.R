# Helpers ######################################################################

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

# Returns the kth r-permutation of elements
permutation <- function(k, r, elements) {
  n <- length(elements)
  f <- factorial(r)
  group <- as.integer((k - 1) / f)
  item <- (k - 1) %% f
  
  comb <- combination(group + 1, r, elements)
  perm.worker(item + 1, comb)
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

# Permutations Pseudo-vector ###################################################

setClass(
  Class = "PPV",
  representation(k = "numeric", items = "vector"),
)

setMethod(
  f = "show",
  signature = "PPV",
  definition = function(object) cat(
    sprintf(
      paste(
        "Instance of class PPV\n",
        "Pseudo-vector containing %s %s-permutations\n",
        "of items taken from the list:\n[%s]",
        collapse = ""
      ),
      length(object),
      object@k,
      paste(object@items, collapse = ", ")
    )
  )
)

setMethod(
  f = "length",
  signature = "PPV",
  definition = function(x) choose(length(x@items), x@k) * factorial(x@k)
)

setMethod(
  f = "[",
  signature = "PPV",
  definition = function(x, i, j, drop) {
    i <- index.check(i, length(x))
    if (!missing(j)) cat("Warning: Only first coordinate used.\n")
    permutation(i, x@k, x@items)
  }
)

# Export #######################################################################

#' Permutations Pseudo-Vector 
#' @description
#' The \code{PPV} class defines a pseudo-vector containing all 
#' the \code{k}-permutations of the objects stored
#' in \code{items}. The function \code{ppv} is a constructor for this class.
#' @aliases
#' permutation
#' permutations
#' @param k the number of objects taken at a time.
#' @param items a vector of objects to be permuted.
#' @return an instance of \code{PPV}.
#' @author Richard Ambler
#' @examples
#' # create a pseudo-vector of 10-permutations from the first 15 letters
#' p <- ppv(10, letters[1:15])
#' # generate a description
#' print(p)
#' # compatable with length
#' length(p)
#' # inspect a few of the permutations "stored" in p
#' p[1]
#' p[1000000]
#' p[10897286400]
#' @seealso Combinations Pseudo-Vector \code{\link{cpv}}
#' @references
#' Steinhaus-Johnson-Trotter algorithm. (2014, April 29).
#' In \emph{Wikipedia, The Free Encyclopedia}.
#' Retrieved 13:24, September 5, 2014
#' @export

ppv <- function(k, items) new(
  Class = "PPV", 
  k = k, 
  items = items
)

# Combinations Pseudo-vector ###################################################

setClass(
  Class = "CPV",
  representation(k = "numeric", items = "vector"),
)

setMethod(
  f = "show",
  signature = "CPV",
  definition = function(object) cat(
    sprintf(
      paste(
        "Instance of class CPV\n",
        "Pseudo-vector containing %s %s-combinations\n",
        "of items taken from the list:\n[%s]",
        collapse = ""
      ),
      length(object),
      object@k,
      paste(object@items, collapse = ", ")
    )
  )
)

setMethod(
  f = "length",
  signature = "CPV",
  definition = function(x) choose(length(x@items), x@k)
)

setMethod(
  f = "[",
  signature = "CPV",
  definition = function(x, i, j, drop) {
    i <- index.check(i, length(x))
    if (!missing(j)) cat("Warning: Only first coordinate used.\n")
    combination(i, x@k, x@items)
  }
)

# Export #######################################################################

#' Combinations Pseudo-Vector 
#' @description
#' The \code{CPV} class defines a pseudo-vector containing all 
#' the arranged \code{k}-combinations of the objects stored
#' in \code{items}. The function \code{cpv} is a constructor for this class.
#' @aliases
#' combination
#' combinations
#' @param k the number of objects taken at a time.
#' @param items a vector of objects to be permuted.
#' @return an instance of \code{CPV}.
#' @author Richard Ambler
#' @examples
#' # create a pseudo-vector of 10-combinations from the first 15 letters
#' c <- cpv(10, letters[1:15])
#' # generate a description
#' print(c)
#' # compatable with length
#' length(c)
#' # inspect a few of the combinations "stored" in c
#' c[1]
#' c[1000]
#' c[3003]
#' @seealso Permutations Pseudo-Vector \code{\link{ppv}}
#' @references
#' Steinhaus-Johnson-Trotter algorithm. (2014, April 29).
#' In \emph{Wikipedia, The Free Encyclopedia}.
#' Retrieved 13:24, September 5, 2014
#' @export

cpv <- function(k, items) new(
  Class = "CPV", 
  k = k, 
  items = items
)
