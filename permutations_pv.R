# Permutations Pseudo-vector

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

#' Permutations Pseudo-Vector Length
#' @description
#' Get the length of a \code{PPV} instance.
#' @param x an instance of \code{PPV}
#' @return the number of permutations in pseudo-vector \code{x}
#' @details
#' Since \code{x} contains all the \code{k}-permutations of objects in vector
#' \code{items}, \code{length(x)} will return 
#' \code{choose(length(items), k) * factorial(k)}.
#' @seealso Combinations Pseudo-Vector \code{\link{cpv}} 
#' @seealso Amalgams Pseudo-Vector \code{\link{apv}} 
#' @seealso Selections Pseudo-Vector \code{\link{spv}} 
#' @seealso Subsets Pseudo-Vector \code{\link{sspv}} 

setMethod(
  f = "length",
  signature = "PPV",
  definition = function(x) choose(length(x@items), x@k) * factorial(x@k)
)

#' Retrieve a Permutation by Index
#' @description
#' Access a permutation stored in a \code{PPV} instance by index.
#' @param x an instance of \code{PPV}.
#' @param i an index specifying the position of the sought permutation.
#' @param j not used.
#' @param drop not used.
#' @return the permutation located at position \code{i} in pseudo-vector \code{x}
#' @details
#' The permutation at index \code{i} of pseudo-vector \code{x} is not actually 
#' stored in memory but calculated as needed. The extract method is used solely
#' for interpretation.
#' 
#' @seealso Combinations Pseudo-Vector \code{\link{cpv}} 
#' @seealso Amalgams Pseudo-Vector \code{\link{apv}} 
#' @seealso Selections Pseudo-Vector \code{\link{spv}} 
#' @seealso Subsets Pseudo-Vector \code{\link{sspv}} 

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

#' Permutations Pseudo-Vector Constructor 
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
#' @details
#' The arrangement of permutations is similar, but in many cases not identical, 
#' to that obtained from the
#' Steinhaus-Johnson-Trotter algorithm (see references).
#' @examples
#' # create a pseudo-vector of 5-permutations from the first 10 letters
#' p <- ppv(5, letters[1:10])
#' # generate a description
#' print(p)
#' # compatable with length
#' length(p)
#' # inspect a few of the permutations "stored" in p
#' p[1]
#' p[1000]
#' p[length(p)]
#' @seealso Combinations Pseudo-Vector \code{\link{cpv}} 
#' @seealso Amalgams Pseudo-Vector \code{\link{apv}} 
#' @seealso Selections Pseudo-Vector \code{\link{spv}} 
#' @seealso Subsets Pseudo-Vector \code{\link{sspv}} 
#' @references
#' Steinhaus-Johnson-Trotter algorithm. (2014, April 29).
#' In \emph{Wikipedia, The Free Encyclopedia}.
#' Retrieved 13:24, September 5, 2014
#' @export
#' @import methods

ppv <- function(k, items) new(
  Class = "PPV", 
  k = k, 
  items = items
)
