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

#' Combinations Pseudo-Vector Length
#' @description
#' Get the length of a \code{CPV} instance.
#' @param x an instance of \code{CPV}
#' @return the number of combinations in pseudo-vector \code{x}
#' @details
#' Since \code{x} contains all the \code{k}-combinations of objects in vector
#' \code{items}, \code{length(x)} will return \code{choose(length(items), k)}.
#' @seealso Permutations Pseudo-Vector \code{\link{ppv}} 
#' @seealso Amalgams Pseudo-Vector \code{\link{apv}} 
#' @seealso Selections Pseudo-Vector \code{\link{spv}} 
#' @seealso Subsets Pseudo-Vector \code{\link{sspv}} 

setMethod(
  f = "length",
  signature = "CPV",
  definition = function(x) choose(length(x@items), x@k)
)

#' Retrieve a Combination by Index
#' @description
#' Access a combination stored in a \code{CPV} instance by index.
#' @param x an instance of \code{CPV}.
#' @param i an index specifying the position of the sought combination.
#' @param j not used.
#' @param drop not used.
#' @return the combination located at position \code{i} in pseudo-vector \code{x}
#' @details
#' The combination at index \code{i} of pseudo-vector \code{x} is not actually 
#' stored in memory but calculated as needed. The extract method is used solely
#' for interpretation.
#' 
#' @seealso Permutations Pseudo-Vector \code{\link{ppv}} 
#' @seealso Amalgams Pseudo-Vector \code{\link{apv}} 
#' @seealso Selections Pseudo-Vector \code{\link{spv}} 
#' @seealso Subsets Pseudo-Vector \code{\link{sspv}} 

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

#' Combinations Pseudo-Vector Constructor 
#' @description
#' The \code{CPV} class defines a pseudo-vector containing all 
#' the arranged \code{k}-combinations of the objects stored
#' in \code{items}. The function \code{cpv} is a constructor for this class.
#' @aliases
#' combination
#' combinations
#' @param k the number of objects taken at a time.
#' @param items a vector of objects to be combined.
#' @return an instance of \code{CPV}.
#' @author Richard Ambler
#' @details
#' The combinations are arranged according to the order in which the objects
#' appear in \code{items}. Combinations containing the first object in 
#' \code{items} are followed by combinations that contain the second object
#' but not the first, which are followed by combinations that contain the third
#' but neither the first or the second, etc.
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
#' @seealso Amalgams Pseudo-Vector \code{\link{apv}} 
#' @seealso Selections Pseudo-Vector \code{\link{spv}} 
#' @seealso Subsets Pseudo-Vector \code{\link{sspv}} 
#' @references
#' Steinhaus-Johnson-Trotter algorithm. (2014, April 29).
#' In \emph{Wikipedia, The Free Encyclopedia}.
#' Retrieved 13:24, September 5, 2014
#' @export
#' @import methods

cpv <- function(k, items) new(
  Class = "CPV", 
  k = k, 
  items = items
)