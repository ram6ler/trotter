# Selections Pseudo-vector ###################################################

setClass(
  Class = "SPV",
  representation(k = "numeric", items = "vector"),
)

setMethod(
  f = "show",
  signature = "SPV",
  definition = function(object) cat(
    sprintf(
      paste(
        "Instance of class SPV\n",
        "Pseudo-vector containing %s %s-selections (combinations with replacement)\n",
        "of items taken from the list:\n[%s]",
        collapse = ""
      ),
      length(object),
      object@k,
      paste(object@items, collapse = ", ")
    )
  )
)

#' Selections Pseudo-Vector Length
#' @description
#' Get the length of an \code{SPV} instance.
#' @param x an instance of \code{SPV}
#' @return the number of selections (combinations with replacement) in pseudo-vector \code{x}
#' @details
#' Since \code{x} contains all the \code{k}-selections of objects in vector
#' \code{items}, \code{length(x)} will return \code{choose(length(items) + k - 1, k)}.
#' @seealso Permutations Pseudo-Vector \code{\link{ppv}} 
#' @seealso Combinations Pseudo-Vector \code{\link{cpv}} 
#' @seealso Amalgams Pseudo-Vector \code{\link{apv}}  
#' @seealso Subsets Pseudo-Vector \code{\link{sspv}} 

setMethod(
  f = "length",
  signature = "SPV",
  definition = function(x) choose(length(x@items) + x@k - 1, x@k)
)

#' Retrieve a Selection by Index
#' @description
#' Access a selection (combination with replacement) stored in an \code{SPV} instance by index.
#' @param x an instance of \code{SPV}.
#' @param i an index specifying the position of the sought selection.
#' @param j not used.
#' @param drop not used.
#' @return the selection located at position \code{i} in pseudo-vector \code{x}
#' @details
#' The selection at index \code{i} of pseudo-vector \code{x} is not actually 
#' stored in memory but calculated as needed. The extract method is used solely
#' for interpretation.
#' 
#' @seealso Permutations Pseudo-Vector \code{\link{ppv}} 
#' @seealso Combinations Pseudo-Vector \code{\link{cpv}} 
#' @seealso Amalgams Pseudo-Vector \code{\link{apv}} 
#' @seealso Subsets Pseudo-Vector \code{\link{sspv}} 

setMethod(
  f = "[",
  signature = "SPV",
  definition = function(x, i, j, drop) {
    i <- index.check(i, length(x))
    if (!missing(j)) cat("Warning: Only first coordinate used.\n")
    selection(i, x@k, x@items)
  }
)

# Export #######################################################################

#' Selections Pseudo-Vector Constructor
#' @description
#' The \code{SPV} class defines a pseudo-vector containing all 
#' the arranged \code{k}-selections (combinations with replacement) of the objects stored
#' in \code{items}. The function \code{spv} is a constructor for this class.
#' @aliases
#' selection
#' selections
#' @param k the number of objects taken at a time.
#' @param items a vector of objects to be selected.
#' @return an instance of \code{SPV}.
#' @author Richard Ambler
#' @details
#' The selections are arranged according to the order in which the objects
#' appear in \code{items}. The arrangement is very similar to the arrangement
#' of combinations (see \link{cpv}) except that objects may be repeatedly selected.
#' 
#' @examples
#' # create a pseudo-vector of 10-selections from the first 15 letters
#' s <- spv(10, letters[1:15])
#' # generate a description
#' print(s)
#' # compatable with length
#' length(s)
#' # inspect a few of the combinations "stored" in s
#' s[1]
#' s[1000]
#' s[1961256]
#' @seealso Permutations Pseudo-Vector \code{\link{ppv}} 
#' @seealso Combinations Pseudo-Vector \code{\link{cpv}} 
#' @seealso Amalgams Pseudo-Vector \code{\link{apv}} 
#' @seealso Subsets Pseudo-Vector \code{\link{sspv}} 
#' @references
#' Steinhaus-Johnson-Trotter algorithm. (2014, April 29).
#' In \emph{Wikipedia, The Free Encyclopedia}.
#' Retrieved 13:24, September 5, 2014
#' @export
#' @import methods

spv <- function(k, items) new(
  Class = "SPV", 
  k = k, 
  items = items
)