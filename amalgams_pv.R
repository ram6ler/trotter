# Amalgams Pseudo-vector ###################################################

setClass(
  Class = "APV",
  representation(k = "numeric", items = "vector"),
)

setMethod(
  f = "show",
  signature = "APV",
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

#' Amalgams Pseudo-Vector Length
#' @description
#' Get the length of an \code{APV} instance.
#' @param x an instance of \code{APV}
#' @return the number of amalgams (permutations with replacement) in pseudo-vector \code{x}
#' @details
#' Since \code{x} contains all the \code{k}-amalgams of objects in vector
#' \code{items}, \code{length(x)} will return \code{length(items) ^ k)}.
#' @seealso Permutations Pseudo-Vector \code{\link{ppv}} 
#' @seealso Combinations Pseudo-Vector \code{\link{cpv}} 
#' @seealso Selections Pseudo-Vector \code{\link{spv}} 
#' @seealso Subsets Pseudo-Vector \code{\link{sspv}} 

setMethod(
  f = "length",
  signature = "APV",
  definition = function(x) length(x@items) ^ x@k 
)

#' Retrieve an Amalgam by Index
#' @description
#' Access an amalgam (permutation with replacement) stored in an \code{APV} instance by index.
#' @param x an instance of \code{APV}.
#' @param i an index specifying the position of the sought amalgam
#' @param j not used.
#' @param drop not used.
#' @return the amalgam located at position \code{i} in pseudo-vector \code{x}
#' @details
#' The amalgam at index \code{i} of pseudo-vector \code{x} is not actually 
#' stored in memory but calculated as needed. The extract method is used solely
#' for interpretation.
#' 
#' @seealso Permutations Pseudo-Vector \code{\link{ppv}} 
#' @seealso Combinations Pseudo-Vector \code{\link{cpv}} 
#' @seealso Selections Pseudo-Vector \code{\link{spv}} 
#' @seealso Subsets Pseudo-Vector \code{\link{sspv}} 

setMethod(
  f = "[",
  signature = "APV",
  definition = function(x, i, j, drop) {
    i <- index.check(i, length(x))
    if (!missing(j)) cat("Warning: Only first coordinate used.\n")
    amalgam(i, x@k, x@items)
  }
)

# Export #######################################################################

#' Amalgams Pseudo-Vector Constructor
#' @description
#' The \code{APV} class defines a pseudo-vector containing all 
#' the arranged \code{k}-amalgams (permutations with replacement) of the objects stored
#' in \code{items}. The function \code{apv} is a constructor for this class.
#' @aliases
#' amalgam
#' amalgams
#' @param k the number of objects taken at a time.
#' @param items a vector of objects to be amalgamated.
#' @return an instance of \code{APV}.
#' @author Richard Ambler
#' @details
#' The amalgams are arranged according to the order in which the objects
#' appear in \code{items}. The arrangement is very similar to that used by the \code{PPV} class
#' (see \link{ppv}) except that objects are replaced during permutation creation.
#' 
#' @examples
#' # create a pseudo-vector of 10-amalgams from the first 15 letters
#' a <- apv(10, letters[1:15])
#' # generate a description
#' print(a)
#' # compatable with length
#' length(a)
#' # inspect a few of the combinations "stored" in a
#' a[1]
#' a[1000000]
#' a[576650390625]
#' @seealso Permutations Pseudo-Vector \code{\link{ppv}} 
#' @seealso Combinations Pseudo-Vector \code{\link{cpv}}
#' @seealso Selections Pseudo-Vector \code{\link{spv}} 
#' @seealso Subsets Pseudo-Vector \code{\link{sspv}} 
#' @references
#' Steinhaus-Johnson-Trotter algorithm. (2014, April 29).
#' In \emph{Wikipedia, The Free Encyclopedia}.
#' Retrieved 13:24, September 5, 2014
#' @export
#' @import methods

apv <- function(k, items) new(
  Class = "APV", 
  k = k, 
  items = items
)