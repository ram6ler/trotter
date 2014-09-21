# Subsets Pseudo-vector ###################################################

setClass(
  Class = "SSPV",
  representation(items = "vector"),
)

setMethod(
  f = "show",
  signature = "SSPV",
  definition = function(object) cat(
    sprintf(
      paste(
        "Instance of class SSPV\n",
        "Pseudo-vector containing the %s subsets\n",
        "of items taken from the list:\n[%s]",
        collapse = ""
      ),
      length(object),
      paste(object@items, collapse = ", ")
    )
  )
)

#' Subsets Pseudo-Vector Length
#' @description
#' Get the length of an \code{SSPV} instance.
#' @param x an instance of \code{SSPV}
#' @return the number of subsets in pseudo-vector \code{x}
#' @details
#' Since \code{x} contains all the subsets of objects in vector
#' \code{items}, \code{length(x)} will return \code{2 ^ length(items)}.
#' @seealso Permutations Pseudo-Vector \code{\link{ppv}} 
#' @seealso Combinations Pseudo-Vector \code{\link{cpv}} 
#' @seealso Amalgams Pseudo-Vector \code{\link{apv}} 
#' @seealso Selections Pseudo-Vector \code{\link{spv}} 


setMethod(
  f = "length",
  signature = "SSPV",
  definition = function(x) 2 ^ length(x@items) 
)

#' Retrieve a Subset by Index
#' @description
#' Access asubset stored in an \code{SSPV} instance by index.
#' @param x an instance of \code{SSPV}.
#' @param i an index specifying the position of the sought amalgam
#' @param j not used.
#' @param drop not used.
#' @return the subset located at position \code{i} in pseudo-vector \code{x}
#' @details
#' The subset at index \code{i} of pseudo-vector \code{x} is not actually 
#' stored in memory but calculated as needed. The extract method is used solely
#' for interpretation.
#' @seealso Permutations Pseudo-Vector \code{\link{ppv}} 
#' @seealso Combinations Pseudo-Vector \code{\link{cpv}} 
#' @seealso Amalgams Pseudo-Vector \code{\link{apv}} 
#' @seealso Selections Pseudo-Vector \code{\link{spv}}

setMethod(
  f = "[",
  signature = "SSPV",
  definition = function(x, i, j, drop) {
    i <- index.check(i, length(x))
    if (!missing(j)) cat("Warning: Only first coordinate used.\n")
    k.subset(i, x@items)
  }
)

# Export #######################################################################

#' Subsets Pseudo-Vector Constructor
#' @description
#' The \code{SSPV} class defines a pseudo-vector containing all 
#' the arranged subsets of the objects stored
#' in \code{items}. The function \code{sspv} is a constructor for this class.
#' @aliases
#' subsets
#' @param items a vector of objects to be subsetted.
#' @return an instance of \code{SSPV}.
#' @author Richard Ambler
#' @details
#' The subsets are arranged according to the order in which the objects
#' appear in \code{items}. The first subset, containing none of the objects, 
#' is \code{NULL}.
#' 
#' @examples
#' # create a pseudo-vector of subsets from the first 15 letters
#' ss <- sspv(letters[1:15])
#' # generate a description
#' print(ss)
#' # compatable with length
#' length(ss)
#' # inspect a few of the combinations "stored" in ss
#' ss[1]
#' ss[1000]
#' ss[32768]
#' @seealso Permutations Pseudo-Vector \code{\link{ppv}} 
#' @seealso Combinations Pseudo-Vector \code{\link{cpv}} 
#' @seealso Amalgams Pseudo-Vector \code{\link{apv}} 
#' @seealso Selections Pseudo-Vector \code{\link{spv}}
#' @export
#' @import methods

sspv <- function(items) new(
  Class = "SSPV",
  items = items
)