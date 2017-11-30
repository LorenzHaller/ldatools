#' Wrapes variable in a function
#'
#' @param x A String the will be wrapped in \code{fname}.
#' @param fname A string of the special function in which \code{x} will be
#' wrapped in. Defaults to \code{"rcs"}
#' @seealso \code{\link[rms]{rms.trans}}
#' @keywords internal
fpaste <- function(x, fname=NULL) {
  if(is.null(fname)) {
    x
  } else {
    paste0(fname, "(", x, ")")
  }
}

#' Check if string appears in character vector multiple times
#'
#' @param char The string for which to check multiple appearances in \code{vec}.
#' @param vec The character vector in which to check multiple appearences of \code{char}.
#' @return \code{TRUE} if \code{char} appears more than once in \code{vec},
#' \code{FALSE} otherwise.
#' @keywords internal
get_multappear <- function(char, vec) {
  sum(grepl(char, vec, fixed=TRUE)) > 1
}

#' Check multiple strings for multiple appearances
#'
#' A vectorized version of \code{get_multappear}.
#'
#' @param charvec A Vector of strings. Each will be checked for multiple appearences
#' in \code{vec}
#' @inheritParams get_multappear
#' @return \code{TRUE} if any element of \code{charvec} appears in \code{vec}
#' more than once, \code{FALSE} otherwise.
#' @keywords internal
vget_multappear <- function(charvec, vec) {
  any(sapply(charvec, function(x) {
    get_multappear(x, vec)
  }))
}

#' Create formulas for all possible combinations of variables
#'
#' Creates formulas for all possible combinations of variables provided to
#' the function. All terms can appear as linear terms or non-linear terms.
#' @param covars A character vector of variables for which all possible
#' unique model formulas will be created.
#' @param lhs The left-hand side of the formula to be produced.
#' @inheritParams fpaste
#' @importFrom e1071 bincombinations
#' @return A vector of formula strings for possible (and correct) model
#' specifications given the variables provided.
#' @export
create_formulas <- function(
  covars,
  lhs   = "Surv(time, status)~",
  fname = NULL) {

  if(!is.null(fname)) {
    covars2  <- c(covars, fpaste(covars, fname=fname))
  } else {
    covars2 <- covars
  }
  combos   <- e1071::bincombinations(length(covars2))[-1,] == 1
  ind.mult <- apply(combos, 1, function(z) vget_multappear(covars, covars2[z]))
  formxy   <- sapply(which(!ind.mult),
    function(z) {
        paste0(covars2[combos[z,, drop=TRUE]], collapse="+")
  })

  return(paste0(lhs, formxy))

}
