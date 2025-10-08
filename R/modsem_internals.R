.internalModsemAPI <- function(nm) {
  # Function for calling internal modsem objects
  # using ::: directly leads to an R CMD check note

  eval(parse(text=paste0("modsem:::", nm)))
}


reverseIntTerm   <- .internalModsemAPI("reverseIntTerm")
isLavaanObject   <- .internalModsemAPI("isLavaanObject")
getMissingLabels <- .internalModsemAPI("getMissingLabels")
getMean          <- .internalModsemAPI("getMean")
calcCovParTable  <- .internalModsemAPI("calcCovParTable")
