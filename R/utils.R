select_last <- function(x) {
  if (all(is.na(x))) {
    return(x[1])
  } else {
  no_na <- stats::na.omit(x)
  no_na[length(no_na)]
  }
}

select_mode <- function(x) {
  ux <- stats::na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

select_max <- function(x) {
  if (all(is.na(x))) {
    return(x[1])
  } else {
  max(x, na.rm = TRUE)
  }
}

select_min <- function(x) {
  if (all(is.na(x))) {
    return(x[1])
  } else {
  min(x, na.rm = TRUE)
  }
}