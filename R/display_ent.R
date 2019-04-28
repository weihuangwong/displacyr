#' @importFrom utils getFromNamespace tail

hilite <- function(x, deco = "red") {
  deco <- getFromNamespace(deco, "crayon")
  if (grepl(" $", x)) {
    paste(deco(trimws(x)), " ")
  } else {
    deco(x)
  }
}

#' Visualize entities in text parsed with Spacy.
#'
#' @param x     A spacyr_parsed object
#' @param deco  A crayon style, given as a character string
#' @return A character vector with styled tokens
#' @details x must include the "whitespace_" attribute. Therefore, \code{spacy_parse}
#'   should be run with the \code{additional_attributes} argument:
#'   \code{spacy_parse(x, ..., additional_attributes = "whitespace_")}. This
#'   function is designed to be called for its side effect, which outputs
#'   a styled character vector to the terminal.
#' @examples
#' \dontrun{
#' txt <- "Kenneth Benoit is currently Professor of Computational Social
#' Science in the Department of Methodology at the London School of
#' Economics and Political Science. He has previously held positions in
#' the Department of Political Science at Trinity College Dublin and at
#' the Central European University (Budapest)."
#' txt <- gsub("\n", " ", txt)
#
#' parsed <- spacyr::spacy_parse(txt, lemma = FALSE, additional_attributes = "whitespace_")
#' display_ent(parsed, deco = "yellow")
#' }
#' @export
display_ent <- function(x, deco = "red") {
  if (!("whitespace_" %in% names(x)))
    stop('Dataframe must have "whitespace_" column; try spacy_parse(x, ..., additional_attributes = "whitespace_")')
  # create token groups
  x$index <- 1:nrow(x)
  for (i in 2:nrow(x)) {
    if (grepl("_I$", x$entity[i])) x$index[i] <- x$index[i-1]
  }
  groups <- split(x, x$index)
  out <- unlist(lapply(groups, function(x) {
    entity <- gsub("^(.*)_B$", "\\1", x$entity[1])
    if (entity != "")
      x$token[nrow(x)] <- paste(tail(x$token, 1), sprintf("[%s]", entity))
    entity_token <- paste(paste0(x$token, x$whitespace_), collapse = "")
    if (entity == "") return(entity_token)
    hilite(entity_token, deco = deco)
  }))
  disp_out <- strwrap(paste(out, collapse = ""), width = 80)
  cat(disp_out, "\n", sep = "\n")
  out
}

