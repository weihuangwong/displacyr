#' @importFrom utils getFromNamespace tail

hilite <- function(x, deco = "red") {
  .dec <- getFromNamespace(deco, "crayon")
  tokens <- strsplit(x, " ")[[1]]
  if (length(tokens) > 1)
    tokens <- c(paste0(tokens[1:(length(tokens) - 1)], " "),
      tail(tokens, 1))
  paste(sapply(c(" ", tokens, " "), .dec), collapse = "")
}

#' Visualize entities in text parsed with spacyr.
#'
#' @param x     A spacyr_parsed object
#' @param deco_ent  A crayon style for entity name, given as a character string
#' @param deco_lab  A crayon style for entity label, given as a character string
#' @return A character string with styled tokens
#' @details x must include the "whitespace_" attribute. Therefore, \code{spacy_parse}
#'   should be run with the \code{additional_attributes} argument:
#'   \code{spacy_parse(x, ..., additional_attributes = "whitespace_")}. This
#'   function is designed to be called for its side effect, which outputs
#'   a styled character vector to the terminal.
#' @examples
#' \dontrun{
#' txt <- "Larry Page is the co-founder of Google, located in Mountain View, CA."
#' parsed <- spacyr::spacy_parse(txt, additional_attributes = "whitespace_")
#' display_ent(parsed)
#' }
#' @export
display_ent <- function(x, deco_ent = "bgYellow", deco_lab = "bgBlue") {
  if (!inherits(x, "spacyr_parsed"))
    stop('x must be output from spacyr_parsed')
  if (!all(c("entity", "whitespace_") %in% names(x)))
    stop('Dataframe must have "entity" and "whitespace_" columns; try spacy_parse(x, ..., entity = TRUE, additional_attributes = "whitespace_")')
  .dec <- getFromNamespace(deco_lab, "crayon")
  # create token groups
  x$index <- 1:nrow(x)
  for (i in 2:nrow(x)) {
    if (grepl("_I$", x$entity[i])) x$index[i] <- x$index[i - 1]
  }
  groups <- split(x, x$index)
  out <- unlist(lapply(groups, function(s) {
    entity <- gsub("^(.*)_B$", "\\1", s$entity[1])
    entity_label <- ""
    if (entity != "")
      entity_label <- paste0(.dec(sprintf(" %s ", entity)), tail(s$whitespace_, 1))
    entity_token <- paste(paste0(s$token, s$whitespace_), collapse = "")
    if (entity == "") return(entity_token)
    paste0(hilite(entity_token, deco = deco_ent), entity_label)
  }))
  disp_out <- paste(out, collapse = "")
  cat(disp_out, "\n", sep = "\n")
  invisible(out)
}

