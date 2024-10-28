#' Substitute values of parameters in a SLiM extension template
#'
#' Substitute values of templated {{parameters}} in a given SLiM extension template
#'
#' If a file or a multi-line string given as \code{template} contains parameters
#' specified as \\code{{param}} where "param" can be arbitrary variable name, this
#' function substitutes such templated {{parameters}} for concrete values. Such
#' modified template is then used to extend a built-in slendr SLiM script, allowing
#' for a customization of its default behavior (most commonly replacing its
#' assumption of neutrality for non-neutral scenarios, such as simulations of
#' natural selection).
#'
#' @param template Either a path to an extension script file, or a string
#'   containing the entire SLiM extension code
#' @param ... Named function arguments interpreted as key=value pairs to be
#'   used in argument substitution
#'
#' @returns Path to a file with a saved extension script containing all
#'   substituted values
#' @export
substitute_values <- function(template, ...) {
  if (file.exists(template))
    extension_code <- readLines(template, warn = FALSE)
  else
    extension_code <- strsplit(template, "\n")[[1]]

  arguments <- list(...)

  for (arg in names(arguments)) {
    pattern <- sprintf("\\{\\{%s\\}\\}", arg)
    if (!any(grepl(pattern, extension_code))) {
      stop("Template pattern '{{", arg, "}}' not found in the extension script",
           call. = FALSE)
    }
    value <- arguments[[arg]]
    extension_code <- gsub(pattern, as.character(value), extension_code)
  }

  remaining_code <- gsub("(.*)//.*$", "\\1", extension_code)
  match_info <- regexpr("(\\{\\{.*\\}\\})", remaining_code)
  matched_groups <- regmatches(extension_code, match_info)

  if (length(matched_groups) > 0) {
    stop("The extension script contains the following unsubstituted patterns: ",
         paste(matched_groups, collapse = ", "), call. = FALSE)
  }

  substituted_code <- normalizePath(tempfile(), winslash = "/", mustWork = FALSE)
  writeLines(extension_code, substituted_code)

  substituted_code
}

check_initialization <- function(code) {
  # check for the presence of initialize*() SLiM calls
  init_calls <-
    c(any(grepl("initializeMutationType", code)),
      any(grepl("initializeMutationType", code)),
      any(grepl("initializeGenomicElementType", code)),
      any(grepl("initializeGenomicElement", code)),
      any(grepl("initializeMutationRate", code)),
      any(grepl("initializeRecombinationRate", code)))

  # an extension SLiM snippet can either have none or all the required
  # initialize*() calls
  if (!(sum(init_calls) == 0 || sum(init_calls) == length(init_calls))) {
    stop("SLiM extension snippets must either contain no initialize*()\n",
         "calls (and thus rely entirely on slendr's default initialization), or\n",
         "they must contain an initialization callback with at least the\n",
         "following minimal set of genomic initialization calls anywhere in\n",
         "the extension code:\n\n",
         "initialize() {\n",
         "    initializeMutationType(...);\n",
         "    initializeGenomicElementType(...);\n",
         "    initializeGenomicElement(...);\n",
         "    initializeMutationRate(...);\n",
         "    initializeRecombinationRate(...);\n",
         "}\n\n",
         "This is just a minimal required example. Of course, you are free to\n",
         "set up mutation and genomic initialization of arbitrary complexity.\n\n",
         "If you wish, you can still use slendr's constants SEQUENCE_LENGTH\n",
         "and RECOMBINATION_RATE as passed through the slim() function.",
         call. = FALSE
    )
  }

  sum(init_calls) == length(init_calls)
}
