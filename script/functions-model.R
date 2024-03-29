### Helper functions for modeling with bbr ####  ----------------------------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(bbr))

#' Helper to open control stream for editing in Rstudio
#' @param .mod a `bb{.model_type}_model` object
edit_model <- function(.mod){
  .mod %>%
    get_model_path() %>% 
    file.edit()
}

#' Check tags against external glossary
#' 
#' Checks whether any of `tags` are _not_ contained in
#' the list or vector passed to `glossary`
#' @param tags Object to check. Can be a character vector of tags, a `bbi_model`
#'   object, or a `bbi_log_df` tibble.
#' @param glossary Either a list or character vector of tags to check against. 
audit_tags <- function(tags, glossary) {
  UseMethod("audit_tags")
}

#' @describeIn audit_tags Takes a character vector of tags and 
#' checks if any are not contained in `glossary`
audit_tags.character <- function(tags, glossary) {
  tags_bool <- tags %in% glossary
  
  if (any(!tags_bool)) {
    warning(paste(
      "The following are not valid tags:",
      paste(tags[which(!tags_bool)], collapse = ", ")
    ), call. = FALSE)
    return(invisible(FALSE))
  }
  return(invisible(TRUE))
}

#' @describeIn audit_tags Takes a `bbi_nonmem_model` object and 
#' checks if any of the tags are not contained in `glossary`
audit_tags.bbi_nonmem_model <- function(tags, glossary) {
  audit_tags(tags$tags, glossary)
}

#' @describeIn audit_tags Takes a tibble of class `bbi_run_log_df` (the output of [bbr::run_log()])
#' and checks if any of the models contain tags that are not contained in `glossary`
audit_tags.bbi_run_log_df <- function(tags, glossary) {
  log_df <- tags # rename arg for clarity
  tags_bool <- purrr::map(log_df$tags, function(.t) {
    .t %in% glossary
  } )
  
  bad_mods <- purrr::map_lgl(tags_bool, ~any(!.x))
  
  if (any(bad_mods)) {
    bad_msg <- purrr::map_chr(which(bad_mods), function(.m) {
      mod_path <- log_df$absolute_model_path[.m]
      bad_tags <- log_df$tags[[.m]][!tags_bool[[.m]]]
      paste0("  ", mod_path, ": ", paste(bad_tags, collapse = ", "))
    })
    
    warning(paste(c(
      "The following models have invalid tags:",
      bad_msg
    ), collapse = "\n"))
  }
  
  invisible(!bad_mods)
}


#' Compare objective function values between models
#'
#' Takes a bbi_summary_log_df and character vector of models 
#' and filters to only those models. 
#' Returns tibble with run, ofv, tags,
#' and any columns passed to ...
compare_ofv <- function(models, log_df, ...) {
  log_df %>%
    filter(run %in% models) %>%
    select(run, ofv, tags, ...) %>%
    collapse_to_string(tags, ...) %>%
    knitr::kable()
}

#' HTML formatting for tags in run log
#' 
#' Returns the input tibble, with an extra column `tags_diff` that contains any
#' tags _added_ to a given model (as compared to its parent model) and any tags
#' _removed_ with a strikethrough.
#' @param .log_df The tibble returned from `run_log() %>% add_tags_diff()`
#' @param .keep Whether to keep the `tags_added` and `tags_removed` columns from `add_tags_diff()`
format_tags_diff <- function(.log_df, .keep = FALSE) {
  if (
    !any(str_detect(names(.log_df), "tags_added")) ||
    !any(str_detect(names(.log_df), "tags_removed"))
  ) {
    stop("Must have both `tags_added` and `tags_removed` columns to call `format_tags_diff()`. Use `add_tags_diff()` to create them")
  }
  .log_df <- bbr::suppressSpecificWarning(
    collapse_to_string(.log_df, tags_added, tags_removed),
    .regexpr = "collapse_to_string\\(\\) only works on list columns"
  ) %>%
    mutate(
      tags_diff = paste0(tags_added, "; ~~", tags_removed, "~~") %>% 
        str_replace("; ~~~~","") %>% 
        str_replace("^; ", "")
    )
  if (isFALSE(.keep)) {
    .log_df <- select(.log_df, -tags_added, -tags_removed)
  }
  return(.log_df)
}
