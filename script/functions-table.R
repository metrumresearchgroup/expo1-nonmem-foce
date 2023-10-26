### General table-related helper functions ####  -------------------------------------------------------
asNum  <- function(f){ return(as.numeric(as.character(f))) }
sig = pmtables::sig
select <- dplyr::select
rename <- dplyr::rename
ggplot2::theme_set(ggplot2::theme_bw())
parens <- function(x) paste0('(',x,')')
parensSQ <- function(x) paste0('[',x,']')
parensSQ_CV <- function(.x) glue::glue("[CV\\%=<<.x>>]", .open = "<<", .close  = ">>")
parensSQ_corr <- function(.x) glue::glue("[Corr=<<.x>>]", .open = "<<", .close  = ">>")
parensSQ_se <- function(.x) glue::glue("[SD=<<.x>>]", .open = "<<", .close  = ">>")
getEvenNo = function(x) x[which(x %% 2 == 0)]


## If not using bbr function: extract required parameters from the ext file ---------------------------
read_extfile <- function(file) {
  tab <- read_table(file,skip = 1)  
  iteration <- tab[["ITERATION"]]
  tab[c("ITERATION", "OBJ")] <- NULL
  estim <- filter(tab, iteration == -1000000000)        # Estimate
  stderr <- filter(tab,    iteration == -1000000001)    # Standard error of the estimate
  random_effect_sd <- filter(tab,    iteration == -1000000004)  # Correlation matrix
  random_effect_sdse<- filter(tab,    iteration == -1000000005) # SE of correlation matrix
  tibble(
    parameter_names = names(estim), 
    estimate = unlist(estim),
    stderr = unlist(stderr),
    random_effect_sd = unlist(random_effect_sd),
    random_effect_sdse = unlist(random_effect_sdse),
    diag = str_match_all(parameter_names, "([0-9]+),([0-9]+)") %>%
      map_lgl(~.x[2]==.x[3]) %>% replace_na(FALSE),
    fixed = stderr > 1e9
  )
}

## If not using bbr function: extract shrinkage from the shk file ---------------------------
read_shkfile <- function(file) {
  tab <- suppressWarnings(read_table(file,skip = 1))  
  shrink <- tab %>% 
    filter(TYPE == 4) %>% 
    select(names(tab)[str_detect(colnames(tab), "ET")])        
  tibble(
    etas = names(shrink), 
    num = unlist(stringr::str_extract(etas, "\\d+")),
    name = paste0("OMEGA", num, num),
    shrinkage = unlist(shrink),
  ) %>% 
    select(name, shrinkage)
}





