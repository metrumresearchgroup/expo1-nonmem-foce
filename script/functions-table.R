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


### For parameter tables ####  -------------------------------------------------------
# Most CV equations https://ascpt.onlinelibrary.wiley.com/doi/full/10.1002/psp4.12404
getCV_lognormO  <- function(v) sqrt(exp(v) - 1) * 100
getCV_propS  <- function(v) sqrt(v) * 100
### shouldn't report CV% for add etas unless it's constrained to be positive
# getCV_addO  <- function(v, t) (sqrt(v) / abs(t)) * 100  

#' SD and %CV for logit-normal distributions
#' Decided CV% not appropriate for logit transforms so report SD
#' %CV for random variable Y,
#'   Y = a + b * (1 / (1 + exp(-X))),
#' where X ~ N(mu, sigma) is the normally-distributed logit term
#' e.g. for PARAM = 1 / (1 + EXP(-(THETA1 + ETA1)))
#'
#' @param .mean mean of the logit term (THETA1 in the example)
#' @param .var  variance of the logit term (OMEGA(1,1) in the example)
#' @param .a  additive term
#' @param .b  proportional term
getSD_logitO <- function(.mean, .var, .a = 0, .b = 1) {
  # cat("Mean", .mean, " Var: ", .var, "\n")
  sdList = NA
  for (i in 1:length(.mean)) {
    m = .mean[i]
    v = .var[i]
    if (is.na(m) | is.na(v)) { 
      sd = NA_real_ } else {
        moments <- logitnorm::momentsLogitnorm(mu = m, sigma = sqrt(v))
        sd <- sqrt(moments[["var"]])
        # cv <- .b * sqrt(moments[["var"]]) / (.b * abs(moments[["mean"]]) + .a) * 100
      }
    sdList = c(sdList, sd)
  }
  return(sdList[-1])
}

# Confidence intervals
lowerCI <- function(est, se) est - 1.96*se
upperCI <- function(est, se) est + 1.96*se
parensSQ <- function(x) paste0('[',x,']')

# Greek number helper functions
mathMode <- function(.x) glue::glue("$<<.x>>$", .open = "<<", .close  = ">>")
gtGreek <- function(.x) glue::glue("\\<<.x>>", .open = "<<", .close  = ">>")
greekNum <- function(.x, .y) glue::glue("<<.x>>_{<<.y>>}", .open = "<<", .close  = ">>")
expGreek  <- function(.x, .y) glue::glue("$\\exp(\\<<.x>>_{<<.y>>})$", .open = "<<", .close  = ">>")
logitGreek  <- function(.x, .y) glue::glue("$\\exp(\\<<.x>>_{<<.y>>}) / \\newline(1 + \\exp(\\<<.x>>_{<<.y>>}))$", .open = "<<", .close  = ">>")


## check whether "~" is used to signify the associated THETA
checkTransforms <- function(df){
  df$transTHETA = NA
  if(any(str_detect(df$trans, "~"))){
    # if there is a '~' in df$trans, replace NA with the value
    # e.g. logitOmSD ~ THETA1 puts "THETA1" in transTHETA
    df$transTHETA[which(str_detect(df$trans, "~"))] = 
      stringr::str_split(df$trans, fixed("~")) %>% map(trimws) %>% map(2) %>% unlist
    
    # Then, remove everything after the "~" in the trans column and 
    # replace THETAx in transTHETA with corresponding estimate
    df = df %>% 
      mutate(trans = case_when(str_detect(trans, "~") ~  
                                 stringr::str_split(trans, fixed("~")) %>% map(trimws) %>% map(1) %>% unlist,
                               TRUE ~ trans),
             ## second, replace THETA with corresponding estimate
             transTHETA = estimate[match(transTHETA, parameter_names)]
      )
  }
  return(df)
}

## Define a series of true/false columns to make filter easier later
defineRows <- function(df){
  df %>% 
    mutate(
      TH = stringr::str_detect(name, "TH"),
      OM = stringr::str_detect(name, "OM"),
      S = stringr::str_detect(name, "S"),
      LOG = (trans=="logTrans"),
      LOGIT = (trans=="logitTrans"),
      lognormO = (trans=="lognormalOm"),
      Osd = (trans=="OmSD"),
      logitOsd = (trans=="logitOmSD"),
      propErr = (trans=="propErr"),
      addErr = (trans=="addErr")
    )
}

## calculate 95% confidence intervals
get95CI <- function(df){
  df %>%      
    mutate(lower = lowerCI(value, se), 
           upper = upperCI(value, se))
}

## calculate % RSE - not used but included if needed 
# Note, this is appropriate when parameters are estimated untransformed or in the log 
# it may not be appropriate if any other transformations (such as logit) were used
getpRSE <- function(df){
    df %>%
      mutate(pRSE = case_when(fixed ~ "-",
                              # pRSE of a log-trans TH is equivalent to the CV% of a log-trans TH
                              TH & LOG ~ sig  (sqrt(exp(se^2)-1)*100), 
                              TH & !LOG & !LOGIT ~ sig ((se/abs(value)) * 100),
                              diag & !LOG & !LOGIT ~ sig ((se/abs(value)) * 100),
                              TRUE ~ "-"))
}

## Back transform parameters estimated in the log domain
# make sure any other calculations, such as CI (and pRSE) are 
# done before back-calculating these values
backTrans_log <- function(df){
  df %>% 
    mutate(value = case_when(LOG ~ exp(value), TRUE ~ value),
           lower = case_when(LOG ~ exp(lower), TRUE ~ lower),
           upper = case_when(LOG ~ exp(upper), TRUE ~ upper))
}
backTrans_logit <- function(df){
  df %>% 
    mutate(value = case_when(LOGIT ~ exp(value)/(1+exp(value)), TRUE ~ value),
           lower = case_when(LOGIT ~ exp(lower)/(1+exp(lower)), TRUE ~ lower),
           upper = case_when(LOGIT ~ exp(upper)/(1+exp(upper)), TRUE ~ upper))
}
## Calculate CV%
getpCV <- function(df){
  df %>% 
    mutate(cv = case_when(diag & OM & lognormO ~ sig(getCV_lognormO(value)), 
                          #diag & OM & logitOsd ~ sig(getSD_logitO(.mean=transTHETA, .var = value)),
                          diag & S & propErr ~ sig(getCV_propS(value)), 
                          TRUE ~ "-"))
}

# value should have estimate [something]
#   theta = estimate only                           # use estimate column
#   omega diagonals = variance [%CV]                # estimate [CV from estimate, stderr]
#   omega off-diagonals = covariance [corr coeff]   # estimate [random_effect_sd]
#   sigma diagonal proportional = variance [%CV]    # estimate [CV from estimate, stderr]
#   sigma diagonal additive = variance [SD]         # estimate [random_effect_sd]
getValueSE <- function(df){
  df %>% 
    mutate(value = estimate, 
           se = stderr,
           corr_SD = case_when(OM & !diag |
                                 S & diag & addErr ~ sig(random_effect_sd), 
                               TRUE ~ "-")
    )
}


# 95% CI should show lower, upper or FIXED
# rounding for display in report
# define what is in estimate column and what is in square brackets 
formatValues <- function(df){
  df %>% 
    # back transform any parameters here
    backTrans_log() %>%   # back transform from log domain
    backTrans_logit() %>% 
    getpCV() %>%          # get % CV 
    # format the values for the final table
    mutate(ci = paste0(sig(lower), ', ', sig(upper)), 
           ci = if_else(fixed, "FIXED", ci),
           # get sd if needed 
           sd = case_when(diag & OM & Osd ~ sig(random_effect_sd),
                          diag & OM & logitOsd ~ sig(getSD_logitO(.mean=transTHETA, .var = value)),
                          TRUE ~ "-"
           ),
           # round values for report table
           value = sig(value), 
           # define which values appear where
           value = case_when(diag & OM & Osd |
                               diag & OM & logitOsd ~ glue::glue("{value} {parensSQ_se(sd)}"),
                             
                             diag & OM | 
                               diag & S & propErr ~ 
                               glue::glue("{value} {parensSQ_CV(cv)}"),
                             !diag & OM ~ glue::glue("{value} {parensSQ_corr(corr_SD)}"),
                             diag & S & addErr ~ glue::glue("{value} {parensSQ_se(corr_SD)}"),
                             !diag & S ~ glue::glue("{value} {parensSQ_corr(corr_SD)}"),
                             TRUE ~ value),
           # round shrinkage values for report table
           shrinkage = case_when(is.na(shrinkage) ~ "-", 
                                 TRUE ~ sig(shrinkage)))
  
}

## Format the THETA/OMEGA/SIGMA values to display as greek letters with 
# subscript numbers
formatGreekNames <- function(df){
  df %>% 
    mutate(greekName = name) %>% 
    # make column with greek letters and parameter numbers
    separate(greekName,
             into = c("text", "num"),
             sep = "(?<=[A-Za-z])(?=[0-9])"
    ) %>% 
    separate(parameter_names,
             into = c("text2", "num2"),
             sep = "A"
    ) %>% 
    select(-num, -text2) %>% 
    mutate(text = case_when(OM ~ "Omega", 
                            S ~ "Sigma",
                            TRUE ~ tolower(text)),
           greek = case_when(TH & LOG ~ expGreek(text, num2),
                             TH & LOGIT ~ logitGreek(text, num2),
                             TRUE ~ mathMode(greekNum(gtGreek(text), num2))
           )
    )
}


## Define which parameters should appear under which panel name in the final table
getPanelName = function(df){
  df %>% 
    mutate(type = case_when(panel=="RV" ~ "Residual variance",
                            OM & !diag ~ "Interindividual covariance parameters",
                            OM & diag & panel=="IIV" ~ "Interindividual variance parameters",
                            # IOV not used here but included for convenience 
                            OM & diag & panel=="IOV"  ~ "Interoccasion variance parameters",  
                            panel=="cov" ~ "Covariate effect parameters",
                            panel=="struct" ~ "Structural model parameters"),
           # Make type a factor and use to sort, this ensures all parameters 
           # of the same type are together - needed to make sure panels pull out 
           # correct rows
           type_f = case_when(panel=="RV" ~ 6,
                              OM & !diag ~ 5,
                              OM & diag & panel=="IIV" ~ 3,
                              # IOV not used here but included for convenience 
                              OM & diag & panel=="IOV"  ~ 4,  
                              panel=="cov" ~ 2,
                              panel=="struct" ~ 1)
    ) %>% 
    arrange(type_f)
}

# Format values for bootstrap run
formatValues_boot <- function(df){
  df %>% 
    mutate(boot_ci = paste0(sig(lower), ', ', sig(upper)), 
           boot_ci = if_else(fixed, "FIXED", boot_ci),
           boot_value = sig(value))
}


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





