local({
  
  r_version <- "4.3"
  
  correct_r <- grepl(
    paste0("R version ", r_version),
    R.version[["version.string"]], 
    fixed = TRUE
  )
  
  if (!correct_r) {
    stop(paste0("This project only works with R ", r_version), call. = FALSE)
  }
  
  options(
    
    # set some bbr opinionated defaults, these won't impact users who don't use bbr
    'bbr.bbi_exe_path' = file.path(getwd(), "bin", "bbi"),
    
    # do not check that the project library is synced with lockfile on load
    renv.config.synchronized.check = FALSE
  )
  
  source("renv/activate.R")
  
  # Attempt to set options(repos) from pkgr.yml "Repos" section.
  # This is primarily to ensure that creating an renv.lock file
  # with renv::snapshot() will point to the same repos as pkgr.
  pkgr_list <- tryCatch(
    suppressWarnings(yaml::read_yaml("pkgr.yml")),
    error = identity
  )
  
  if (inherits(pkgr_list, "error")) {
    
    warning(
      "Extracting repos from pkgr.yml failed: ", conditionMessage(pkgr_list),
      call. = FALSE
    )
    
  } else {
    
    # if Repos is empty, repos_to_set will be "NULL"
    repos_to_set <- unlist(pkgr_list$Repos)
    
    if (!is.null(repos_to_set)) {
      
      # a value must be set to CRAN or R will complain
      # point to MPN if it's there, else grab the first one (it's arbitrary)
      if (!("CRAN" %in% names(repos_to_set))) {
        repos_to_set[["CRAN"]] <- 
          if ("MPN" %in% names(repos_to_set)) {
            repos_to_set[["MPN"]]
          } else {
            repos_to_set[[1]]
          }
      }
      
      options(repos = repos_to_set)
      
    } else {
      
      warning(
        "No repos found in pkgr.yml",
        call. = FALSE
      )
      
    }
  }
  
  if (interactive()) {
    
    repos <- getOption('repos')
    
    # library info ------------------------------------------------------------
    message(
      "repos set to: \n\t",
      paste(names(repos), repos, sep = ": ", collapse = "\n\t")
    )
    
    message("library paths set to: \n\t", paste0(.libPaths(), collapse = "\n\t"))
    
    # bbr warnings ------------------------------------------------------------
    bbi_path <- getOption('bbr.bbi_exe_path')
    
    if (!file.exists(bbi_path)) {
      message("") # Line break
      warning(
        sprintf("bbi not found at path `%s` ", bbi_path),
        "either run `bbr::use_bbi()` to install bbi or\n",
        "check what you entered in the `bbr.bbi_exe_path` option and make sure it is correct.",
        call. = FALSE
      )
    }
    
  }
  
  # render all mrggsave call as png as well for hashing/testing purposes
  options("mrggsave.dev" = "pdf,png")
  
})
