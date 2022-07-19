
beginItemized <- function() return("\\begin{itemize}")
endItemized   <- function() return("\\end{itemize}")
item          <- function() return("\\item")
bold          <- function(.x) glue("\\textbf{<<.x>>}", .open = "<<", .close  = ">>")
contents      <- function(.x) paste0(names(.x), ": ", .x)

perStudy = function(..., out_dir){
  row_list = list(...)
  
  # grab file extension from first element and then remove it from the list
  fileEx = row_list[1]
  row_list = row_list[-1]
  
  # First element is the upper level of the list
  out = c(beginItemized(),
          item(), bold(contents(row_list[1])),
          beginItemized())
  
  # make second level of bullets
  for(rr in 2:length(row_list)){
    if(!is.na(row_list[rr])) {
      out = c(out, item(), contents(row_list[rr]))
    }
  }
  
  # end both itemized lists
  out = c(out, endItemized(),
          endItemized()
  )
  
  # save the information out as characters
  out %>%
    as.character() %>%
    writeLines(con = file.path(out_dir, glue("text-study-sum-{fileEx}.tex")))
}


