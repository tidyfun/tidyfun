# set up imports and re-declare as documented tidyfun exports: 

reexport <- function(f, package = "tf", file = here::here("R/tf-importexport.R")) {
  #sanitize subassignment and infix function names:
  if(grepl("(<-)|(%)", f)) f <- paste0("`",f,"`")
  
  docstring <- paste0("\n#' @inherit  ", package, "::", f,
         "\n#' @importFrom ", package, " ", f,
         "\n#' @export",
         "\n", f, " <- ", package, "::", f,"\n")
  
  cat(docstring, file = file, append = TRUE)
}

tf_imports <- getNamespaceExports("tf") |>  sort()

tf_docs <- sapply(tf_imports, reexport)  

# this does not work for pkgdown:
# - s3 methods stay hidden (registered, but not exported, so no docs)
# - more complicated docfiles that use rdname etc get amputated 
