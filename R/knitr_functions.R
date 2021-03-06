## Adopted from https://github.com/ramnathv/htmlwidgets/blob/master/R/knitr-methods.R
## The htmlwidgets code had the following COPYRIGHT file
## ## YEAR: 2016
## ## COPYRIGHT HOLDER: Ramnath Vaidyanathan, Joe Cheng, JJ Allaire, Yihui Xie, and Kenton Russell
## End of the htmlwidgets code COPYRIGHT file copy

## This code has been modified from it original htmlwidgets version by Andrey Tovchigrechko, 2016

# Reusable function for registering a set of methods with S3 manually. The
# methods argument is a list of character vectors, each of which has the form
# c(package, genname, class).
registerMethods <- function(methods) {
  lapply(methods, function(method) {
    pkg <- method[[1]]
    generic <- method[[2]]
    class <- method[[3]]
    func <- get(paste(generic, class, sep="."))
    if (pkg %in% loadedNamespaces()) {
      registerS3method(generic, class, func, envir = asNamespace(pkg))
    }
    setHook(
      packageEvent(pkg, "onLoad"),
      function(...) {
        registerS3method(generic, class, func, envir = asNamespace(pkg))
      }
    )
  })
}

.onLoad <- function(...) {
  # Our package provides methods for knitr::knit_print, but knitr isn't a Depends or
  # Imports of our package, only an Enhances. Therefore, the NAMESPACE file has to
  # declare it as an export, not an S3method. That means that R will only know to
  # use our methods if our package is actually attached, i.e., you have to use
  # library(ourpackage) in a knitr document or else you'll get escaped HTML in your
  # document. This code snippet manually registers our method(s) with S3 once both
  # our package and knitr are loaded.
  registerMethods(list(
    # c(package, genname, class)
    c("knitr", "knit_print", "anrep")
  ))
}

#.onAttach <- function(...) {
#  # warn if the version of shiny is lower than what was specified in DESCRIPTION
#  checkShinyVersion(error = FALSE)
#}

knit_print.anrep <- function(x, ..., options = NULL) {
  knitr::knit_print(x$save(out.formats="knitr"), options = options,  ...)
}
