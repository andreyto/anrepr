make.global <- function(var=NULL,name=NULL,except=c("anrep.section")) {
  if(is.null(var)) {
    if(is.null(name)) {
      name="global"
    }
    p.e = parent.frame()
    if(name=="global") {
      t.e = globalenv()
      for(n in ls(p.e, all.names=TRUE)) {
        if(! (n %in% except) ) assign(n, get(n, p.e), t.e)
      }
      return()
    }
    else {
      var = as.environment(as.list(p.e, all.names=TRUE))
    }
  }
  if(is.null(name)) {
    name = deparse(substitute(var))
  }
  assign(name,var,envir=globalenv())
}


are.automatic.rownames <- function(df) {
  all(rownames(df) == paste(seq(nrow(df))))
}

tempfile.unix <- function(...) {
  x = tempfile(...)
  gsub("\\","/",x,fixed=T)
}

html.path <- function(...) {
  paste(...,sep="/")
}

first_defined_arg <- function(...) {
  x = list(...)
  if(length(x)==0) return (NULL)
  ind = which(!(is.na(x) | sapply(x,is.null)))
  if(length(ind)==0) return (NULL)
  x[[ind[[1]]]]
}

#' Write citations for a vector of package names into file in BibTex format
#' TODO can just use bibtex::write.bib
#' TODO after writing (or before if BibTex allows that type), optionally replace
#' Manual type with TechReport that Zotero understands in BibTex (converts to Report).
#' Otherwise Zotero
#' replaces Manual with Book. The replacement parameter should be a list of
#' to:from tuples. In the path BibTex -> Zotero -> RIS -> Endnote Web Page
#' gets converted to Journal Article still. It seems that in certain styles in
#' EndNote (ACS), the only way too show URL is to set type to Web Page. Otherwise
#' it is not clear at all that packages are CRAN packages.
citation.to.file <- function(package,file.name,append=F,...) {
  cit = unlist(sapply(package,function(p) toBibtex(citation(p,...))))
  write(cit,file.name,append=append)
}

#' Adopted from phyloseq code
#' Computes text size of axis label based on the number of
#' labels.
#' Maybe R strwidth can be used even with ggplot2?
calc.text.size <- function(n, mins=0.5, maxs=4, B=6, D=100){
  # empirically selected size-value calculator.
  s <- B * exp(-n/D)
  # enforce a floor.
  s <- ifelse(s > mins, s, mins)
  # enforce a max
  s <- ifelse(s < maxs, s, maxs)
  return(s)
}

new.section.path.el <- function(num=0,sub=F,has.header=F) {
  ret = list(num=num,sub=sub,has.header=has.header)
  class(ret) <- append(class(ret), "section.path.el", 0)
  ret
}

new.section.path <- function(num=0,sub=F,has.header=F) {
  ret = list(new.section.path.el(num=num,sub=sub,has.header=has.header))
  ret
}

incr.section.path <- function(x,has.header=NULL) {
  last = length(x)
  x[[last]]$num = x[[last]]$num + 1
  if(!is.null(has.header)) x[[last]]$has.header = has.header
  x
}

push.section.path <- function(x,sub=F,has.header=F) {
  x[[length(x)+1]] = new.section.path.el(sub=sub,has.header=has.header)
  x
}

pop.section.path <- function(x) {
  if(length(x)==0) stop("Attempting to pop an element from empty sequence")
  x[0:(length(x)-1)]
}

extract.path.nums.section.path <- function(x) {
  sapply(x,function(y) y$num)
}

extract.path.subs.section.path <- function(x) {
  sapply(x,function(y) y$sub)
}

get.sub.level.section.path <- function(x) {
  sum(extract.path.subs.section.path(x))
}

cut.to.bottom.sub.section.path <- function(x) {
  subs = extract.path.subs.section.path(x)
  pos = length(subs) - match(T,rev(subs)) + 1
  if(is.na(pos)) {
    return (list())
  }
  else {
    if(pos>1)
      return (x[1:pos-1])
    else
      return (list())
  }
}

format.section.path<-function(x) {
  stopifnot(!is.null(x))
  num = extract.path.nums.section.path(x)
  #Using code block quotes here - otherwise just ( makes it a link in Markdown,
  #and \( is MathJax inline opening bracket in the rendered HTML e.g. in knitr
  return (paste("`(",paste(num,sep="",collapse="."),")`",sep=""))
}

format.section.path.as.file<-function(x) {
  num = extract.path.nums.section.path(x)
  return (paste(num,sep="",collapse="."))
}

# Converts objects of several kinds into an environment (possibly by reference).  Copied from package pryr.
to_env <- function(x, quiet = FALSE) {
  if (is.environment(x)) {
    x
  } else if (is.list(x)) {
    list2env(x)
  } else if (is.function(x)) {
    environment(x)
  } else if (length(x) == 1 && is.character(x)) {
    if (!quiet)
      message("Using environment ", x)
    as.environment(x)
  } else if (length(x) == 1 && is.numeric(x) && x > 0) {
    if (!quiet)
      message("Using environment ", search()[x])
    as.environment(x)
  } else {
    stop("Input can not be coerced to an environment", call. = FALSE)
  }
}

# Test if all elements of x are named. Copied from package pryr.
all_named <- function(x) {
  if (length(x) == 0)
    return(TRUE)
  !is.null(names(x)) && all(names(x) != "")
}

# Constructs a function object from a body expression and a pairlist of arguments. Copied from the package
# pryr.  Examples: make_function(alist(x =), x**2, env) will create (function(x) x**2) and environment() of
# this function will be env.  make_function(alist(), x**2, env) will create (function() x**2), so that when
# the function is called, the value of x will be search in env and up the enclosing chain of environments.
make_function <- function(args, body, env = parent.frame()) {
  args <- as.pairlist(args)
  # stopifnot( all_named(args), is.language(body))
  env <- to_env(env)

  eval(call("function", args, body), env)
}

dummy <- function() {

}
