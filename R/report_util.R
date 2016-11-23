make.global <- function(var) {
  assign(deparse(substitute(var)),var,envir=globalenv())
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

mget.stack<-function(x,ifnotfound) {
  pars = lapply(rev(sys.parents()),sys.frame)
  #print(pars)
  #print(sys.parent())
  #print(sys.frames())
  for(fr in pars) {
    y = tryCatch(get(x,envir=fr,inherits=F),error=function(x) NULL)
    #print(paste("y=",y,"ifnotfound=",ifnotfound))
    if(!is.null(y)) {
      #print(paste("y=",y,"ifnotfound=",ifnotfound))
      return (y)
    }
  }
  return (ifnotfound)
}

new_section_path_el <- function(num=0,sub=F,has.header=F) {
  list(num=num,sub=sub,has.header=has.header)
}

extract.path.nums.section.path <- function(x) {
  sapply(x,function(y) y$num)
}

extract.path.nums.anrep.section <- function(x) {
  extract.path.nums.section.path(x$path)
}

extract.path.subs.section.path <- function(x) {
  sapply(x,function(y) y$sub)
}

extract.path.subs.anrep.section <- function(x) {
  extract.path.subs.section.path(x$path)
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

get.default.section<-function() {
  x = new.env(parent=emptyenv())
  x$path = list(new_section_path_el())
  return (x)
}

clone.anrep.section<-function(x) {
  return (as.environment(as.list(x, all.names=TRUE)))
}

get.anrep.section<-function(default=get.default.section()) {
  y = mget.stack("anrep.section",ifnotfound=0)
  if(!identical(y,0)) return (y)
  return (default)
}

## when x is NULL, this function creates a clone
push.anrep.section<-function(x=NULL,sub=F,has.header=F) {
  if(is.null(x)) {
    x = get.anrep.section(default=NULL)
    if(is.null(x)) {
      x = get.default.section()
    }
    else {
      x = clone.anrep.section(x)
    }
  }
  x$path[[length(x$path)+1]] = new_section_path_el(num=0,sub=sub,has.header=has.header)
  return (x)
}

pop.anrep.section<-function(x=NULL) {
  if(is.null(x)) {
    x = get.anrep.section(default=NULL)
    if(is.null(x)) {
      x = get.default.section()
    }
  }
  l = length(x$path)
  if(l>1)
    x$path = x$path[1:l-1]
  return (x)
}

incr.anrep.section<-function(x=NULL) {
  if(is.null(x)) {
    x = get.anrep.section(default=NULL)
    if(is.null(x)) {
      x = get.default.section()
    }
  }
  last = length(x$path)
  if(last>0) {x$path[[last]]$num = x$path[[last]]$num + 1}
  else {x$path[[1]]$num = 1}
  return (x)
}

incr.anrep.section.if.zero <- function(x=NULL) {
  if(is.null(x)) {
    x = get.anrep.section()
  }
  last = length(x$path)
  if(last>0 && x$path[[last]]$num == 0) {x$path[[last]]$num = x$path[[last]]$num + 1}
  return (x)
}

format.section.path<-function(x=NULL) {
  if(is.null(x)) {
    x = get.anrep.section()$path
  }
  num = extract.path.nums.section.path(x)
  return (paste("\\(",paste(num,sep="",collapse="."),"\\)",sep=""))
}

format.anrep.section<-function(x=NULL) {
  if(is.null(x)) {
    x = get.anrep.section()
  }
  format.section.path(x$path)
}

format.section.path.as.file<-function(x=NULL) {
  if(is.null(x)) {
    x = get.anrep.section()$path
  }
  num = extract.path.nums.section.path(x)
  return (paste(num,sep="",collapse="."))
}

format.anrep.section.as.file<-function(x=NULL) {
  if(is.null(x)) {
    x = get.anrep.section()
  }
  format.section.path.as.file(x$path)
}

