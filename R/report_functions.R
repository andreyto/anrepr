set_default_external_options <- function() {
  pander::panderOptions("round",4)
  pander::panderOptions("table.style","rmarkdown") #"grid"
  pander::panderOptions("table.split.table",Inf)
  pander::panderOptions("table.alignment.default","left")
  pander::panderOptions("evals.messages",F)
  #pander::panderOptions("graph.fontsize",10)
  pander::evalsOptions("cache",F)
  pander::evalsOptions("cache.mode","environment")
  #pander::evalsOptions("output",c("result"))
  pander::evalsOptions("output",c("all"))
  pander::evalsOptions("graph.output","svg") #"png"
  pander::evalsOptions("graph.unify",F)
  pander::evalsOptions("width",800) #800 #1000
  pander::evalsOptions("height",600) #640 #840
  pander::evalsOptions("res",75)
  pander::evalsOptions("hi.res",T)
  pander::evalsOptions("hi.res.width",1200)
  pander::evalsOptions("graph.env",F)
  pander::evalsOptions("graph.recordplot",F)
  pander::evalsOptions("graph.RDS",F)
  ##using graph.name option causes seemingly unconnected
  ##errors starting with warnings like:
  ##`No pander method for "ggplot", reverting to default`
  ##described in the URL but still not solved apparently
  #https://github.com/Rapporter/rapport/issues/98
  #pander::evalsOptions("graph.name","plot-%d-%n")
  ## This is ggplot2 function that changes
  ## text base size within a current theme for entire session
  if(requireNamespace("ggplot2", quietly = TRUE)) {
    ggplot2::theme_set(ggplot2::theme_gray(base_size = 20))
  }
}

# print a named list as a string of named function arguments
arg_list_as_str<-function(x,collapse=",") {
  paste("[",
        paste(capture.output(str(x,no.list=T,comp.str="",give.attr=F,give.head=F)),collapse=collapse),
        "]",
        sep=""
  )
}

anrep.as.printed.return <- function(x,attrs="") {
  x = capture.output(print(x))
  paste0('\n', repChar('`', 7),
         ifelse(attrs == '', '', sprintf('{%s}', attrs)),
         '\n', paste(x,
                     collapse = '\n'),
         '\n', repChar('`', 7), '\n')
}

anrep.special.symb = "\\-\\[\\]`*_{}()#+!~"

anrep.escape.special <- function(x) {
  x = gsub(paste('([',anrep.special.symb,'])',sep=''),"\\\\\\1",
           format(x,digits=pander::panderOptions("digits")),perl=T)
  ##the only way to have backstick is table cells is to use special symbol
  gsub('[|]','&#124;',x)
}

anrep.link.verbatim.return <- function(url,text=NULL) {
  if(is.null(text)) {
    text = url
  }
  sapply(seq_along(url),function(i) { pander::pandoc.link.return(url[i],pander::pandoc.verbatim.return(text[i]))})
}

anrep.anchor.return <- function(anchor,text) {
  anchor.tag = sprintf('<a name="%s"></a>',anchor)
  ret = sprintf("%s%s",
                anchor.tag,
                anrep.link.verbatim.return(sprintf("#%s",anchor),
                                           text))
  return(ret)
}

## make string x a (more or less) valid file name
anrep.str_to_file_name <- function(x,max.length=0) {
  x = gsub('[^-[:alnum:]._]+','.',x)
  if(max.length>0) {
    x = substring(x,1,max.length)
  }
  x
}
