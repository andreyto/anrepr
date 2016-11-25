#' Reset various global options to tune up a default style of the reports
#'
#' This overrides various options in the \code{pander} package and changes
#' the default ggplot2 theme.
#' @export
set_default_external_options <- function() {
  message("Overriding default options of the pander package and ggplot2 default theme")
  pander::panderOptions("round",4)
  pander::panderOptions("table.style","rmarkdown") #"grid"
  pander::panderOptions("table.split.table",Inf)
  pander::panderOptions("table.alignment.default","left")
  pander::panderOptions("evals.messages",F)
  pander::panderOptions("graph.fontsize",20)
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

#
#' Return a named list as a string of named function arguments
#'
#' Useful in creating captions where you need to report the parameters
#' with wich some method was called by the user
#' @param x Named list
#' @param collapse Passed to \code{\link[base]{paste}}
#'
#' @export
#'
#' @examples
#' arg_list_as_str(list(a=1,b="bbb"))
#'
arg_list_as_str<-function(x,collapse=",") {
  paste("[",
        paste(capture.output(str(x,no.list=T,comp.str="",give.attr=F,give.head=F)),collapse=collapse),
        "]",
        sep=""
  )
}

#' Return Markdown string with the argument printed verbatim
#'
#' @param x String or object to print
#' @param attrs String with extra Markdown attributes
#' @export
#'
#' @examples
#' anrep.as.printed.return("**Text**")
anrep.as.printed.return <- function(x,attrs="") {
  x = capture.output(print(x))
  paste0('\n', repChar('`', 7),
         ifelse(attrs == '', '', sprintf('{%s}', attrs)),
         '\n', paste(x,
                     collapse = '\n'),
         '\n', repChar('`', 7), '\n')
}

anrep.special.symb = "\\-\\[\\]`*_{}()#+!~"

#' Quote all Markdown special symbols in a string
#'
#' Use this protect Markdown formatting symbols from being interpreted
#' when Markdown is rendered
#' @param x Input string
#'
#' @export
#'
#' @examples
#' anrep.escape.special("_a_")
#' anrep.escape.special("**a**[bbb](ccc)")
#' anrep.escape.special("`a`")
anrep.escape.special <- function(x) {
  x = gsub(paste('([',anrep.special.symb,'])',sep=''),"\\\\\\1",
           format(x,digits=pander::panderOptions("digits")),perl=T)
  ##the only way to have backstick is table cells is to use special symbol
  gsub('[|]','&#124;',x)
}

#' Return Markdown link, escaping the link title
#'
#' @param url Link URL
#' @param text Link title
#'
#' @export
#'
#' @examples
#' anrep.link.verbatim.return("my_link","**link title**")
anrep.link.verbatim.return <- function(url,text=NULL) {
  if(is.null(text)) {
    text = url
  }
  sapply(seq_along(url),function(i) { pander::pandoc.link.return(url[i],pander::pandoc.verbatim.return(text[i]))})
}

#' Return Markdown string with a generated anchor (using HTML tag)
#'
#' @param anchor Name of anchor
#' @param text Title of anchor
#'
#' @export
#'
#' @examples
#' anrep.anchor.return("My_anchor","My anchor title")
anrep.anchor.return <- function(anchor,text) {
  anchor.tag = sprintf('<a name="%s"></a>',anchor)
  ret = sprintf("%s%s",
                anchor.tag,
                anrep.link.verbatim.return(sprintf("#%s",anchor),
                                           text))
  return(ret)
}

#' Convert a string to a (more or less) valid file name
#'
#' @param x String
#' @param max.length Limit to the length of the returned string
#'
#' @export
#'
#' @examples
#' anrep.str_to_file_name("My long $ and ! || && strange string")
anrep.str_to_file_name <- function(x,max.length=0) {
  x = gsub('[^-[:alnum:]._]+','.',x)
  if(max.length>0) {
    x = substring(x,1,max.length)
  }
  x
}

#' Infix operator to wrap a report code block after incrementing section index (Header 1.1 ++ Header 1.2)
#'
#' This is not strictly necessary to use because default \code{anrep$header()}
#' parameters result in incrementing the section index, and no cleanup handling
#' is required to end the section, but using this will make the code's relationship
#' with reporting section more clear, and protect from future implementation changes
#' if they introduce the internal cleanup code
#' @param header_call Call to \code{anrep$header()}
#' @param expr Code section that be reporting under the new subsection
#'
#' @export
#'
#' @examples
#' ## See \code{\link{anrep}} examples
`%anrep++%` <- function(header_call,expr) {
  env = parent.frame()
  header_call = substitute(header_call)
  expr = substitute(expr)
  header_call[["section.action"]] = "incr"
  header_call[["sub"]] = F
  eval(header_call,envir = env)
  invisible(eval(expr, envir = env))
}

#' Infix operator to wrap a report sub-section code block (Header 1.1 >> Subheader 1.1.1)
#'
#' @param header_call Call to \code{anrep$header()}
#' @param expr Code section that be reporting under the new subsection
#'
#' @export
#'
#' @examples
#' ## See \code{\link{anrep}} examples
`%anrep>>%` <- function(header_call,expr) {
  env = parent.frame()
  header_call = substitute(header_call)
  expr = substitute(expr)
  header_call[["section.action"]] = "push"
  header_call[["sub"]] = F
  .report = eval(header_call,envir = env)
  on.exit({invisible(.report$pop.section())})
  ## wrap user code in its own function to protect
  ## our on.exit() handler in case user code uses
  ## on.exit without `add=T`
  make_function(alist(), expr, env = env)()
}

#' Infix operator to wrap a sub-report code block (Header 1.1 // Subreport Header 1.1.1)
#'
#' @param header_call Call to \code{anrep$header()}
#' @param expr Code section that be reporting under the new subsection
#'
#' @export
#'
#' @examples
#' ## See \code{\link{anrep}} examples
`%anrep//%` <- function(header_call,expr) {
  env = parent.frame()
  header_call = substitute(header_call)
  expr = substitute(expr)
  header_call[["section.action"]] = "push"
  header_call[["sub"]] = T
  .report = eval(header_call,envir = env)
  on.exit({invisible(.report$pop.section())})
  ## wrap user code in its own function to protect
  ## our on.exit() handler in case user code uses
  ## on.exit without `add=T`
  make_function(alist(), expr, env = env)()
}
