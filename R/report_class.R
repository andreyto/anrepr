#' @importFrom methods setRefClass new
anrep <- setRefClass('anrep',
                     fields = list(
                       'author' = 'character',
                       'title' = 'character',
                       'date' = 'character',
                       'entries' = 'list',
                       'sections' = 'list',
                       'incremental.save' = 'logical',
                       'out.file' = 'character',
                       'out.formats' = 'character',
                       'portable.html' = 'logical',
                       'object.index' = 'list',
                       'data.dir' = 'character',
                       'widget.dir' = 'character',
                       'widget.deps.dir' = 'character'
                     )
)

anrep$methods(initialize = function(
  author = "Anonymous",
  title = "Analysis",
  date = base::date(),
  out.file = "report",
  out.formats = c("html"),
  incremental.save = F,
  portable.html=T,
  ...
) {

  .self$author=author
  .self$title=title
  .self$date=date
  .self$out.file=out.file
  .self$out.formats=out.formats
  .self$incremental.save=incremental.save
  .self$portable.html=portable.html
  .self$object.index=list(table=1,figure=1)
  .self$data.dir = "data"

  unlink(.self$data.dir,recursive=TRUE,force=TRUE)
  dir.create(.self$data.dir, showWarnings = FALSE, recursive = TRUE)

  graph.dir = pander::evalsOptions("graph.dir")
  unlink(graph.dir,recursive=TRUE,force=TRUE)
  dir.create(graph.dir, showWarnings = FALSE, recursive = TRUE)

  .self$widget.dir = "." #normalizePath(graph.dir,winslash = "/")
  .self$widget.deps.dir = "widget_deps" #html.path(.self$widget.dir,"widget_deps")
  dir.create(.self$widget.deps.dir, showWarnings = FALSE, recursive = TRUE)

  callSuper(...)

})

## private service method - should be called whenever an element is
## appended to the .self$entries
anrep$methods(priv.append.section = function() {
  incr.anrep.section.if.zero()
  .self$sections[[length(.self$sections)+1]] = get.anrep.section()$path
})

## private service method - should be called whenever an element
## that needs its own index in the report is
## appended to the .self$entries
anrep$methods(priv.append.index = function(type) {
  val = .self$object.index[[type]]
  if(is.null(val)) {
    val = .self$object.index[[type]] = 1
  }
  .self$object.index[[type]] = val + 1
  return(val)
})

anrep$methods(add.paragraph = function(x) {
  .self$entries = c(.self$entries, list(
    list(result = pander::pandoc.p.return(x)))
  )
})

anrep$methods(format.caption = function(caption,section=NULL,type=NULL) {
  if(!is.null(caption)) {
    if(is.null(type)) {
      type = ""
      ind = ""
    }
    else {
      ind = .self$priv.append.index(type)
    }
    if(nzchar(ind)) {
      anchor = sprintf('%s.%s',type,ind)
      anchor.name = sprintf("%s %s.",Hmisc::capitalize(type),ind)
      name = anrep.anchor.return(anchor,anchor.name)
    }
    else {
      name = ""
    }
    caption = paste(format.anrep.section(section),name,caption)
    if(substr(caption,nchar(caption),nchar(caption)+1)!=".") {
      caption = paste(caption,".",sep="")
    }
  }

  return (caption)
})


anrep$methods(add.widget = function(x,new.paragraph=T,
                                    caption=NULL,
                                    show.image.links=T,
                                    width = 800,
                                    height = 800,
                                    data.export = NULL,
                                    data.export.descr = NULL,
                                    show.inline = T,
                                    ...) {

  if(!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("This method requires package htmlwidgets.")
  }

  if(new.paragraph) {
    .self$add.p("")
  }

  name.base=paste(anrep.str_to_file_name(caption,20),".html",sep="")

  fn = .self$make.file.name(name.base,dir=.self$widget.dir,make.unique=T)

  htmlwidgets::saveWidget(x,fn,selfcontained = F,libdir=.self$widget.deps.dir)

  caption.res = sprintf("Click to see HTML widget file in full window: %s",
                        anrep.link.verbatim.return(fn))

  caption.type = "widget"

  if(!is.null(caption)) {
    caption = .self$format.caption(caption,type=caption.type)
  }

  caption = paste(caption,caption.res)

  if(!is.null(data.export)) {
    caption = sprintf("%s. Dataset is also saved here: %s",
                      caption,
                      anrep.link.verbatim.return(data.export))
    if(!is.null(data.export.descr)) {
      caption = sprintf("%s, %s",caption,data.export.descr)
    }
  }

  if(!is.null(caption)) {
    .self$add.p(caption)
  }
  if(show.inline) {
    if(is.null(width)) {
      width = pander::evalsOptions("width")
    }

    if(is.null(height)) {
      height = pander::evalsOptions("height")
    }
    iframe.tpl = '<iframe style="max-width=100%"
        src="fn"
        sandbox="allow-same-origin allow-scripts"
        width="100%"
        height="%s"
        scrolling="no"
        seamless="seamless"
        frameBorder="0"></iframe>'
    iframe.tpl = '<iframe src="%s" width="%s" height="%s"> </iframe>'
    report$add(sprintf(iframe.tpl,
                       fn,
                       width,
                       height))
  }
  return(x)
})


anrep$methods(add = function(x,new.paragraph=T,
                             caption=NULL,
                             show.image.links=T,
                             caption.type=NULL,
                             graph.output = pander::evalsOptions("graph.output"),
                             hi.res = pander::evalsOptions("hi.res"),
                             ...) {

  par_fr = parent.frame()
  if(!identical(.GlobalEnv, par_fr)) {
    env = list2env(as.list(par_fr, all.names=TRUE),parent=parent.frame(2))
  }
  else {
    env = NULL
  }
  ## work around pander bug in v.0.6.0 where hi.res is created as a broken symlink plots/normal.res
  ## instead of just normal.res
  if(graph.output == 'svg') {
    hi.res = F
  }
  res = pander::evals(deparse(match.call()[[2]]),env=env,
                      graph.output = graph.output,
                      hi.res = hi.res,
                      ...)
  if(new.paragraph) {
    .self$add.p("")
  }

  is.image = F
  caption.res = ""
  for (r in res) {
    if(any(r$type=="image")) {
      if(show.image.links) {
        rr = r$result
        caption.res = paste(caption.res,
                            sprintf("Image file: %s.",
                                    anrep.link.verbatim.return(as.character(rr)))
        )
        hres.ref = attr(rr,"href")
        if(!is.null(hres.ref)) {
          caption.res = paste(caption.res,
                              sprintf("High resolution image file: %s.",
                                      anrep.link.verbatim.return(hres.ref))
          )
        }
      }
      is.image = T
    }
  }
  if(is.null(caption.type)) {
    if(is.image) {
      caption.type = "figure"
    }
  }
  if(!is.null(caption)) {
    caption = .self$format.caption(caption,type=caption.type)
  }
  if(nzchar(caption.res)) {
    caption = paste(caption,caption.res)
  }
  if(!is.null(caption)) {
    .self$add.p(caption)
  }
  .self$entries = c(.self$entries,res)
  .self$priv.append.section()
  return(res)
})

anrep$methods(add.list = function(x,...) {
  return(.self$add(as.list(x),...))
})

anrep$methods(reset.section = function(...) {
  return(NULL)
})

anrep$methods(default.section = function(...) {
  return(get.default.section(...))
})

anrep$methods(get.section = function(...) {
  return(get.anrep.section(...))
})

anrep$methods(incr.section = function(...) {
  return(incr.anrep.section(...))
})

anrep$methods(push.section = function(...) {
  return(push.anrep.section(...))
})

anrep$methods(pop.section = function(...) {
  return(pop.anrep.section(...))
})

anrep$methods(add.file = function(x,
                                  caption=NULL,
                                  wrap.caption=T,
                                  skip.if.empty=F,
                                  ...) {
  if (wrap.caption && !is.null(caption)) {
    caption = anrep.escape.special(caption)
  }

  caption = .self$format.caption(caption,type="dataset")

  if(is.null(x)) {
    if(!skip.if.empty) {
      if(!is.null(caption)) {
        .self$add.p(caption)
      }
      return(.self$add.p("Empty dataset"))
    }
    else {
      return(.self)
    }
  }
  caption = paste(caption,
                  "Dataset is saved in a file (click to download)",
                  anrep.link.verbatim.return(x)
  )

  return(.self$add.p(caption))
})


anrep$methods(add.table = function(x,
                                   show.row.names=is.matrix(x),
                                   echo=T,
                                   caption=NULL,
                                   wrap.vals=T,
                                   wrap.caption=T,
                                   split.tables=Inf,
                                   style="rmarkdown",
                                   export.to.file=T,
                                   file.name=NULL,
                                   show.first.rows=200,
                                   show.first.cols=200,
                                   skip.if.empty=F,
                                   ...) {
  if (wrap.caption && !is.null(caption)) {
    caption = anrep.escape.special(caption)
  }

  caption = .self$format.caption(caption,type="table")

  if(is.null(x) || nrow(x)==0) {
    if(!skip.if.empty) {
      if(!is.null(caption)) {
        .self$add.p(caption)
      }
      return(.self$add.p("Empty dataset"))
    }
    else {
      return(.self)
    }
  }
  if(show.first.rows > 0) {
    if(show.first.rows >= nrow(x)) {
      show.first.rows = 0
    }
  }
  if(show.first.rows > 0) {
    caption = paste(caption,sprintf("Showing only %s first rows.",show.first.rows))
  }
  if(show.first.cols > 0) {
    if(show.first.cols >= ncol(x)) {
      show.first.cols = 0
    }
  }
  if(show.first.cols > 0) {
    caption = paste(caption,sprintf("Showing only %s first columns.",show.first.cols))
  }

  if(export.to.file) {
    file.name = .self$write.table.file(x,
                                       name.base=paste(anrep.str_to_file_name(caption,20),".csv",sep=""),
                                       descr=NULL,
                                       row.names=show.row.names,
                                       row.names.header=T,
                                       file.name=file.name)
    caption = paste(caption,
                    "Full dataset is also saved in a delimited text file (click to download and open e.g. in Excel)",
                    pander::pandoc.link.return(file.name,pander::pandoc.verbatim.return(file.name))
    )
  }

  if(show.first.rows > 0) {
    if(inherits(x,"data.table")) x = x[1:show.first.rows]
    else x = x[1:show.first.rows,,drop=F]
  }
  if(show.first.cols > 0) {
    if(inherits(x,"data.table")) x = x[,1:show.first.cols,with=F]
    else x = x[,1:show.first.cols,drop=F]
  }

  ## With data.table, I am getting this message:
  ## `data.table inherits from data.frame (from v1.5) but this data.table does not`
  ## when calling `rn = rownames()` below. Converting to data.frame here to get rid of it.

  if(inherits(x,"data.table")) x = as.data.frame(x)

  if(!show.row.names) {
    rownames(x) = NULL
  }

  if(wrap.vals) {
    rn = rownames(x)
    if(show.row.names && !are.automatic.rownames(x)) {
      rn = anrep.escape.special(rn)
    }
    if(is.matrix(x)) {
      x = as.data.frame(x)
    }
    x = sapply(x,anrep.escape.special,USE.NAMES=F,simplify=T)
    if(!is.matrix(x)) {
      x = t(as.matrix(x))
    }
    rownames(x) = rn
    colnames(x) = anrep.escape.special(colnames(x))
  }

  .self$add.p(caption)
  tbl_p = pander::pandoc.table.return(x,split.tables=split.tables,style=style,caption=NULL,...)
  if(echo) {
    print(tbl_p)
  }
  return(.self$add.p(tbl_p))
})

anrep$methods(add.vector = function(x,name=NULL,
                                    show.row.names=T,
                                    caption=NULL,
                                    ...) {
  if(is.null(x) || length(x)==0) {
    if(!is.null(caption)) {
      .self$add.p(.self$format.caption(caption))
    }
    return(.self$add.p("Empty dataset"))
  }
  y = data.frame(x=x)
  if(!is.null(name)) {
    names(y) = c(name)
  }
  if(is.null(names(x))) {
    show.row.names = F
  }
  if(show.row.names) {
    row.names(y) = names(x)
  }
  return(.self$add.table(y,caption=caption,show.row.names=show.row.names,...))
})

anrep$methods(add.p = function(x,rule=F,echo=T,...) {
  if(rule) {
    .self$priv.append.section()
    .self$add.paragraph(pander::pandoc.horizontal.rule.return())
  }
  if(echo) {
    cat(format.anrep.section(),x,"\n")
  }
  .self$priv.append.section()
  return(.self$add.paragraph(x,...))
})

anrep$methods(add.descr = function(x,...) {
  .self$add.p(x,...)
})

anrep$methods(add.package.citation = function(x,...) {
  .self$add.p(capture.output(print(citation(x),style="text")))
})

anrep$methods(add.printed = function(x,caption=NULL,echo=T,...) {
  if(!is.null(caption)) {
    .self$add.p(.self$format.caption(caption))
  }
  return(.self$add.p(anrep.as.printed.return(x,...),echo=echo))
})

anrep$methods(add.header = function(x,level=NULL,anrep.section=NULL,section.action="incr",echo=T,sub=F,...) {
  if(sub) {
    section.action = "push"
  }
  do.clone = is.null(anrep.section)
  anrep.section = switch(section.action,
                          incr=incr.anrep.section(anrep.section),
                          push=incr.anrep.section(anrep.section),
                          keep=get.anrep.section(anrep.section))
  num = extract.path.nums.anrep.section(anrep.section)
  if (is.null(level)) {
    ##headers will shift to the left above level 5 in HTML output
    level = min(5,length(num))
  }
  x = paste(format.anrep.section(anrep.section),x)
  ##newlines currently break header formatting, remove them
  x = gsub("\n"," ",x)
  .self$add.p(pander::pandoc.header.return(x,level=level,...),echo=echo)

  if(section.action=="push") {
    rep.sec.push = NULL
    if(!do.clone) rep.sec.push = anrep.section
    #w/o argument it clones
    anrep.section = push.anrep.section(rep.sec.push,sub=sub,has.header=T)
  }

  if(.self$incremental.save) {
    .self$save()
  }

  return (anrep.section)

})

anrep$methods(make.file.name = function(name.base="",
                                        make.unique=T,
                                        dir=NULL,
                                        section.path=NULL) {
  if(is.null(dir)) {
    dir = .self$data.dir
  }
  if(length(name.base)==0) {
    name.base = ""
  }
  if(name.base=="" && !make.unique) {
    stop("Need either non-empty name.base or make.unique=T")
  }
  fn.start = format.section.path.as.file(section.path)
  if(make.unique) {
    fn = tempfile.unix(paste(fn.start,"-",sep=""),tmpdir=dir,fileext=name.base)
  }
  else {
    fn = file.path(dir,paste(fn.start,name.base,sep="-"),fsep="/")
  }
  return(fn)
})

## Save data as a delimited text file
## ... are optional arguments to write.table
anrep$methods(write.table.file = function(data,
                                          name.base,
                                          make.unique=T,
                                          descr=NULL,
                                          row.names=F,
                                          row.names.header=T,
                                          file.name=NULL,
                                          ...) {
  ## if we write row.names, Excel shifts header row to the left when loading
  if(row.names && row.names.header) {
    data = cbind(rownames=rownames(data),data)
    row.names=F
  }
  if(is.null(file.name)) {
    fn = .self$make.file.name(name.base,make.unique=make.unique)
  }
  else {
    fn = file.name
  }
  write.csv(data,
            fn,
            row.names = row.names,
            ...)
  if (!is.null(descr)) {
    .self$add.descr(paste("Wrote",descr,"to file",
                          pander::pandoc.link.return(fn,fn)))
  }
  return(fn)
})

anrep$methods(save = function(out.file=NULL,out.formats=NULL,portable.html=NULL,sort.by.sections=F,
                              pandoc.binary=NULL) {

  .out.file = first_defined_arg(out.file,.self$out.file,"report")

  .out.formats = first_defined_arg(out.formats,.self$out.formats,"html")

  .portable.html = first_defined_arg(portable.html,.self$portable.html,T)

  fp    = .out.file

  fp.all = list()

  f_sections = .self$sections
  f_entries = .self$entries

  if(sort.by.sections) {
    ##sort by section lexicographically, using a stable sort
    sect_ord = sort.list(
      unlist(lapply(.self$sections,format.section.path)),
      method="shell")
    f_entries = f_entries[sect_ord]
  }

  write.el = function(el,fp) {
    el.str = pander::pander_return(el$result)
    cat(paste(el.str, collapse = '\n'),
        file = fp, append = TRUE)
  }

  for(i.el in seq_along(f_entries)) {
    section = f_sections[[i.el]]
    el = f_entries[[i.el]]

    section.par = cut.to.bottom.sub.section.path(section)

    #print(paste("Full section:",paste(section,collapse=" ")))
    #print(paste("Par section:",paste(section.par,collapse=" ")))

    if(length(section.par) > 0) {
      sub.path = section.par
    }
    else {
      sub.path = NULL
    }
    fp.sub = make.file.name(name.base=fp,
                            make.unique=F,
                            dir=".",
                            section.path=sub.path)
    fp.sub.md = paste(fp.sub,".Rmd",sep="") #".md"
    #print(paste("fp.sub=",fp.sub))

    if(is.null(fp.all[[fp.sub.md]])) {
      cat(pander::pandoc.title.return(.self$title, .self$author, .self$date), file = fp.sub.md)
    }

    if(i.el>1) {
      sub.level.prev = get.sub.level.section.path(f_sections[[i.el-1]])
      sub.level = get.sub.level.section.path(section)
      if(sub.level > sub.level.prev) {
        cat(anrep.link.verbatim.return(paste(fp.sub,".html",sep=""),"Subreport"), #".html"
            file = fp.sub.md.prev, append = TRUE)
        if(section[[length(section)]]$has.header) {
          write.el(f_entries[[i.el-1]],fp.sub.md)
        }
      }
    }

    write.el(el,fp.sub.md)
    fp.all[[fp.sub.md]] = fp.sub
    fp.sub.md.prev = fp.sub.md
  }

  if(is.null(pandoc.binary)) {
    pandoc.binary = pander::panderOptions("pandoc.binary")
    if(is.null(pandoc.binary) || !file.exists(pandoc.binary)) {
      pandoc.binary = Sys.which("pandoc")
    }
  }
  if(is.null(pandoc.binary) || !file.exists(pandoc.binary)) {
    stop("Exetutable file 'pandoc' must be found in the system PATH or in the location provided by you
         for the conversion from Markdown to other formats to work.")
  }

  for(out.format in .out.formats) {
    for(fp.sub.md in names(fp.all)) {
      fp.sub = fp.all[[fp.sub.md]]
      fp.sub.out = sprintf("%s.%s",fp.sub,out.format)
      ## It would be nice to add `options="-s -S"` to support
      ## Pandoc's subscript and suprscript extensions, but
      ## this will entirely replace internal default options and
      ## break TOC etc
      cat(sprintf("Pandoc converting markdown file %s to %s format\n",fp.sub.md,out.format))
      css_base = "killercup-pandoc.css" #"github-rmarkdown.css" #"github-pandoc.css"
      css = system.file("extdata", css_base, package = "anrepr")
      #css_base = basename(css)
      file.copy(css,css_base)
      cmd = sprintf("pandoc --standalone --self-contained --toc -t %s -c %s %s -o %s",out.format,css_base,fp.sub.md,fp.sub.out)
      cat(cmd)
      system(cmd)
      #pander::Pandoc.convert(fp.sub.md,format=out.format,open=F,footer=FALSE,
      #                       portable.html=.portable.html)
    }
  }

})


