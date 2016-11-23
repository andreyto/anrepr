context("report")

run_in_sandbox_dir <- function(expr,dir_name,cleanup=TRUE,make.unique.dir=TRUE) {

  if(missing(dir_name)) dir_name = file.path(getwd(),"tests_run")

  if(make.unique.dir) {
    dir_name = sprintf("%s_%s",dir_name,ceiling(runif(1, 0, 10^18)))
  }

  cwd = getwd()

  on.exit({
    setwd(cwd)
    if(cleanup) unlink(dir_name,recursive=TRUE,force=TRUE)
  })

  unlink(dir_name, recursive=TRUE,force=TRUE)
  dir.create(dir_name, showWarnings=FALSE, recursive=TRUE)
  setwd(dir_name)
  eval(expr,parent.frame())

}

cleanup = F

#set_default_external_options()

test_that("Sections work", {
  run_in_sandbox_dir(cleanup = cleanup,{

  report <- anrep$new()

  get.anrep.section.3<-function() {
    print("get.anrep.section.3")
    #print(incr.anrep.section())
    anrep.section = report$add.header("get.anrep.section.3",section.action="push",sub=T)
    report$add.descr("Plots are shown with relation to various combinations of meta
                     data variables and in different graphical representations. Lots of plots here.")

    #report$add.header("Iterating over all combinations of grouping variables")

    anrep.section = report$add.header("get.anrep.section.3.1")
    report$add.table(data.frame(A=c("a","b"),B=c(1,2)),caption="Table")
    report$add.descr(paste("File name with extra output is ",report$make.file.name("data.csv")))
    return (anrep.section)
  }

  get.anrep.section.2<-function() {
    print("get.anrep.section.2")
    #anrep.section = incr.anrep.section()
    print(get.anrep.section())
    anrep.section = report$add.header("get.anrep.section.2",section.action="push")
    get.anrep.section.3()
    #anrep.section = report$pop.section()
    get.anrep.section.3()
    anrep.section = report$pop.section()
    get.anrep.section.3()
  }

  get.anrep.section.1<-function() {
    print("get.anrep.section.1")
    anrep.section = report$get.section()
    report$add.header("get.anrep.section.1")
    #anrep.section = incr.anrep.section()
    #print(anrep.section)
    print(get.anrep.section())
    report$add.header("get.anrep.section.1")
    report$add(plot(x <- sort(rnorm(47))),caption="Figure")
    get.anrep.section.2()
    #anrep.section = push.anrep.section()
    print(get.anrep.section())
    report$add.header("get.anrep.section.4")
  }

  get.anrep.section.1()

  report$save("test_sections")
  })
})

test_that("Tables work", {
  run_in_sandbox_dir(cleanup = cleanup,{

  report <- anrep$new()

  y = data.frame(a=c(1,2),b=c(2,4),c=c("zzz","mmmm"))

  report$add.table(y,caption="Test table 1",style="multiline")

  y = data.frame(a=c(1))

  report$add.table(y,caption="Test table 2",style="grid")

  report$add.table(matrix(1:100,nrow = 5),caption="Test table 3, default style")

  x = c(x="_a_",y="b",z="c")

  report$add.vector(x,name="Clade",caption="Test vector 1",
                    show.row.names=T,wrap.vals=F,style="grid")
  report$add.vector(x,name="Clade",caption="Test vector 1",
                    show.row.names=T,wrap.vals=T,style="grid")
  report$add.vector(x,name="Clade",caption="Test vector 1",
                    show.row.names=F,wrap.vals=T,style="grid")

  report$save("test_tables")

})

})

