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

    my.func.3<-function(subreports.header) {
      print("my.func.3")
      report$add.header(subreports.header) %anrep//% {
        report$add.header("H2.1.subreport 1") %anrep//% {
          report$add.descr("Some text")
          report$add.header("H2.1.subreport 1 incremented section")
          report$add.table(data.frame(A=c("a","b"),B=c(1,2)),caption="Table")
          report$add.descr(paste("File name with extra output is ",report$make.file.name("data.csv")))
        }
        report$add.header("H2.1.subreport 2") %anrep//% {
          report$add.descr("Some text")
          report$add.header("H2.1.subreport 2 incremented section")
          report$add.table(data.frame(A=c("a","b"),B=c(1,2)),caption="Table")
          report$add.descr(paste("File name with extra output is ",report$make.file.name("data.csv")))
        }
      }
    }

    my.func.2<-function() {
      print("my.func.2")
      report$add.header("H2.1") %anrep>>% {
        for(i in 1:3) {
          my.func.3(sprintf("H2.1 two subreports here %s",i))
        }
      }
    }

    my.func.1<-function() {
      print("my.func.1")
      report$add.header("H1")
      report$add.descr("H1 text")
      report$add.header("H2 with subsections") %anrep>>% {
        report$add(plot(x <- sort(rnorm(47))),caption="Figure one in H2")
        my.func.2()
      }
      report$add.header("H3")
      report$add(plot(x <- sort(rnorm(10))),caption="Figure one in H3")
    }

    my.func.1()

    report$save()
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

    report$save()

  })

})
