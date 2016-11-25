\dontrun {
report <- anrep()

my.func.3<-function(subreports.header) {
  print("my.func.3")
  report$add.header(subreports.header) %anrep//% {
    report$add.header("H2.1.subreport 1") %anrep//% {
      report$add.descr("Some text")
      if(requireNamespace("ggplot2", quietly = TRUE)) {
        report$add(ggplot2::qplot(mpg, wt, data = datasets::mtcars,
                                  facets = vs ~ am,geom = "violin"),
                   caption = "Ggplot2 example")
      }
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
    report$add(plot(x <- sort(rnorm(47))),caption="Figure one in H2",
               graph.unify=T)
    my.func.2()
  }
  report$add.header("H3")
  report$add(plot(x <- sort(rnorm(10))),caption="Figure one in H3",
             graph.unify=T)
}

my.func.1()

report$save()
}
