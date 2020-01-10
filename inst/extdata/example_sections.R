# ---- example_sections_report_functions ----

make_example_sections_report <- function(report_dir=NULL,export=T) {

  # First, we define several functions which will be called one from another,
  # in order to mimic a flow of some typical analysis pipeline. Each
  # function uses our `report` object to add data to the report as well as
  # to create new sections and subreports. We create the `report` object
  # below the definitions of the "analysis" functions.

  ## Top-level function of the analysis pipeline
  my.func.1<-function() {
    print("my.func.1")

    # Add new header. Section numbering will be auto-incremented at the current level
    # (e.g. 1.1 to 1.2)
    report$add.header("H1")

    # Add Markdown-formatted text
    report$add.descr("*H1 text*")

    # Add new header and shift the section numbering into a sub-section
    # with `%anrep>>%` operator (e.g. 1.2 >> 1.2.1)
    report$add.header("H2 with subsections") %anrep>>% {

      # Add a plot with base graphics. Caption will be auto-numbered and
      # contain HTML anchor to reference it later inside the report if your need,
      # for example, to send a link to that plot to a collaborator.
      report$add(plot(x <- sort(rnorm(47))),caption="Figure one in H2",
                 hi.res = T,
                 graph.unify=T)

      # Call deeper into the analysis pipeline
      my.func.2()
    }
    # The closing } above has returned us to the section level that was current
    # before the `%anrep>>% {` call, e.g. 1.2.1 >> 1.2

    # Add new header. Section numbering will be auto-incremented at the current level
    # (e.g. 1.2 to 1.3)
    report$add.header("H3")

    # Add a plot with base graphics
    report$add(plot(x <- sort(rnorm(10))),caption="Figure one in H3",
               hi.res = T,
               graph.unify=T)
  }

  ## This function is called from the top-level function
  my.func.2<-function() {
    print("my.func.2")

    # Add new header and shift the section numbering into a sub-section
    # with `%anrep>>%` operator (e.g. 1.1 >> 1.1.1)
    report$add.header("H2.1") %anrep>>% {
      # Call another function a loop with different parameters, which will
      # result in three subreports, each with two sub-subreports itself. In
      # the HTML output, the subreports will become separate Web pages,
      # accessible through a Subreport link in the parent report.
      for(i in 1:3) {
        my.func.3(sprintf("H2.1 two subreports here %s",i))
      }
    }
    # The closing } above has returned us to the section level that was current
    # before the `%anrep>>% {` call
  }

  # This function will be called in a loop with different parameters for the
  # "analysis" (in this mock case - just different header strings). This
  # will result in several subreports being generated, one for each iteration.
  my.func.3<-function(subreports.header) {
    print("my.func.3")

    # Add new header and drop into a subreport (notice the `%anrep//%` operator)
    report$add.header(subreports.header) %anrep//% {

      # Add new header and drop one more level into another subreport
      report$add.header("H2.1.subreport 1") %anrep//% {

        report$add.descr("**Some text**")

        # Add a sample ggplot2 plot if the package is available
        if(requireNamespace("ggplot2", quietly = TRUE)) {
          report$add(ggplot2::qplot(mpg, wt, data = mtcars,
                                    facets = vs ~ am,geom = "violin"),
                     caption = "Ggplot2 example",
                     hi.res = T)
        }

        if(requireNamespace("DT", quietly = TRUE)) {
          dt = DT::datatable(mtcars, options = list(pageLength = 15))
          report$add.widget(dt,
                            caption = "Dynamic DataTable viewer from DT package")
        }

        if(requireNamespace("plotly", quietly = TRUE)) {
          report$add.widget(plotly::plot_ly(cbind(Model=rownames(mtcars),mtcars),
                                            x = ~mpg, y = ~qsec, color = ~hp, mode="markers", marker = list(size = ~wt),
                                            text =~paste("Model:", Model, "<br>Weight:", wt)),
                            caption = "Dynamic Plotly plot: hover, zoom and brush with your mouse")
        }

        # This simply adds a header and increments the section numbering
        report$add.header("H2.1.subreport 1 incremented section")

        # Add a table
        report$add.table(data.frame(A=c("a","b"),B=c(1,2)),caption="Table")

        # Generate a unique file name into we could save some data
        # inside the report directory
        report$add.descr(
          paste("Generated a unique file name to save some extra output:",
                report$make.file.name("data.csv")))
      }
      # The closing } above has returned us to the reporting level that was
      # current before the `%anrep//% {` call

      # Add new header and drop one more level into another subreport
      report$add.header("H2.1.subreport 2") %anrep//% {
        report$add.descr("Some text")
        report$add.header("H2.1.subreport 2 incremented section")
        report$add.table(data.frame(A=c("a","b"),B=c(1,2)),caption="Table")
        report$add.descr(paste("File name with extra output is ",
                               report$make.file.name("data.csv")))
      }
    }
  }

  ## And now, setting up the report object and running the pipeline

  message(sprintf("Working dir is %s",getwd()))

  # If requested, run in a separate output directory, and make sure we
  # restore the current working directory at the end
  if(!is.null(report_dir)) {
    curdir = getwd()
    dir.create(report_dir,showWarnings = F,recursive = T)
    on.exit(setwd(curdir))
    setwd(report_dir)
  }

  # Create a report object visible from all functions that will need it.
  # Here it will be visible because it is bound to the frame in which the
  # functions were defined, but you can also pass it as a parameter (it is
  # the instance of a reference class, and will not be copied), bind it
  # to the global environment, or create a helper function to extract it from
  # some other environment.
  # Note that we set out.format to 'html' to make sure that we get the results in
  # HTML output files on disk even if this is executed from a knitr session
  # (otherwise under knitr we would get repot as a string return value)

  report <- anrep(out.formats="html")

  # Call my complicated analysis pipeline
  my.func.1()

  # Finally, save the report and, optionally (True by default), export it
  # to the final HTML format. The report is organized as a number of files
  # and directories under the current working directory.
  report$save(export=export)
}

# ---- example_sections_report_run ----

#' \dontrun{
make_example_sections_report("example_sections_report")
#' }
