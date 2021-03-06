% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report_class.R
\docType{class}
\name{anrep-class}
\alias{anrep-class}
\alias{anrep}
\title{Class for the analysis report generator.}
\description{
An object of this class can be considered essentially as a graphical logger for tables, plots,
html widgets and other
objects created during the execution of your analysis pipeline.
The object of this class generates a Markdown text and converts it into a multi-page static
Web site with its \code{save} method. Sections, tables and plots are auto-numbered and
provided with their own URLs.
}
\details{
In the popular R literate programming frameworks like
\href{https://cran.r-project.org/package=knitr}{\pkg{knitr}} package,
the execution flow of the analysis proceeds linearly from the top to the bottom
code chunks on a document page. In contrast to that, with the "logger" approach adopted
by \code{anrep}, the user inserts logging calls into arbitrary places of their existing analysis
code. It does not matter how deeply nested these calls are inside functions, loops or
conditional branches. For example, using regular R loop constructs, it becomes trivial to
generate multiple report section that vary only by the parameter values supplied to the
otherwise identical analysis protocol.

This class relies heavily on the Markdown generation infrastructure provided in the
\href{https://CRAN.R-project.org/package=pander}{\pkg{pander}} package.
The user can manipulate various \pkg{pander}
global options by calling \code{\link[pander]{panderOptions}} method, in order to
fine-tune various defaults used in generating the Markdown representations of R objects.
The \code{anrep} class was inspired in part by the \code{\link[pander]{Pandoc}} design.
The \pkg{anrepr} package provides \code{\link{set_default_external_options}} method
that resets various global options in \pkg{pander} and \pkg{ggplot2} packages to the
values tuned for best default appearance in the final HTML reports generated by this
class.

You can create the new instance of this class as \code{report = anrep(...)} where
the available constructor arguments are described for the \code{initialize} method
below. The object will be writing its output files into the directory that was current
when it was created, silently over-writing existing content. In other words, it assumes
that the current working directory is dedicated by the user for the report output.

In a typical use pattern, you would create singleton report object in a global environment
(such as \code{report <<- anrep()}), and then call methods on this global object in various
locations of your code. You can also create \code{anrep} object in a local environment and
pass this object around any way you want.
Because this is the R reference class, the report object is passed by reference without creating
 new copies.
See the description of the \code{add.header} method for growing the hierarchical section
structure of your report.
}
\section{Methods}{

\describe{
\item{\code{add(
  x,
  caption = NULL,
  show.image.links = TRUE,
  caption.type = NULL,
  graph.output = pander::evalsOptions("graph.output"),
  hi.res = pander::evalsOptions("hi.res"),
  new.paragraph = TRUE,
  ...
)}}{Add plot object, or any other kind of object.

  parameter: caption Text that will be added as a numbered caption

  parameter: show.image.links Include links to generated plot image(s) to the caption

  parameter: ... Other arguments to pander::evals()

  You should use more specific methods when adding tables (add.table()) or vectors
  (add.vector()) because those will try to coerce their inputs into the compatible
  format, and allow you setting additional arguments.
  }

\item{\code{add.descr(x, caption = NULL, ...)}}{Add text.

  Arguments to add.p method are accepted here}

\item{\code{add.file(x, caption = NULL, wrap.caption = TRUE, skip.if.empty = FALSE, ...)}}{Add a link to a file.

  It is recommended that the file name is generated with make.file.name method.

  parameter: wrap.caption Escape Markdown tags in caption
  }

\item{\code{add.header(
  title,
  level = NULL,
  section.action = "incr",
  sub = FALSE,
  echo = NULL,
  ...
)}}{Add new header and automatically start new report section.

  You should rarely provide any arguments other than title.

  Instead, combine a call to this method with the \%anrep??\% infix operators, such as:

  report$add.header('Title') \%anrep>>\%  \{

    report$add.descr('My code block')

  \}

  return: The instance of the anrep class on which this method was called. This is needed
  for the chaining with the infix %anrep??% operators to allow them updating the state
  of the anrep object.
  }

\item{\code{add.image(
  x,
  caption = NULL,
  wrap.caption = TRUE,
  skip.if.empty = FALSE,
  width = pander::evalsOptions("width"),
  height = NULL,
  ...
)}}{Add the image that already exists as a file.

  It is recommended that the file name is generated with make.file.name method.

  parameter: wrap.caption Escape Markdown tags in caption
  parameter: height Default is NULL to let width define it without breaking aspect ratio
  }

\item{\code{add.list(x, ...)}}{Add list object}

\item{\code{add.p(x, rule = FALSE, echo = NULL, ...)}}{Add new paragraph

  parameter: x Text to write in the new paragraph
  parameter: rule Also add horizontal rule
  }

\item{\code{add.package.citation(x, caption = NULL, ...)}}{Add citation for a package name}

\item{\code{add.printed(x, caption = NULL, echo = NULL, ...)}}{Add a chunk of text verbatim preserving it from Markdown formatting.

  This is a lazy escape hatch for situations where R function such as Anova
  summary returns text carefully formatted with spaces and you want to
  include this formatted output instead of generating your own formatting
  with Markdown.

  parameter: caption If provided, numbered caption will be added to the text chunk.
  }

\item{\code{add.table(
  x,
  caption = NULL,
  show.row.names = is.matrix(x),
  wrap.caption = TRUE,
  wrap.vals = TRUE,
  export.to.file = TRUE,
  file.name = NULL,
  show.first.rows = 200,
  show.first.cols = 200,
  split.tables = Inf,
  style = "rmarkdown",
  skip.if.empty = FALSE,
  echo = NULL,
  ...
)}}{Add table object.

  parameter: caption Caption

  parameter: show.row.names Show row names

  parameter: wrap.caption Escape Markdown tags in caption

  parameter: wrap.vals Escape Markdown tags in value cells

  parameter: export.to.file Also save table in a CSV file

  parameter: file.name If exporting to file, use this file name (name will be auto-generated if not set -
  this is a recommended way)

  parameter: show.first.rows Only show in Markdown that many rows. All rows will be still exported to file.
  This option is set to a reasonable default value to prevent the Web browser from slowing
  down when viewing report files with many large tables.

  parameter: show.first.cols Only show in Markdown that many columns. All columns will be still exported
  to file.

  parameter: split.tables Overwrites default pander argument for splitting wide tables across rows

  parameter: style Overwrites default pander argument for tables Markdown style

  parameter: ... Passed to pander::pandoc.table.return
  }

\item{\code{add.vector(x, name = NULL, show.row.names = TRUE, caption = NULL, ...)}}{Add vector object.

  Vector is added as a table. Parameters to add.table method are accepted here.
  }

\item{\code{add.widget(
  x,
  caption = NULL,
  show.image.links = TRUE,
  width = 800,
  height = 800,
  new.paragraph = TRUE,
  data.export = NULL,
  data.export.descr = NULL,
  show.inline = TRUE,
  ...
)}}{Add htmlwidget object.

  parameter: caption Text to add as numberd caption

  parameter: show.image.links Include a link to the generated widget file to the caption

  parameter: data.export File to link from caption. This can be used if the widget has some
  associated dataset saved as a separate file, and that datset can be useful to the user by
  itself.

  parameter: data.export.descr Description for the data.export file, to be added to the
  caption.

  parameter: show.inline Show widget inline in the current report page (currently IFRAME is used
  for that, but this may change). If FALSE, only provide
  a link to the widget from the caption. You might want to set this to FALSE if the widget is
  very expensive to render in the browser (for example, a multiple sequence alignment widget
  for thousands of sequence). In this case, the user will have to navigate to the link in order
  to see the widget.
  }

\item{\code{initialize(
  title = "Analysis Report",
  author = "",
  date = base::date(),
  out.file = "report",
  out.formats = NULL,
  incremental.save = FALSE,
  self.contained.html = TRUE,
  echo = FALSE,
  knitr.auto.asis = TRUE,
  ...
)}}{Construct new instance.

  parameter: title Report title string

  parameter: author Author to show below the title

  parameter: date of the report

  parameter: out.formats Convert generated Markdown to these formats (using Pandoc binary executable). In principle,
  the value can be any of the output formats supported by Pandoc, plus an additional value `knitr`.
  However, not all features of the report work in every format. If NULL, the default output format will be 'html',
  unless we detect that we are executing from a Knitr session, in which case the format will be set to 'knitr'.
  With 'html', the final output will be one or more HTML files on disk. With 'knitr', the method `anrep$save()`
  will return a string with a single concatenated report (any subreport designations will be ignored). You can
  also just type the report variable name or print it instead of calling `anrep$save()` method under Knitr.
  If your are creating the report from Knitr but still want HTML output instead of a string, then set this parameter
  explicitly to 'html'.

  parameter: knitr.auto.asis This has the same effect as `pander::panderOptions` option with the same name. If set,
  this report object will try to detect if it is being executed from knitr, and wrap the final returned
  Markdown in knitr::asis_output class, so that Markdown is rendered as such, rather than as a literal string.
  You would only want to set this to FALSE if you want to obtain the literal Markdown string under Knitr session
  for viewing or further manipulation.

  parameter: self.contained.html Embed images (default resolution versions) and stylesheets into each HTML file.
  Note that generated data files and widget files will still be referenced by links. In any
  case, the report directory as a whole will be portable in a sense that it can be archived into
  a different location or computer and viewed where.

  parameter: out.file Base for the output report file names (Markdown and final formats
  such as HTML). Files will be named like 1-<out.file>.html.
  }

\item{\code{make.file.name(
  name.base = "",
  make.unique = TRUE,
  dir = NULL,
  sec.path = NULL,
  name.base.first = FALSE,
  name.ext = ""
)}}{Return new file name that you can use to write your data to be included with the report.

  parameter: name.base Basename for the file. If make.unique is TRUE, extra suffix will be generated to
  make the final file name unique

  parameter: dir Normally should be left unset (NULL), in which case the file name will be located
  under the data directory of the report

  parameter: sec.path For internal use. Default value will result in a stringified version of the
  current section number becoming part of the file name

  parameter: name.base.first If adding section path to the file name, add it after the name.base
  }

\item{\code{save(
  out.file = NULL,
  out.formats = NULL,
  self.contained.html = NULL,
  pandoc.binary = NULL,
  css.file = NULL,
  export = TRUE,
  concatenate = FALSE,
  pandoc.meta = TRUE,
  knitr.auto.asis = NULL,
  sort.by.sections = FALSE
)}}{Save the report to Markdown and convert it to the final output formats.

  Call this at the end of your analysis.

  Normally, you should leave all arguments at their default values, and simply call

  report$save()

  parameter: out.file A string to use as a root name for the output report files,
  which will be generated as <out.file>_<subreport_section>.<out.format> (for example,
  report_1.1.Rmd or report_1.1.html). This value should not normally contain a directory component
  because all links will be always generated relative to the current working directory.
  Alternatively, out.file can be an open output connection object (inherit from base::connection
  class). In that case, concatenate argument will be reset to TRUE, export - to FALSE, and
  the single concatenated Markdown stream will be sent to the out.file connnection.

  parameter: out.formats Convert Markdown to these formats (using Pandoc binary executable). The only
  format that supports all features of our generated Markdown is currently `html`, which will
  be the default value. In particular, subreports and htmlwidgets will only work with HTML
  output. Another special case is `knitr`. It only can be a single output format, and
  in that case the Markdown output will be concatenated and send to standard output stream
  without Pandoc meta data header. This mode overrides a number of other parameters.

  parameter: self.contained.html Embed images (default resolution versions) and stylesheets into each HTML file.
  Note that generated data files and widget files will still be referenced by links. In any
  case, the report directory as a whole will be portable in a sense that it can be archived into
  a different location or computer and viewed where. Default value is initialized by the report
  object constructor.

  parameter: pandoc.binary Path to Pandoc binary executable. Default value is initialized from pander
  package global options or located in the PATH variable, in that order of preference.

  parameter: css.file Path to CSS (Cascading Style Sheets) file to include in all HTML report files.
  Use this arguments to modify the style of the reports.
  This file will be copied into the report directory and linked from where.
  The default file included with the package can be accessed with

  system.file('extdata', 'anrep.css', package = 'anrepr')

  parameter: export Export Markdown report files into the final output formats (ignored if
  Pandoc binary is not found)

  parameter: concatenate Flatten all subreports into a single output file

  parameter: pandoc.meta Add Pandoc metadata header

  parameter: knitr.auto.asis Same meaning as in the constructor

  return: data.frame with files names for all (sub)reports for all output formats, and a field named
  is_root that is set to TRUE for the row that is the root level report (the report
  from which the viewing has to start)
  }

\item{\code{write.table.file(
  data,
  name.base,
  make.unique = TRUE,
  descr = NULL,
  row.names = FALSE,
  row.names.header = TRUE,
  file.name = NULL,
  ...
)}}{Save table in a CSV text file.

  Most arguments are taken from the signatures of make.file.name method and
  base::write.csv.

  parameter: row.names.header If writing row names, add a column name 'rownames' to the header. This
  works around unintended misallignment of rows when loading the file into some
  spreadsheet programs.
  }
}}

\examples{
\dontrun{
## Refer to the vignettes to see how to build a complete report by calling the methods of this
## class.
}
}
