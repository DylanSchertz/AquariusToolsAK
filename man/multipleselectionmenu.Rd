% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multipleselectionmenu.R
\name{multipleselectionmenu}
\alias{multipleselectionmenu}
\title{Multiple Selection Menu}
\usage{
multipleselectionmenu(options, Title = "Which options?", preselected)
}
\arguments{
\item{options}{Character object of possible values to select.}

\item{Title}{(optional) Character for title prompt to pop up. Default is "Which options?"}

\item{preselected}{(optional) character ojbect of sites to be added to the output}
}
\value{
A character object of selected item(s).
}
\description{
This function opens a graphic interface to allow selection of multiple values.
}
\examples{
multipleselectionmenu(unique(c("DENA-023", "YUCH-004", "WRST-015", "BELA-088")),"Which sites? (select one at a time)")
}
