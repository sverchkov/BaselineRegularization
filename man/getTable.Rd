% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getTable.R
\name{getTable}
\alias{getTable}
\title{Get a table, possibly from a database}
\usage{
getTable(con, table, label = deparse(rlang::enexpr(table)))
}
\arguments{
\item{con}{A DBI Connection obtained with [DBI::dbConnect()]}

\item{table}{A table or the name of the table to extract}

\item{label}{User-friendly name for the table (used in info message)}
}
\value{
The table
}
\description{
Takes a connection, table or table name, and (optional) label, if the second parameter isn't a table name, it just
returns it, otherwise, announces that it is loading the table from the database and uses [dplyr::tbl()] to do that.
Also adds an explanatory message if dbplyr is not loaded.
}
