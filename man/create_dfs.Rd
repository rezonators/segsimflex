% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helperFunctions.R
\name{create_dfs}
\alias{create_dfs}
\title{Convert plaintext strings to data.frames for segsimflex}
\usage{
create_dfs(
  x,
  boundary_regex = c(",|\\\\.|(--)|\\\\?"),
  speaker_regex = " {0,1}[^\\\\s]*?: "
)
}
\arguments{
\item{x}{The strings to be converted. Generally of length 2 (for the two annotations).}

\item{boundary_regex}{Regular expression for boundaries.}

\item{speaker_regex}{Regular expression for participant labels.}
}
\value{
A list of two data frames, one per annotator.
}
\description{
Convert plaintext strings to data.frames for segsimflex
}
\examples{
create_dfs(c("JOHN: Hello , how are you ? MARY: I am fine , thank you .", "JOHN: Hello how are you ? MARY: I am fine thank you ."))
create_dfs(c("A: 下 雨 天 , 留 客 . 天 留 , 我 不 留 . B: 下 雨 天 , 留 客 天 , 留 我 不 ? 留 !", "A: 下 雨 天 留 客 . 天 留 我 不 留 . B: 下 雨 天 , 留 客 天 , 留 我 不 ? 留 ."))
}
