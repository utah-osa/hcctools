\name{get_2sd_cat}
\alias{get_2sd_cat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~
Assign the category of a median price using 2*sd formula
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Compares the median price and determines if it is within, above, or below the mean + 2 * sd. It then creates a new variable called "bundle_group" to the dataframe.
}
\usage{
get_2sd_cat(df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
dataframe
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Alexander Nielson
}


