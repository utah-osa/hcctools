\name{get_tag_cor}
\alias{get_tag_cor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Get the Pearson Correlation for each Tag in an APCD df
}
\description{
For each tag it calculates the pearson correlation in respect to the median bundle price. An absolute value transform is applied on the correlation valeus so that tags may be sorted by magnitude. This helps determine which tags may be influencing price and justify a split.
}
\usage{
get_tag_cor(df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{x} here~~
An APCD dataframe which has tags.
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
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Alexander Nielson
}

