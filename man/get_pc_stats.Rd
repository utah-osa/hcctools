\name{get_pc_stats}
\alias{get_pc_stats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{get_pc_stats
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
get_pc_stats pulls all the CPT and HCPCS codes from the bundled procedure string. Then it splits each string and counts the frequency of each code. It then orders by descending frequency.
}
\usage{
get_pc_stats(df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{ An APCD dataframe with tags
%%     ~~Describe \code{x} here~~
}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
a dataframe with the frequency of procedure codes by descending frequency.
}
\author{
%%  ~~who you are~~
Alexander Nielson
}

