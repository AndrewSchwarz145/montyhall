% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-problem.R
\name{play_game}
\alias{play_game}
\title{Determining the Results for Both Strategies}
\usage{
play_game()
}
\arguments{
\item{...}{no arguments are used by this function.}
}
\value{
This function returns a data frame that has two columns
and two rows showing the strategies used and results.
}
\description{
\code{play_game} is a wrapper function that contains all of
the aforementioned functions.  It assigns them to objects
to be used as inputs in the corresponding function's arguments.
This function then puts the results of each strategy into a
usable data frame designed to be used in a for loop. The data
frame has two columns and rows displaying the strategy used and
the results of using said strategy.
}
\details{
This function shows the results of each strategy for one game.
}
\examples{
 play_game()
 
 To test this function simply use it several times:
 play_game()
 play_game()
 play_game()
 
 This should produce tables that display the strategy and if it won
 or loss the game.
 
}
