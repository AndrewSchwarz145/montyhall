% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-problem.R
\name{determine_winner}
\alias{determine_winner}
\title{Determining the Winner}
\usage{
determine_winner(final.pick, game)
}
\arguments{
\item{...}{This function has two arguments, final.pick and game.
final.pick is inputted by the change_door function,
and game is inputted by the create_game function.}
}
\value{
This function returns a length of 1 character vector
displaying the results of the game.
}
\description{
\code{determine_winner} shows if the contestant has won or lost the
game depending on if their final pick had a goat or car behind it.
}
\details{
This function replicates the part of the game where the contestant's
final pick is opened revealing if there was a goat or car behind
the door.
}
\examples{
 determine_winner(final.pick, game)
 
 In order to use this function you must use objects assigned
 to the outputs of create_game() and change_door().
 
 To test this function you must use a vector that replicates
 the results from create_game() and a numeric value for 
 final.pick.
 
 Test example:
 
 this.game <- c("goat", "car", "goat")
 determine_winner(final.pick = 1, this.game)
 
 
}
