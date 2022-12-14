% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-problem.R
\name{open_goat_door}
\alias{open_goat_door}
\title{Host Opens One of the Losing Doors}
\usage{
open_goat_door(game, a.pick)
}
\arguments{
\item{...}{This function has two arguments: game and a.pick:
game is inputted by the create_game function
and a.pick is inputted by the select_door function.}
}
\value{
This function returns a length of 1 numeric vector
indicating which goat door is revealed to the contestant.
}
\description{
\code{open_goat_door} selects a door with a goat behind it to reveal to the
contestant.
}
\details{
This function replicates the part of the game where the host opens one
of the doors with a goat behind it. The contestant then has a choice to
make with this information- to stay with their initial pick or switch,
which will be done in the next function.
}
