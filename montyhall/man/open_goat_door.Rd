% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-problem.R
\name{open_goat_door}
\alias{open_goat_door}
\title{A door with a goat behind it is opened.}
\usage{
open_goat_door(game, a.pick)
}
\arguments{
\item{...}{the arguments include thegame function generated from 'create_game()' and the
selected door from the 'select_door()'}
}
\value{
The function returns the door with the goat behind it.
}
\description{
\code{open_goat_door()} generates a value corresponding with the opened door.
}
\details{
One of the three doors is opened to reveal a goat. The
opened door cannot be the door the contestant chose nor
the door with the car behind it.
}
\examples{
  open_goat_door()

}
