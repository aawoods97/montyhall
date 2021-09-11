#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'   Select a door.
#'
#' @description
#'   `select_door()` allows the player to pick a random door out of the three.
#'
#' @details
#'   The contestant selects a door of which they assume the car is behind.
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a value corresponding with the door the contestant selected.
#'
#' @examples
#'   select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   A door with a goat behind it is opened.
#'
#' @description
#'   `open_goat_door()` generates a value corresponding with the opened door.
#'
#' @details
#'   One of the three doors is opened to reveal a goat. The
#'   opened door cannot be the door the contestant chose nor
#'   the door with the car behind it.
#'
#' @param ... the arguments include thegame function generated from 'create_game()' and the 
#'   selected door from the 'select_door()'
#'
#' @return The function returns the door with the goat behind it.
#'
#' @examples
#'   open_goat_door()
#'
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'   Change the selected door.
#'
#' @description
#'   `change_door()` generates a number corresponding with the door the contestant selects.
#'
#' @details
#'   The contestant is given the option to change their door selection.
#'
#' @param ... the arguments are stay (whether the contestant chooses to stay), the door that
#'   the host opened, and the door the contestant chose.
#' 
#' @return The function returns a number the door the contestant chooses.
#'
#' @examples
#'   change_door()
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#'   Determine whether the contestant won or not.
#'
#' @description
#'   `determine_winner()` tells the contestant if they won if they choose the door with the car 
#'
#' @details
#'   The contestant is notified if they win the game. If the
#'   contestants pick matches the door with the car, they are
#'   declared the winner. If they choose the door with the goat,
#'   The contestant is informed that they lost the game.
#'
#' @param ... the arguments include the final door the contestant picked and the game from 
#'   'create_game()'
#' 
#' @return The function returns a message on the game's result
#'
#' @examples
#'   determine_winner()
#'
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'   Play the Monty Hall Problem game.
#'
#' @description
#'   `play_game()` generates an entire game played by the computer.
#'
#' @details
#'   All functions are called in the function to result in a full
#'   game play of the Monty Hall Problem.
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns if the game was won or lost.
#'
#' @examples
#'   play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'   Plays a new Monty Hall Problem game that repeats 'n' times.
#'
#' @description
#'   `play_n_games()` generates 'n' new games that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. The game will be simulated 'n' times.
#'
#' @param ... the argument is the number of times the game will be simulated.
#' 
#' @return The function returns a table with the proportions of losing and winning
#'   if they choose to stay or switch their final selection door.
#'
#' @examples
#'   play_n_games()
#'
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
