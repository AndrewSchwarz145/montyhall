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
#'   To test this function simply use it several times.
#'   
#'   create_game()
#'   create_game()
#'   create_game()
#'   
#'   This should result in randomly sampled games.
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'  Selecting an Initial Door at Random
#' 
#' @description
#'  `select_door` randomly samples a vector that represents the possible
#'  door options a contestant could have chosen. 
#' 
#' @details
#'  This function replicates the "Let's Make a Deal" game by selecting
#'  one of the three doors a contestant could have chosen.  This function
#'  randomly selects a door which will then be used later to test the
#'  two strategies of switching vs staying with a selected door.
#' 
#' @param ... no arguments are used in this function
#' 
#' @return The function returns a length of 1 numeric vector
#'  indicating what door the contestant has initially picked.
#' 
#' @examples
#'  select_door()
#'  
#'  To test this code simply use it several times.
#'  select_door()  
#'  select_door()
#'  select_door()
#'  
#'  This should results in random doors being selected.
#'   
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'  Host Opens One of the Losing Doors
#'  
#' @description
#'  `open_goat_door` selects a door with a goat behind it to reveal to the
#'  contestant.
#'  
#' @details
#'  This function replicates the part of the game where the host opens one 
#'  of the doors with a goat behind it. The contestant then has a choice to
#'  make with this information- to stay with their initial pick or switch,
#'  which will be done in the next function.
#'  
#' @param ...
#'  This function has two arguments: game and a.pick:
#'  game is inputted by the create_game function 
#'  and a.pick is inputted by the select_door function.
#'    
#' @return This function returns a length of 1 numeric vector
#'  indicating which goat door is revealed to the contestant.
#'  
#' @examples
#'  open_goat_door(game, a.pick)
#'  
#'  To test this code simply use the function several times,
#'  there are no defaults for this function so you must
#'  assign the outputs of create_game and select_door to objects
#'  to be inputted into this function. 
#'  
#'  Test example:
#'  
#'  game1 <- create_game()
#'  door1 <- select_door()
#'  open_goat_door(game1, door1) 
#'  
#'  This function should always result in a goat door being 
#'  opened and that door should not be a contestant's pick.
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
#'  Contestant Chooses to Switch or Stay
#'  
#' @description
#'  `change_door` allows contestant to switch their initial pick to
#'  the remaining unopened door or allows them to keep their
#'  initial pick.
#'  
#' @details
#'  This function replicates the part of the game where the contestant 
#'  is allowed to stay with their initial pick or switch their pick to
#'  the unopened door.
#' @param ... 
#'  This function has 3 arguments: stay = True or False,
#'  opened.door which is inputted by the open_goat_door function,
#'  and a.pick which is inputted by the select_door function. 
#'  
#' @return This function returns a length of 1 numeric vector
#'  that represents the contestant's final pick.
#'  
#' @examples
#'  change_door(stay = F, opened.door, a.pick)
#'  
#'  This function have values put in for the defaults or
#'  it must be used with the outputs of open_goat_door
#'  and select_door assigned to objects.  
#'  
#'  You can test this function with the following examples.
#'  
#'  Test example: 
#'  
#'  change_door(stay = T, opened.door = 1, a.pick = 2)
#'  This should result in 2 since stay is TRUE.
#'  
#'  change_door(stay = F, opened.door = 1, a.pick = 2)
#'  This should result in 3 since stay = FALSE.
#'  
#'  Test example:
#'  
#'  object_select_door <- select_door()
#'  object_open_goat_door <- open_goat_door()
#'  change_door(stay = T, object_open_goat_door, object_select_door)
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
#'  Determining the Winner
#' @description
#'  `determine_winner` shows if the contestant has won or lost the 
#'  game depending on if their final pick had a goat or car behind it.
#'  
#' @details
#'  This function replicates the part of the game where the contestant's
#'  final pick is opened revealing if there was a goat or car behind
#'  the door.
#'  
#' @param ... 
#'  This function has two arguments, final.pick and game.
#'  final.pick is inputted by the change_door function,
#'  and game is inputted by the create_game function.
#'  
#' @return This function returns a length of 1 character vector 
#'  displaying the results of the game.
#'  
#' @examples
#'  determine_winner(final.pick, game)
#'  
#'  In order to use this function you must use objects assigned
#'  to the outputs of create_game() and change_door().
#'  
#'  To test this function you must use a vector that replicates
#'  the results from create_game() and a numeric value for 
#'  final.pick.
#'  
#'  Test example:
#'  
#'  this.game <- c("goat", "car", "goat")
#'  determine_winner(final.pick = 1, this.game)
#'  
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
#'  Determining the Results for Both Strategies
#' @description
#'  `play_game` is a wrapper function that contains all of
#'  the aforementioned functions.  It assigns them to objects
#'  to be used as inputs in the corresponding function's arguments.
#'  This function then puts the results of each strategy into a 
#'  usable data frame designed to be used in a for loop. The data 
#'  frame has two columns and rows displaying the strategy used and
#'  the results of using said strategy.
#'   
#' @details
#'  This function shows the results of each strategy for one game.
#'  
#' @param ... no arguments are used by this function.
#'  
#' @return This function returns a data frame that has two columns 
#'  and two rows showing the strategies used and results.
#'   
#' @examples
#'  play_game()
#'  
#'  To test this function simply use it several times:
#'  play_game()
#'  play_game()
#'  play_game()
#'  
#'  This should produce tables that display the strategy and if it won
#'  or loss the game.
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
#'  Simulating the Game
#'  
#' @description
#'  `play_n_games` is a function made to simulate the strategies
#'  used in the game and to table the results. This function is designed
#'  to test which strategy is better at winning the game by playing the 
#'  game many times and tallying up the wins.  For the most accurate 
#'  results, set n = 10000. 
#'  
#' @details
#'  This function shows the results for each strategy for a specified
#'  amount of games. It can simulate the game up to n amount of times.
#'  
#' @param ... 
#'  This function has one argument which is n = amount of games.
#' 
#' @return 
#'  This function returns a probability table using the dplyr package.
#'  It shows the win rates for each strategy.
#'   
#' @examples
#'  plan_n_games(n=10000)
#'  
#'  To test this game you must insert an integer for n.
#'  
#'  Test example:
#'  play_n_games(n=10)
#'  play_n_games(n=100)
#'  play_n_games(n=1000)
#'  
#'  This should result in a proportions table that displays the 
#'  win rate of each strategy for the specified amount of games. 
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
