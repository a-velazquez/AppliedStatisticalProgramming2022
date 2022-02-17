#### Functions for playing Let's Make a Deal


## Function 1
# The "player" chooses a door with the following function
choose_door <- function(x){
  # The game only has 3 doors, so x cannot be any other number
  criteria <- x %in% c(1,2,3)
  if (!criteria){
    stop("Error: input x must be 1, 2, or 3.")
  }
  
  # The output will be a list containing the selected door's number, stored as an integer
  numbered_door <- list("selected"=as.integer(x))
  
  # Assign the output class "door" and return
  class(numbered_door) <- "door"
  return(numbered_door)
}



## Function 2
# The player validates a door with the following function
is_numbered <- function(door){
  if(is.integer(door$selected)){
    return(door)
  } else {
    stop("Error: please select a door with choose_door() to proceed")
  }
}



## Function 3
PlayGame <- function(door){
  # The winning door is chosen (from 1:3, randomly choose 1)
  winning_door <- sample(3,1)
  
  # Determine the outcome with a logical test
  outcome <- winning_door == door$selected
  
  # Use the logical test to deliver the appropriate message
  if(outcome){
    message(paste0("Congratulations! Door ", winning_door, " was correct! You've won the game!"))
  } else{
    message(paste0("Oh no, you chose Door ", door$selected, 
                   " and the winning door was Door ", winning_door, 
                   ". Please play again!"))
  }
}


