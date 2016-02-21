#########################
### Let's Make a Deal ###
#########################

## Define a new class: door. Objects of this class simply take on one numeric value: 1, 2, or 3 – indicating which door a candidate chooses.

## Function:
# - creates an object of class door
# - generates numeric value (1, 2, or 3)
# - allow user to select door, randomly assign door as default

doorObject <- function(doorNumber=NULL){
  # assign value for doorNumber if not provided by the user
  if(is.null(doorNumber)){
    doorNumber <- sample(1:3, 1)}
  # check input type (must be integer 1, 2, or 3)
  if(!(doorNumber %in% c(1:3))){
    stop("Input must be 1, 2, or 3!")} 
  # assign object to class door
  class(doorNumber) <- "door"
  # return message of selected door and doorObject
  cat("\n You have selected door: ", doorNumber)
  invisible(doorNumber)
}

# check default w/ no input
doorObject()

# wrong input: not integer
doorObject(doorNumber = 2.4)

# wrong input: not 1:3
doorObject(doorNumber = 10)

# user input: doorNumber = 1
doorObject(doorNumber = 1)


## Create a method for door objects that is called PlayGame. This method is supposed to do the following:
# - take the numeric value that is stored in the door object
# - draw a random number between 1 and 3 that presents the door behind which the car is hidden
# - compare the two numbers, and print a message congratulating a winning candidate that chose the correct door
# - expressing sympathies for a losing candidate that chose the wrong door

# create method for door objects
PlayGame <- function(x){
  UseMethod("PlayGame", x)
}

# create method for objects NOT of class door
PlayGame.default <- function(x){
  # randomly assign winningDoor value
  print("Please make sure input is of class 'door'!")
}

# create method for objects of class: door
PlayGame.door <- function(x){
  # randomly assign winningDoor value
  winningDoor <- sample(1:3, 1)
  # matching input and winningDoor
  if(winningDoor == x){
    # return message of selected door and doorObject
    cat("\n You have selected the winning door: ", winningDoor, "\n Congrats!")
  }
  # mismatch b/w input and winningDoor
  else{
    # return message of selected door and doorObject
    cat("\n You have selected the wrong door! The winning door is: ", winningDoor)
  }
}


# create test object: numeric
testObject1 <- 1
PlayGame(testObject1)

# create test object: character
testObject2 <- "1"
PlayGame(testObject2)

# create test object: class "door"
testObject3 <- doorObject()
PlayGame(testObject3)


