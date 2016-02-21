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


## This time use the S4 system. Under S4, you will need to write:
# - construction function that allows the user to create a door object
# - validation function that checks whether the value stored in door is actually an integer
# - the new generic method PlayGame explained above

# setClass for "door"
setClass(Class="door",
         # specify doorNumber as numeric input by the user
         slots = c(doorNumber = "numeric"
         ),
         # if input not specified by user, randomly assign doorNumber
         prototype = prototype(
           doorNumber = sample(1:3, 1)
         ),
         # create validity check for input as integer 1, 2, or 3
         validity = function(object){
           # check input type (must be integer 1, 2, or 3)
           if(!(object@doorNumber %in% c(1:3))){
             stop("Input must be 1, 2, or 3!")
           }
         }
)

# create new method PlayGame
setMethod("PlayGame", c(x="door"),
          definition = function(x) {
            # randomly assign winningDoor value
            winningDoor <- sample(1:3, 1)
            # matching input and winningDoor
            if(winningDoor == x@doorNumber){
              # return message of selected door and doorObject
              cat("\n You have selected the winning door: ", winningDoor, "\n Congrats!")
            }
            # mismatch b/w input and winningDoor
            else{
              # return message of selected door and doorObject
              cat("\n You have selected the wrong door! The winning door is: ", winningDoor)
            }
          }
)

# create generic function that executes method 
setGeneric(name = "PlayGame", def = function(x){
  standardGeneric("PlayGame")
}
)

# create test object: not class door
# throw error
testObject4 <- new("numeric", doorNumber=1)
PlayGame(testObject4) 

# create test object: doorNumber not integer
# throw error
testObject5 <- new("door", doorNumber=1.5)

# create test object: doorNumber not 1:3
# throw error
testObject6 <- new("door", doorNumber=10)

# create test object: doorNumber not specified
testObject7 <- new("door")
testObject7
PlayGame(testObject7) 

# create test object: not class door
testObject8 <- new("door", doorNumber=1)
testObject8
PlayGame(testObject8) 