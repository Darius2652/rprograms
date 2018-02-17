# Helper Methods

Deck <- c(1:13, 1:13, 1:14, 1:14)

CardNames <- c("ACE", "TWO", "THREE", "FOUR", "FIVE", "SIX", "SEVEN", "EIGHT",
               "NINE", "TEN", "JACK", "QUEEN", "KING", "JOKER")

Card <- function(number) {
  return(CardNames[number]);
}

RandomSnap <- function() {
  return(paste(
    sample( c( "S", "s", "$" ) )[1],
    sample( c( "N", "n"      ) )[1],
    sample( c( "A", "a", "@" ) )[1],
    sample( c( "P", "p"      ) )[1],
    sep = ""
  ))
}

# Program

ShuffledDeck <- sample(c(Deck))
  # print(Card(ShuffledDeck))

CardCount <- length(ShuffledDeck)

HalfPack <- CardCount / 2;

Deck1 <- ShuffledDeck[ 1:HalfPack ]
Deck2 <- ShuffledDeck[ (HalfPack+1):CardCount ]

Deck1 <- sample(Deck1)
Deck2 <- sample(Deck2)

play <- TRUE

cat("-----------------\n    SNAP GAME\n-----------------\n")
cat(paste("Each player has", HalfPack, "cards.\n"))

input <- ""

while(input != "y" && input != "n") {
  cat("\nWould you like to play? [Y/N]\n");
  input <- tolower(readline())
}

if(input == "n") {
  play <- FALSE
}

AllTimes <- 0
plays <- 0

while(play) {
  
  snap <- RandomSnap();
  input <- "";
  
  cat(paste("SNAP! You have the same card.\n\nEnter the following to continue:\n\n    [ ", snap ," ]\n", sep = ""))
  
  startTime <- Sys.time()
  while(input != snap && input != "stop") {
    input <- readline()
  }
  endTime <- Sys.time()
  
  if(input != "stop") {
    plays <- plays + 1
    AllTimes <- AllTimes + as.double(endTime - startTime)
  }
  
  cat(endTime - startTime)
  
  cat("\n\n\n");
  
  if(input == "stop") {
    play <- FALSE
  }
}

cat(paste("\nAverage Time: ", (AllTimes / plays) ,"\n\n"))

print("GAME OVER")
