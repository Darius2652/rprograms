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

Table <- NULL

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

snap <- FALSE
turn <- 0
card <- 0
lastCard <- 0
winner <- 0

while(play) {
  input <- "";
  
  if(lastCard != 0 && (lastCard == card || card == 14)) {
    snap <- TRUE
  }
  
  if(snap) {
    
    snap <- RandomSnap();
    
    ComputerTime <- runif(1, min = 3.8, max = 7.2);
    
    cat(paste("SNAP! You have the same card.\n\nEnter the following to continue:\n\n    [ ", snap ," ]\n", sep = ""))
    
    startTime <- Sys.time()
    while(input != snap && input != "stop") {
      input <- readline()
    }
    totalTime <- as.double(Sys.time() - startTime)
    
    if(totalTime <= ComputerTime) {
      Deck1 <- c(Deck1, sample(Table))
      cat("\nYou win the deck!\n\n")
    } else {
      Deck2 <- c(Deck2, sample(Table))
      cat("\nYou were too slow.\n\n")
    }
    
    Table <- NULL
    
    snap <- FALSE
    
    lastCard <- 0
    
    if(length(Deck1) == 0) {
      winner = "The Computer"
      play <- FALSE
    } else if(length(Deck2) == 0) {
      winner = "You"
      play <- FALSE
    }
    
  } else if(turn == 0) {
    
    if(length(Deck1) > 0) {
    
      input <- invisible(readline(prompt = paste("[",length(Deck1),"] You      : ")))
      
      lastCard <- card
      
      card <- Deck1[1];
      Deck1 <- Deck1[-1];
      
      cat(paste("                     ", Card(card),"\n", sep = ""))
      
      Table <- c(Table, card)
    
    }
    
    turn <- 1;
    
  } else {
    
    if(length(Deck2) > 0) {  
    
      cat(paste("[",length(Deck2),"] Computer : \n"));
      
      sleepTime <- runif(1) + 0.5;
      
      Sys.sleep(sleepTime);
      
      lastCard <- card
      
      card <- Deck2[1];
      Deck2 <- Deck2[-1];
      
      cat(paste("                     ", Card(card),"\n", sep = ""))
      
      Table <- c(Table, card)
      
    }
    
    turn <- 0;
    
  }
  
  if(length(Deck1) == 0 && length(Deck2) == 0) {
    play <- FALSE
  }
  
  if(input == "stop") {
    play <- FALSE
  }
}

if(winner == 0) {
  cat("\n\nNobody Won\n")
} else {
  cat(paste("\n\n",winner," Won!\n", sep = ""))
}

cat("GAME OVER!")
