# Llewellyn Anthony, 15 February 2018

#Setup

Players <- c(
  "Dealer",
  "Llewellyn",
  "Dian"
)

PlayerScores <- c(
  0,
  0,
  0
)

CardNames <- c("n ACE"," TWO"," THREE"," FOUR"," FIVE"," SIX"," SEVEN","n EIGHT"," NINE"," TEN"," JACK"," QUEEN"," KING")

OneSet <- c(1:10, 10, 10, 10)
Deck <- c(OneSet, OneSet, OneSet, OneSet)

Busted <- 21

#Program

ShuffledDeck <- sample(Deck)

# DeckType <- c("Hearts","Diamonds","Clubs","Spades")

AlivePlayers <- length(Players)

CurrentPlayer <- 1;

CardGiven <- FALSE

while(AlivePlayers > 0)
{
  PlayerName <- Players[CurrentPlayer];
  PlayerScore <- PlayerScores[CurrentPlayer];
  
  if(PlayerScore < 21) {
    IsDealer <- (PlayerName == "Dealer")
    
    print(paste(PlayerName, "'s Turn (Score: ", PlayerScore, ")", sep = ""));
    
    GiveCard <- FALSE

    if(PlayerScore == 0) {
      GiveCard <- TRUE
    } else if(IsDealer) {     # If it is not the dealer,
      if(PlayerScore < 16) {  # let the player decide if they want a card
        GiveCard <- TRUE
      } else {
        print("The Dealer stays")
      }
    } else {
      entry <- "";
      while(entry != "y" && entry != "n") {
        entry <- readline(prompt = "Do you want a card? (y/n) ")
      }
      if(entry == "y") GiveCard <- TRUE
    }
    
    if(GiveCard) {
      CardGiven <- TRUE;
      
      Card <- ShuffledDeck[1];          # Pick the first card from the shuffled deck
      ShuffledDeck <- ShuffledDeck[-1]; # Remove this card from the deck
      
      PlayerScore <- PlayerScore + Card;
      
      PlayerScores[CurrentPlayer] <- PlayerScore;
      
      print(paste(PlayerName, " is given a", CardNames[Card], ". (Score:", PlayerScore, ")", sep = ""))
    }
  }
  
  CurrentPlayer <- CurrentPlayer + 1         # CurrentPlayer++
  if(CurrentPlayer > length(Players)) {      # If CurrentPlayer > Players.Length
    CurrentPlayer = 1;
    
    if(CardGiven == FALSE) {
      AlivePlayers <- -1;
    }
    
    CardGiven <- FALSE;
    
    text <- readline("Press enter to start next round");  # string text = Console.ReadLine()
    if(text == "stop") {
      AlivePlayers = -1;                                  # if "stop" entered, exit while loop
    }
  }
  print("");
}

print("Who is alive?")
print(table(Players, PlayerScores < 22))

print("");

WinIndex <- which(PlayerScores == max(PlayerScores[PlayerScores < 22]))

print(paste("Winner:", Players[WinIndex]))
