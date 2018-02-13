Players <- c("Player 1", "Player 2", "Player 3", "Player 4");

CardSet <- c("A", "K", "Q", "J", 2:10);

Cards <- c(CardSet, CardSet, CardSet, CardSet);
Shuffled <- sample(Cards);

KingsFound <- 0;
CurrentPlayer <- 1;

while (KingsFound < 4 && length(Shuffled) > 0) {
  print(paste(Players[CurrentPlayer], "'s Turn", sep = ""));
	print("Press enter to pick a card...");
	readline();
  CurrentPlayer <- CurrentPlayer + 1;
  if(CurrentPlayer > length(Players)) {
    CurrentPlayer = 1;
  }
	Picked <- Shuffled[1];
	Shuffled <- Shuffled[-1];
	print(paste("Picked Card:", Picked));
	if (Picked == "K") {
		KingsFound <- KingsFound + 1;
	}
	print("");
	print("---");
	print("");
}

print("GAME OVER! Start drinking...")
