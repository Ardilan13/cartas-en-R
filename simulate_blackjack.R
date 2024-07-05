deck <- read.csv("cards.csv")

facecard <- deck$face %in% c("king", "queen", "jack")
deck$value[facecard] <- 10
deck$value[deck$face == "ace"] <- 11

DECK <- deck

deal <- function(no_cards) {
  if (no_cards == 1) {
    card <- deck[1, ]
    assign("deck", deck[-1, ], envir = globalenv())
  }
  
  else {
    card <- deck[1:no_cards, ]
    assign("deck", deck[-(1:no_cards), ], envir = globalenv())
  }
  
  card
}

shuffle <- function(){
  random <- sample(1:52, size = 52)
  assign("deck", DECK[random, ], envir = globalenv())
}

sum_of_hand <- function(hand) {
  
  aces <- FALSE
  aces_ind <- numeric()
  sumh <- 0

  # find aces
  for (k in 1:length(hand$value)) {
    if(hand$value[k] == 11) {
      aces <- TRUE
      aces_ind <- c(aces_ind, k)
    }
  }
  
  # if no aces were found, sum all items and return
  if (aces == FALSE) {
    return(sum(hand$value))
  }
  
  # otherwise aces were found so first sum items that are NOT aces
  not_aces <- subset(hand$value, !(hand$value %in% c(11)))
  sumh <- sum(not_aces)
  
  # if sum of non-aces > 10 all aces must be treated as 1's
  if (sumh > 10) {
    hand$value[hand$value == 11] <- 1
    return(sumh <- sum(hand$value))
  }
  
  # if sum of non-aces <= 9, need to check count of aces. 
  # If > 1 one can be treated as 11 while other must be treated as 1
  if (sumh <= 9 & length(aces_ind) > 1) {
    sumh <- sumh + 11
    for (i in 1:(length(aces_ind) - 1)) {
      sumh <- sumh + 1
    }
    return(sumh)
  }
  
  # if sum of non-aces == 10, need to check count of aces.
  # if == 1, then add 11 to sum and exit. if > 1, treat all aces as 1's
  if (sumh == 10 & length(aces_ind) == 1) {
    sumh <- sumh + 11
    return(sumh)
  } else {
    for (i in 1:length(aces_ind)) {
      sumh <- sumh + 1
    }
    return(sumh)
  }
  
}

player_hand <- function(hand, stand_at) {
  no_cards <- nrow(deck)
  stand <- FALSE
  current <- 0
  
  while (stand != TRUE) {
    current <- sum_of_hand(hand)
    # if card index == number of cards in decks, time to stop
    if (no_cards == 0) {
      stand <- TRUE
    }
    # else if hand is < stand_at value, take another card  
    else if (current < stand_at) {
      hand <- rbind(hand, deal(1))
    } 
    else {
      stand <- TRUE
    }
  }
  
  return(current)
}

dealer_hand <- function(hand) {
  no_cards <- nrow(deck)
  stand <- FALSE
  current <- 0
  
  while (stand != TRUE) {
    current <- sum_of_hand(hand)
    # if card index == number of cards in decks, time to stop
    if (no_cards == 0) {
      stand <- TRUE
    }
    # else if hand is <= 16, take another card  
    else if (current <= 16) {
      hand <- rbind(hand, deal(1))
    } 
    else {
      stand <- TRUE
    }
  }
  
  return(current)
}

play_blackjack <- function(bet, stand_at) {
  no_cards <- nrow(deck)
  winnings <- 0
  d_hand <- 0
  p_hand <- 0
  
  while (no_cards > 0) {
    
    if (no_cards >= 10) {
      p_hand <- player_hand(deal(2), stand_at)
    }
    
    else {
      return(winnings)
    }
    
    if (p_hand <= 21) {
      d_hand <- dealer_hand(deal(2))
    }
    
    if (p_hand > 21) {
      winnings <- winnings - bet
    }
    
    else if (d_hand > 21) {
      winnings <- winnings + bet
    }
    
   
   else if (p_hand > d_hand) {
    winnings <- winnings + bet
   }  
   
   else if (p_hand < d_hand) {
    winnings <- winnings - bet
   }
    
  no_cards <- nrow(deck)
  }
  return(winnings)
}
shuffle()

N <- 1000

# set size of bets
bet <- 2


# set hand total for player to stand at
p_stand <- 15

# create a vector to store results of play
res <- data.frame(N = 1:N, winnings = numeric(N))

for (i in 1:N){
  shuffle()
  res$winnings[i] <- play_blackjack(bet, p_stand)
}

avg_winnings <- mean(res$winnings)
message(paste("Average Winnings per Hand = ", avg_winnings))








