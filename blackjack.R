# Función para construir el mazo de cartas
build_deck <- function(cards_data) {
  deck <- list()
  for (i in seq_len(nrow(cards_data))) {
    card <- paste(cards_data$face[i], "of", cards_data$suit[i])
    value <- cards_data$value[i]
    deck[[i]] <- list(card = card, value = value)
  }
  return(deck)
}

# Función para barajar el mazo
shuffle_deck <- function(deck) {
  deck <- sample(deck)
  return(deck)
}

# Función para repartir cartas
deal_cards <- function(deck, num_cards) {
  cards_dealt <- deck[1:num_cards]
  remaining_deck <- deck[-(1:num_cards)]
  return(list(cards_dealt = cards_dealt, remaining_deck = remaining_deck))
}

# Función para revisar si es número positivo
is_positive_number <- function(value) {
  return(!is.na(value) && value > 0)
}

# Función para imprimir texto en colores específicos
color_text <- function(text, color) {
  color_code <- switch(tolower(color),
    red = "31",
    green = "32",
    yellow = "33",
    blue = "34",
    magenta = "35",
    cyan = "36",
    white = "37",
    black = "30",
    reset = "0"
  )

  cat(paste0("\033[1;", color_code, "m", text, "\033[0m\n"))
}

# Función para ingresar dinero con validación
enter_money <- function() {
  color_text(("Vamos a jugar 21!"), "blue")
  repeat {
    money <- as.numeric(readline(prompt = "¿Cuánto dinero desea ingresar al juego?: ")) # nolint: line_length_linter.
    if (!is_positive_number(money)) {
      color_text("Ingresó un valor no numérico o no válido. Intente nuevamente.", "red") # nolint
    } else {
      color_text(paste("Su dinero:", money), "green")
      return(money)
    }
  }
}

# Función para realizar apuesta con validación
place_bet <- function(money) {
  repeat {
    bet <- as.numeric(readline(prompt = "Ingrese su apuesta: "))
    if (!is_positive_number(bet)) {
      color_text("Ingresó un valor no numérico o no válido. Intente nuevamente.", "red") # nolint
    } else if (bet > money) {
      color_text("No tiene suficiente dinero para esta apuesta. Intente nuevamente.", "red") # nolint
      color_text(paste("\nSu dinero actual:", money), "green")
    } else {
      money <- money - bet
      color_text(paste("Ha apostado:", bet), "yellow")
      color_text(paste("Su dinero actual:", money), "green")
      return(list(bet = bet, money = money))
    }
  }
}

# Función para jugar un turno
play_turn <- function(money, deck) {
  color_text("\n--- Nuevo turno ---", "cyan")
  bet <- place_bet(money)$bet
  deck <- shuffle_deck(deck)
  card_index <- 1

  dealt_cards <- deal_cards(deck, 2)
  cat("\nCartas repartidas:\n")
  for (card in dealt_cards$cards_dealt) {
    color_text(paste(card$card, ":", card$value), "cyan")
  }

  # Calcular la suma de cartas al inicio del turno
  cards_sum <- sum_cards_value(dealt_cards$cards_dealt)
  color_text(paste("\nSuma de cartas:", cards_sum), "magenta")

  if (cards_sum >= 22) {
    color_text("Has perdido! La suma de tus cartas es mayor o igual a 22.", "red") # nolint: line_length_linter.
    color_text(paste("Su dinero actual:", money), "green")
    return(NULL)
  }

  repeat {
    choice <- as.numeric(readline(prompt = "¿Qué desea hacer? (1: Pedir otra carta, 2: Plantarse, 3: Retirarse): ")) # nolint: line_length_linter.
    if (choice == 1) {
      if (card_index <= length(deck)) {
        card_index <- card_index + 1 # Avanzar al siguiente índice de carta
        new_card <- deck[card_index]
        color_text(paste("Nueva carta:", new_card[[1]]$card, ":", new_card[[1]]$value), "cyan") # nolint: line_length_linter.
        dealt_cards$cards_dealt <- c(dealt_cards$cards_dealt, new_card)
        cards_sum <- sum_cards_value(dealt_cards$cards_dealt)
        color_text(paste("Nueva suma de cartas:", cards_sum), "magenta")
        if (cards_sum >= 22) {
          money <- money - bet
          color_text("\nHas perdido! La suma de tus cartas es mayor o igual a 22.", "red") # nolint: line_length_linter
          color_text(paste("\nSu dinero actual:", money), "green")
          return(list(money = money))
        }
      } else {
        color_text("\nNo quedan más cartas en el mazo.", "yellow")
        return(list(money = money))
      }
    } else if (choice == 2) {
      if (cards_sum < 17) {
        color_text("\nNo puedes plantarte con una suma menor a 17. Debes pedir otra carta.", "yellow") # nolint
      } else {
        cat("Te has plantado con una suma de:", cards_sum, "\n") # nolint: line_length_linter.
        if (cards_sum == 21) {
          money <- money + (bet * 2.5)
          color_text("\n¡Blackjack! Has ganado 2.5 veces tu apuesta.", "green")
          color_text(paste("Su dinero actual:", money), "green")
        } else {
          money <- money + (bet * 2)
          color_text(paste("\nHas ganado! Su dinero actual:", money), "green")
        }
        return(list(money = money))
      }
    } else if (choice == 3) {
      color_text("\nTe has retirado. Se ha devuelto su apuesta.", "yellow")
      color_text(paste("Dinero retirado:", money), "green")
      money <- 0
      return(list(money = money))
    } else {
      color_text("Opción no válida. Intente nuevamente.", "red")
    }
  }
}


# Función para sumar el valor de las cartas
sum_cards_value <- function(cards) {
  values <- sapply(cards, function(card) {
    value <- card$value
    if (is.na(value)) {
      return(0) # Manejar el caso de valor numérico ausente
    } else {
      if (value >= 10) {
        value <- 10
      } else if (value == 1) {
        value <- 11
      }
      return(value)
    }
  })

  total <- sum(values, na.rm = TRUE)

  # Si la suma es mayor a 21 y hay algún as, ajustamos el valor del as a 1
  while (total > 21 && any(values == 11)) {
    idx <- which(values == 11)[1]
    values[idx] <- 1
    total <- sum(values, na.rm = TRUE) # Recalcular la suma ajustada
  }

  return(total)
}



# Inicio del juego
money <- enter_money()
if (!is.null(money)) {
  setwd(".")
  deck <- read.csv("./cards.csv")
  deck <- build_deck(deck)
  while (money > 0) {
    money_after_turn <- play_turn(money, deck)
    if (is.null(money_after_turn)) {
      color_text("Juego terminado. Te has quedado sin dinero.", "red")
      break
    } else {
      money <- money_after_turn$money
    }
  }
  cat("Gracias por regalar su dinero!\n")
} else {
  cat("Juego cancelado. Ingresó un valor no válido.\n")
}
