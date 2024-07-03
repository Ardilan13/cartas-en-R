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
    cyan = "36"
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
      color_text(paste("Su dinero actual:", money), "green")
      return(list(bet = bet, money = money))
    }
  }
}

# Función para calcular el valor de una carta en el contexto del juego
calculate_card_value <- function(card) {
  value <- card$value
  if (is.na(value)) {
    return(0) # Manejar el caso de valor numérico ausente
  } else {
    if (value >= 10) {
      return(10) # Figuras valen 10
    } else if (value == 1) {
      return(11) # As inicialmente vale 11
    } else {
      return(value) # Otros valores se mantienen igual
    }
  }
}

# Función para sumar el valor de las cartas ajustando Ases si es necesario
sum_cards_value <- function(cards) {
  values <- sapply(cards, calculate_card_value)
  total <- sum(values, na.rm = TRUE)
  # Si la suma es mayor a 21 y hay algún as, ajustamos el valor del as a 1
  while (total > 21 && any(values == 11)) {
    idx <- which(values == 11)[1]
    values[idx] <- 1
    total <- sum(values, na.rm = TRUE) # Recalcular la suma ajustada
  }
  return(total)
}

# Función para actualizar el conteo de cartas según el sistema Hi-Lo
update_count <- function(card, count) {
  value <- card$value
  if (value >= 2 && value <= 6) {
    count <- count + 1
  } else if (value >= 10 || value == 1) {
    count <- count - 1
  }
  return(count)
}

# Función para recomendar acción basada en conteo y suma de cartas del jugador
recommend_action <- function(player_sum, dealer_upcard_value, count) {
  true_count <- count / 52 # True count en un mazo único

  if (player_sum <= 11) {
    return("Recomendación: Pedir otra carta.")
  } else if (player_sum >= 17) {
    return("Recomendación: Plantarse.")
  } else if (player_sum >= 12 && player_sum <= 16) {
    if (dealer_upcard_value >= 7) {
      return("Recomendación: Pedir otra carta.")
    } else if (dealer_upcard_value >= 2 && dealer_upcard_value <= 6) {
      if (true_count >= 1) {
        return("Recomendación: Pedir otra carta.") # nolint: line_length_linter.
      } else if (true_count <= -1) {
        return("Recomendación: Pedir otra carta.")
      } else {
        return("Recomendación: Plantarse.")
      }
    } else {
      return("Recomendación: Plantarse.")
    }
  }
  return("Recomendación: Evaluar manualmente.")
}

# Función para jugar un turno
play_turn <- function(money, deck, count) {
  color_text("\n--- Nuevo turno ---", "cyan")
  bet <- place_bet(money)$bet
  deck <- shuffle_deck(deck)
  card_index <- 5 # Índice de la próxima carta a repartir

  # Repartir cartas al jugador
  dealt_cards_player <- deal_cards(deck, 2)

  # Repartir cartas a la casa
  dealt_cards_house <- deal_cards(dealt_cards_player$remaining_deck, 2)
  color_text("\n- Cartas de la casa -", "yellow")
  color_text(paste(dealt_cards_house$cards_dealt[[1]]$card, ":", calculate_card_value(dealt_cards_house$cards_dealt[[1]])), "yellow") # nolint: line_length_linter.
  hidden_card <- dealt_cards_house$cards_dealt[[2]] # Carta oculta de la casa
  color_text("*** **** **** : **", "yellow")

  color_text("\n- Cartas del jugador -", "cyan")
  for (card in dealt_cards_player$cards_dealt) {
    Sys.sleep(0.8)
    color_text(paste(card$card, ":", calculate_card_value(card)), "cyan")
    count <- update_count(card, count) # Actualizar conteo para cartas del jugador # nolint: line_length_linter.
  }
  Sys.sleep(0.5)

  # Calcular la suma de cartas del jugador
  cards_sum_player <- sum_cards_value(dealt_cards_player$cards_dealt)
  color_text(paste("Suma de cartas del jugador:", cards_sum_player), "magenta")

  # Mostrar recomendación al jugador
  recommendation <- recommend_action(cards_sum_player, calculate_card_value(dealt_cards_house$cards_dealt[[1]]), count) # nolint
  cat("\n")
  color_text(recommendation, "blue")

  if (cards_sum_player >= 22) {
    money <- money - bet
    color_text("Has perdido! La suma de tus cartas es mayor o igual a 22.", "red") # nolint: line_length_linter.
    color_text(paste("Su dinero actual:", money), "green")
    return(list(money = money, count = count))
  }

  # Turno del jugador
  repeat {
    choice <- as.numeric(readline(prompt = "¿Qué desea hacer? (1: Pedir otra carta, 2: Plantarse, 3: Retirarse): ")) # nolint: line_length_linter.
    if (choice == 1) {
      if (card_index <= length(deck)) {
        new_card <- deck[card_index]
        Sys.sleep(0.8)
        color_text(paste("Nueva carta:", new_card[[1]]$card, ":", calculate_card_value(new_card[[1]])), "cyan") # nolint: line_length_linter.
        dealt_cards_player$cards_dealt <- c(dealt_cards_player$cards_dealt, new_card) # nolint: line_length_linter.
        cards_sum_player <- sum_cards_value(dealt_cards_player$cards_dealt)
        count <- update_count(new_card[[1]], count) # Actualizar conteo para nueva carta # nolint: line_length_linter.
        Sys.sleep(0.5)
        color_text(paste("Nueva suma de cartas:", cards_sum_player), "magenta")
        Sys.sleep(0.5)
        if (cards_sum_player >= 22) {
          money <- money - bet
          color_text("\nHas perdido! La suma de tus cartas es mayor o igual a 22.", "red") # nolint: line_length_linter
          color_text(paste("\nSu dinero actual:", money), "green")
          return(list(money = money, count = count))
        } else {
          # Mostrar recomendación al jugador
          recommendation <- recommend_action(cards_sum_player, calculate_card_value(dealt_cards_house$cards_dealt[[1]]), count) # nolint
          cat("\n")
          color_text(recommendation, "blue")
        }
        card_index <- card_index + 1
      } else {
        color_text("\nNo quedan más cartas en el mazo.", "yellow")
        return(list(money = money, count = count))
      }
    } else if (choice == 2) {
      color_text(paste("Te has plantado con una suma de:", cards_sum_player), "cyan") # nolint: line_length_linter.
      break
    } else if (choice == 3) {
      color_text("\nTe has retirado. Se ha devuelto su apuesta.", "yellow")
      color_text(paste("Dinero retirado:", money), "green")
      money <- 0
      return(list(money = money, count = count))
    } else {
      color_text("Opción no válida. Intente nuevamente.", "red")
    }
  }

  # Turno de la casa
  color_text("\n--- Turno de la casa ---", "yellow")
  color_text(paste("Primera carta de la casa: ", dealt_cards_house$cards_dealt[[1]]$card, ":", calculate_card_value(dealt_cards_house$cards_dealt[[1]])), "yellow") # nolint
  color_text(paste("Carta oculta de la casa: ", hidden_card$card, ":", calculate_card_value(hidden_card)), "yellow") # nolint
  count <- update_count(hidden_card, count) # Actualizar conteo para carta oculta # nolint: line_length_linter.
  for (card in dealt_cards_house$cards_dealt) {
    count <- update_count(card, count) # Actualizar conteo para cartas visibles de la casa # nolint: line_length_linter.
  }

  cards_sum_house <- sum_cards_value(dealt_cards_house$cards_dealt)
  while (cards_sum_house < 17) {
    new_card <- deck[card_index]
    Sys.sleep(0.8)
    color_text(paste("Casa pide nueva carta:", new_card[[1]]$card, ":", calculate_card_value(new_card[[1]])), "yellow") # nolint: line_length_linter
    dealt_cards_house$cards_dealt <- c(dealt_cards_house$cards_dealt, new_card)
    cards_sum_house <- sum_cards_value(dealt_cards_house$cards_dealt)
    count <- update_count(new_card[[1]], count) # Actualizar conteo para nueva carta de la casa # nolint
    card_index <- card_index + 1
  }

  Sys.sleep(0.5)
  color_text(paste("Suma final de cartas de la casa:", cards_sum_house), "magenta") # nolint: line_length_linter.
  Sys.sleep(0.5)
  # Comparar resultados
  if (cards_sum_player == 21 && cards_sum_house != 21) {
    money <- money + 1.5 * bet
    color_text(paste("\n¡Blackjack! Has ganado 2.5 veces. Su dinero actual:", money), "green") # nolint
  } else if (cards_sum_house >= 22 || cards_sum_player > cards_sum_house) {
    money <- money + bet
    color_text(paste("\n¡Has ganado! Su dinero actual:", money), "green")
  } else if (cards_sum_player == cards_sum_house) {
    color_text(paste("\nEmpate. Su dinero actual:", money), "yellow")
  } else {
    money <- money - bet
    color_text(paste("\nHas perdido contra la casa. Su dinero actual:", money), "red") # nolint: line_length_linter.
  }
  return(list(money = money, count = count))
}

# Inicio del juego
money <- enter_money()
if (!is.null(money)) {
  setwd("C:/Users/Dilan/Documents/cartas-en-R")
  deck <- read.csv("./cards.csv")
  deck <- build_deck(deck)
  count <- 0 # Inicializar el conteo de cartas
  while (money > 0) {
    turn_result <- play_turn(money, deck, count)
    if (is.null(turn_result)) {
      color_text("Juego terminado. Te has quedado sin dinero.", "red")
      break
    } else {
      money <- turn_result$money
      count <- turn_result$count
    }
  }
  cat("Gracias por su dinero!\n")
} else {
  cat("Juego cancelado. Ingresó un valor no válido.\n")
}
