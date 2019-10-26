#function to convert messy formatted html to plaintext
extract_names <- function(text) {
  munged <- gsub("\\\n|\\\t", "", text)
}

#function to scrape the players and positions for both starters/subs (type)
#and away/home (team)
get_player_positions <- function(read, team, type) {
  player <- read %>%
    html_nodes(paste(type, team, "a")) %>%
    html_text()
  if(length(player) == 0) player <- check_other_scrapes(read, team, type)
  position <- read %>%
    html_nodes(paste(type, team, ".position")) %>%
    html_text()
  if(length(position) == 0) position <- NA

  if(length(player) != length(position)) {
    check <- read %>%
      html_nodes(paste(type, team, ".player")) %>%
      html_text()

    player_positions <- match_player_positions(check, player, position)

  } else {
    player_positions <- data.frame(
      player,
      position
    )
  }
}

#function to match players and positions when lists are of different lengths
match_player_positions <- function(check, players, positions) {
  if(length(positions) < length(players)) {
    position_present <- unlist(lapply(check, function(x) any(unlist(lapply(positions, grepl, x)))))
    for(x in which(!position_present)) {
      positions <- append(positions, NA, x)
    }
    df <- data.frame(
      player = players,
      position = positions
    )
  } else {
    player_present <- unlist(lapply(check, function(x) any(unlist(lapply(players, grepl, x)))))
    for(x in which(!player_present)) {
      players <- append(players, NA, x)
    }
    df <- data.frame(
      player = players,
      position = positions
    )
  }
  return(df)
}

#function to check if any players are listed at all
check_players_present <- function(read, team) {
  x <- read %>%
    html_nodes(paste(team, ".player"))
  if(length(x) > 0) {
    return(1)
  } else {
    return(0)
  }
}

#function to try other nodes to scrape players
check_other_scrapes <- function(read, team, type) {
  player <- read %>%
    html_nodes(paste(team, ".player a")) %>%
    html_text()
  if(length(player) == 0) {
    player <- read %>%
      html_nodes(paste(team, ".player a")) %>%
      html_text()
    if(length(player) == 0) {
      return(NA)
    } else {
      return(player)
    }
  } else {
    return(player)
  }
}
