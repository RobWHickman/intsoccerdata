#function to convert messy formatted html to plaintext
extract_names <- function(text) {
  munged <- gsub("\\\n|\\\t", "", text)
}

#function to scrape the players and positions for both starters/subs (type)
#and away/home (team)
get_player_positions <- function(read, team, type, first11) {
  player <- read %>%
    html_nodes(paste(type, team, "a")) %>%
    html_text()
  if(length(player) == 0 & first11) player <- check_other_scrapes(read, team, type, "a")
  position <- read %>%
    html_nodes(paste(type, team, ".position")) %>%
    html_text()
  if(length(position) == 0 & first11) position <- check_other_scrapes(read, team, type, ".position")

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
check_other_scrapes <- function(read, team, type, extra) {
  player <- read %>%
    html_nodes(paste(team, ".player", extra)) %>%
    html_text()
  if(length(player) == 0) {
    player <- read %>%
      html_nodes(paste(".headers+ .lineup", team, extra)) %>%
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

#get the subs and when they came on
#merge into the df of players
add_sub_info <- function(players, read, team) {
  subs_on <- read %>%
    html_nodes(paste0(team, " .substitute")) %>%
    html_text() %>%
    extract_names()
  subs_off <- read %>%
    html_nodes(paste0(team, " .substituted")) %>%
    html_text() %>%
    extract_names()
  sub_time <- read %>%
    html_nodes(paste0(".substitutions ", team, " .time")) %>%
    html_text() %>%
    as.numeric()

  players$time_off[unlist(lapply(subs_off, grep, players$player))] <- sub_time
  players$time_on[unlist(lapply(subs_on, grep, players$player))] <- sub_time
  players$sub_off[unlist(lapply(subs_off, grep, players$player))] <- 1
  players$sub_on[unlist(lapply(subs_on, grep, players$player))] <- 1
  return(players)
}

merge_event_info <- function(players, events) {
  if(!all(is.na(players))) {
    events <- events[which(events$player %in% players$player),]
    events$player <- as.character(events$player)
    for(event_type in c("G", "Y", "R")) {
      if(any(events$event == event_type)) {
          players <- left_join(
            players,
            events %>%
              filter(player %in% players$player) %>%
              filter(event == event_type) %>%
              select(player) %>%
              table() %>%
              as.data.frame() %>%
              `names<-`(c("player", event_type)),
            by = "player"
          )
          na <- which(is.na(players[event_type]))
          players[event_type][na,] <- 0
      } else {
        merge <- data.frame(player = players$player,
                            event_type = 0) %>%
          `names<-`(c("player", event_type))
        players <- left_join(players, merge)
      }
    }
    return(players)
  } else {
    return(NA)
  }
}
