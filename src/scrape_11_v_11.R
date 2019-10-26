get_basic_data <- function(read) {
  match_title <- read %>%
    html_nodes("h1") %>%
    html_text()

  basic_data <- read %>%
    html_nodes(".basicData") %>%
    html_text()

  teams <- read %>%
    html_nodes(".teamname a") %>%
    html_text()
  home <- teams[1]
  away <- teams[2]

  score <- read %>%
    html_nodes(".score") %>%
    html_text()
  h_goal <- score[1]
  a_goal <- score[2]

  h_manager <- read %>%
    html_nodes(".home b") %>%
    html_text()
  if(length(h_manager) == 0) h_manager <- NA
  a_manager <- read %>%
    html_nodes(".away b") %>%
    html_text()
  if(length(a_manager) == 0) a_manager <- NA

  df <- data.frame(
    title = match_title,
    data = basic_data,
    home,
    away,
    h_goal,
    a_goal,
    h_manager,
    a_manager
  )

  return(df)
}

get_events <- function(read) {
  goals <- read %>%
    html_nodes(".goals td") %>%
    html_text() %>%
    .[!grepl("\\\n", .)] %>%
    matrix(nrow = 2) %>%
    t() %>%
    as.data.frame()
  names(goals) <- c("player", "mins")
  if(nrow(goals) > 0) goals$event <- "G"

  cards <- read %>%
    html_nodes(".cards td") %>%
    html_text() %>%
    matrix(nrow = 3) %>%
    t() %>%
    as.data.frame()
  names(cards) <- c("player", "mins", "event")

  if(nrow(goals) > 0 & nrow(cards) > 0) {
    events <- rbind(goals, cards) %>%
      mutate(mins = as.numeric(as.character(mins)),
             event = as.character(event))

    events <- events[order(events$mins, str_order(events$event, decreasing = TRUE)),]
    return(events)
  } else {
    return(NA)
  }
}
