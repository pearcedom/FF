
scrapeFF <- function(x){
    library(jsonlite)
    library(tibble)
    library(magrittr)
    ff.list <- fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")
    ff.dfr_base <- as_tibble(ff.list$elements)
    ff.dfr_base$element_type <- ifelse(ff.dfr_base$element_type == 1, "GK",
                                    ifelse(ff.dfr_base$element_type == 2, "DEF",
                                        ifelse(ff.dfr_base$element_type == 3, "MID", "FWD")))
    teams_dfr <- tibble(
        team_code = c(3, 91, 90, 8, 31, 20, 11, 43, 88, 13, 14, 110, 1, 25, 56, 80, 21, 6, 57, 35),
        team = c("Arsenal", "Bournemouth", "Burnley", "Chelsea", "Crystal Palace", "Southampton", "Everton", "Man City", "Hull", "Leicester", "Liverpool", "Stoke", "Man Utd", "Middlesbrough", "Sunderland", "Swansea", "West Ham", "Tottenham", "Watford", "West Brom"))
    merge(ff.dfr_base, teams_dfr, by = "team_code") %>% as_tibble()
}
