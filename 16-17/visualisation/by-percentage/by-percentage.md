The Wisdom of the Crowd
================
Dominic Pearce
11 August 2016

``` r
library(jsonlite)
library(tibble)
library(magrittr)
library(ggplot2)
library(ggthemes)
```

Found a much better (and simpler) source of data
------------------------------------------------

#### Needs a bit of reorganisation first

-   change position code to GK, DEF, etc...
-   change team code to team names
-   subset by useful columns (though this is unecessary)

``` r
ff.list <- fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")
ff.dfr_base <- as_tibble(ff.list$elements)
ff.dfr_base$element_type <- ifelse(ff.dfr_base$element_type == 1, "GK", 
                                   ifelse(ff.dfr_base$element_type == 2, "DEF", 
                                          ifelse(ff.dfr_base$element_type == 3, "MID", "FWD")))

teams_dfr <- tibble(team_code = c(3, 91, 90, 8, 31, 20, 11, 43, 88, 13, 14, 110, 1, 25, 56, 80, 21, 6, 57, 35), team = c("Arsenal", "Bournemouth", "Burnley", "Chelsea", "Crystal Palace", "Southampton", "Everton", "Man City", "Hull", "Leicester", "Liverpool", "Stoke", "Man Utd", "Middlesbrough", "Sunderland", "Swansea", "West Ham", "Tottenham", "Watford", "West Brom"))

ff.dfr_teams <- merge(ff.dfr_base, teams_dfr, by = "team_code") %>% as_tibble()

ff.dfr <- ff.dfr_teams[c("id", "team.y", "status", "first_name", "second_name", "now_cost", "value_form", "value_season", "selected_by_percent", "form", "total_points", "points_per_game", "minutes", "goals_scored", "assists", "clean_sheets", "goals_conceded", "own_goals", "penalties_saved", "penalties_missed", "yellow_cards", "red_cards", "saves", "bonus", "bps", "influence", "creativity", "threat", "element_type")]
```

Plot as percentage selected by position
---------------------------------------

#### Of players selected by at least 1% of the population

``` r
ggplot(ff.dfr[ff.dfr$selected_by_percent > 1,], aes(x = second_name, y = as.numeric(selected_by_percent), fill = team.y, colour = team.y)) +
  geom_bar(stat = 'identity', width = 0.03) + 
  geom_point(size = 3) + 
  facet_wrap(~element_type, ncol = 1, scales = 'free_x') +
  theme_pander() +
  theme(legend.position = 'bottom', 
        axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank()) +
  guides(fill = guide_legend(title = "Team"), colour = 'none') +
  ylab("Percentage Selected By")
```

![](by-percentage_files/figure-markdown_github/unnamed-chunk-3-1.png)
