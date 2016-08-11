Auto-team Selection
================
Dominic Pearce

``` r
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
```

``` r
library(lpSolve)
library(stringr)
library(RCurl)
library(jsonlite)
library(plyr)
library(readr)
```

``` r
dfr <- read_tsv("/Users/dom/Documents/FF/16-17/scrapes/16-08-11-scrape.txt")
dfr[c("web_name", "now_cost")]
```

    ## # A tibble: 505 x 2
    ##          web_name now_cost
    ##             <chr>    <int>
    ## 1    Schneiderlin       50
    ## 2            Rojo       55
    ## 3         Pereira       45
    ## 4         Carrick       45
    ## 5  Schweinsteiger       55
    ## 6         Januzaj       55
    ## 7         Martial       95
    ## 8           Pogba       85
    ## 9         Herrera       65
    ## 10       Valencia       55
    ## # ... with 495 more rows

``` r
df <- read_tsv("/Users/dom/Documents/FF/16-17/scrapes/16-08-11-scrape.txt")
```

``` r
# The vector to optimize on
objective <- df$total_points

# Fitting Constraints
num_gk <- 2
num_def <- 5
num_mid <- 5
num_fwd <- 3
max_cost <- 1000
```

``` r
# Create vectors to constrain by position
df$Goalkeeper <- ifelse(df$element_type == "GK", 1, 0)
df$Defender <- ifelse(df$element_type == "DEF", 1, 0)
df$Midfielder <- ifelse(df$element_type == "MID", 1, 0)
df$Forward <- ifelse(df$element_type == "FWD", 1, 0)

# Create constraint vectors to constrain by max number of players allowed per team
team_constraint <- unlist(lapply(unique(df$team.y), function(x, df){
  ifelse(df$team.y==x, 1, 0)
}, df=df))

# next we need the constraint directions
const_dir <- c("=", "=", "=", "=", rep("<=", 21))
```

``` r
# Now put the complete matrix together
const_mat <- matrix(c(df$Goalkeeper, df$Defender, df$Midfielder, df$Forward,
                      df$now_cost, team_constraint),
                    nrow=(5 + length(unique(df$team.y))), byrow=TRUE)
const_rhs <- c(num_gk, num_def, num_mid, num_fwd, max_cost, rep(3, 20))
```

``` r
# then solve the matrix
x <- lp ("max", objective, const_mat, const_dir, const_rhs, all.bin=TRUE, all.int=TRUE)
```

``` r
# And this is our team!
print(arrange(df[which(x$solution==1),], desc(Goalkeeper), desc(Defender),
              desc(Midfielder), desc(Forward), desc(total_points)))
```

    ## # A tibble: 15 x 62
    ##    team_code    id      photo     web_name status   code first_name
    ##        <int> <int>      <chr>        <chr>  <chr>  <int>      <chr>
    ## 1          3     2  11334.jpg         Cech      a  11334       Petr
    ## 2         57   407  18656.jpg        Gomes      a  18656   Heurelho
    ## 3          3     6  98745.jpg     Bellerín      a  98745     Héctor
    ## 4          6   382  55605.jpg Alderweireld      a  55605       Toby
    ## 5         13   173  12413.jpg         Huth      s  12413     Robert
    ## 6         13   167  15033.jpg       Morgan      a  15033        Wes
    ## 7         91    33  41320.jpg      Daniels      a  41320    Charlie
    ## 8         13   176 103025.jpg       Mahrez      a 103025      Riyad
    ## 9         21   374  45124.jpg         Ayew      a  45124      André
    ## 10        14   485  41733.jpg    Wijnaldum      a  41733  Georginio
    ## 11       110   331  41464.jpg   Arnautovic      d  41464      Marko
    ## 12         6   390  93264.jpg         Dier      a  93264       Eric
    ## 13        11   143  66749.jpg       Lukaku      d  66749     Romelu
    ## 14        57   426  58498.jpg       Ighalo      a  58498      Odion
    ## 15        57   425  41725.jpg       Deeney      a  41725       Troy
    ## # ... with 55 more variables: second_name <chr>, squad_number <int>,
    ## #   news <chr>, now_cost <int>, chance_of_playing_this_round <chr>,
    ## #   chance_of_playing_next_round <int>, value_form <dbl>,
    ## #   value_season <dbl>, cost_change_start <int>, cost_change_event <int>,
    ## #   cost_change_start_fall <int>, cost_change_event_fall <int>,
    ## #   in_dreamteam <lgl>, dreamteam_count <int>, selected_by_percent <dbl>,
    ## #   form <dbl>, transfers_out <int>, transfers_in <int>,
    ## #   transfers_out_event <int>, transfers_in_event <int>, loans_in <int>,
    ## #   loans_out <int>, loaned_in <int>, loaned_out <int>,
    ## #   total_points <int>, event_points <int>, points_per_game <dbl>,
    ## #   ep_this <chr>, ep_next <dbl>, special <lgl>, minutes <int>,
    ## #   goals_scored <int>, assists <int>, clean_sheets <int>,
    ## #   goals_conceded <int>, own_goals <int>, penalties_saved <int>,
    ## #   penalties_missed <int>, yellow_cards <int>, red_cards <int>,
    ## #   saves <int>, bonus <int>, bps <int>, influence <dbl>,
    ## #   creativity <dbl>, threat <dbl>, ict_index <dbl>, ea_index <int>,
    ## #   element_type <chr>, team.x <int>, team.y <chr>, Goalkeeper <dbl>,
    ## #   Defender <dbl>, Midfielder <dbl>, Forward <dbl>

``` r
print(str_c('Total Price: ', sum(df[which(x$solution==1), 'now_cost'])))
```

    ## [1] "Total Price: 1000"

``` r
print(str_c('Total Points: ', sum(df[which(x$solution==1), 'total_points'])))
```

    ## [1] "Total Points: 2465"
