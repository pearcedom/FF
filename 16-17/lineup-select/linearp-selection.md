Auto-team Selection, Pre-season
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
library(purrr)
library(tibble)
```

### Using linear programming we set a number of constraints and a variable to optimise our selection on and the computer choses the best possible team based off of last seasons points tallies

Credit to \_\_\_ for the script on how to do this!

### Data load

``` r
df <- read_tsv("/Users/dom/Documents/FF/16-17/scrapes/16-08-11-scrape.txt")
```

### Optimisation variable

Total points are what we really care about here, anything else (goals, assists, etc.) is secondary

``` r
objective <- df$total_points
```

### Fitting Constraints

Teams must consist of a 15-man squad, 2xGK, 5xDEF, 5xMID and 3xATK. The maximum spend is £100 million. We'll also constrain on chance of playing next round.

*note:* for whatever reason the player data is given in units of 100,000s

``` r
#position allowances
num_gk <- 2
num_def <- 5
num_mid <- 5
num_fwd <- 3

#binary vectors specifying which rows (i.e. which players) belong to a given position
df$Goalkeeper <- ifelse(df$element_type == "GK", 1, 0)
df$Defender <- ifelse(df$element_type == "DEF", 1, 0)
df$Midfielder <- ifelse(df$element_type == "MID", 1, 0)
df$Forward <- ifelse(df$element_type == "FWD", 1, 0)

likelihood <- df$chance_of_playing_next_round
df$likelihood <- ifelse(is.na(likelihood), 0, ifelse(likelihood >= 75, 0, 1))

#budget
max_cost <- 1000
```

Additionally, a maximum of three players from the same team can be selected

``` r
team_constraint <- sapply(unique(df$team.y), function(x) ifelse(df$team.y == x, 1, 0), USE.NAMES = TRUE) %>% as_tibble %>% t
```

### Combine all the information so far into a single matrix

This results in a 505-column (one per player) and 25-row (1 per variable (4 x position, 1 x current cost, 20 x team)) matrix

``` r
const_mat <- rbind(df$Goalkeeper, df$Defender, df$Midfielder, df$Forward, df$likelihood, df$now_cost, team_constraint)
```

### Lastly for the constraints, we specify their directions and define the righthand-side values

``` r
const_dir <- c("=", "=", "=", "=","<=", rep("<=", 21))
const_rhs <- c(num_gk, num_def, num_mid, num_fwd, 2, max_cost, rep(3, 20))
#i.e. =2GK, =5DEF, =5MID, =3ATK, =£100m, <=2 injured players, <=three players from a single team

rbind(const_dir, const_rhs)
```

    ##           [,1] [,2] [,3] [,4] [,5] [,6]   [,7] [,8] [,9] [,10] [,11] [,12]
    ## const_dir "="  "="  "="  "="  "<=" "<="   "<=" "<=" "<=" "<="  "<="  "<=" 
    ## const_rhs "2"  "5"  "5"  "3"  "2"  "1000" "3"  "3"  "3"  "3"   "3"   "3"  
    ##           [,13] [,14] [,15] [,16] [,17] [,18] [,19] [,20] [,21] [,22]
    ## const_dir "<="  "<="  "<="  "<="  "<="  "<="  "<="  "<="  "<="  "<=" 
    ## const_rhs "3"   "3"   "3"   "3"   "3"   "3"   "3"   "3"   "3"   "3"  
    ##           [,23] [,24] [,25] [,26]
    ## const_dir "<="  "<="  "<="  "<=" 
    ## const_rhs "3"   "3"   "3"   "3"

### And solve

Here we feed the function - "max": specifying that we want as much as possible - "objective": based on total score - "const\_mat": our constraint vectors all housed together - "const\_dir": the direction of our constraints (i.e. whether it's less than/equal to) - "const\_rhs": the righthand-side values (i.e. the value that it must be less than/equal to) - "all.bin": specifies that all variables are binary - "all.int": specifies that all variables are integers

``` r
x <- lp("max", objective, const_mat, const_dir, const_rhs, all.bin=TRUE, all.int=TRUE)
```

### The squad!

``` r
df_out <- df[which(x$solution==1), c("first_name", "second_name", "now_cost", "team.y", "element_type", "total_points")]

knitr::kable(df_out[order(df_out$element_type),])
```

| first\_name | second\_name |  now\_cost| team.y      | element\_type |  total\_points|
|:------------|:-------------|----------:|:------------|:--------------|--------------:|
| Héctor      | Bellerín     |         65| Arsenal     | DEF           |            172|
| Toby        | Alderweireld |         65| Tottenham   | DEF           |            166|
| Robert      | Huth         |         50| Leicester   | DEF           |            140|
| Wes         | Morgan       |         50| Leicester   | DEF           |            139|
| Charlie     | Daniels      |         50| Bournemouth | DEF           |            130|
| Romelu      | Lukaku       |         90| Everton     | FWD           |            185|
| Odion       | Ighalo       |         75| Watford     | FWD           |            175|
| Troy        | Deeney       |         70| Watford     | FWD           |            166|
| Petr        | Cech         |         55| Arsenal     | GK            |            159|
| Heurelho    | Gomes        |         50| Watford     | GK            |            157|
| Eric        | Dier         |         55| Tottenham   | MID           |            130|
| Riyad       | Mahrez       |         95| Leicester   | MID           |            240|
| Georginio   | Wijnaldum    |         80| Liverpool   | MID           |            170|
| André       | Ayew         |         75| West Ham    | MID           |            171|
| Marko       | Arnautovic   |         75| Stoke       | MID           |            165|

### We still need to decided on an 11 however, we quickly rework the solution, adding in the following constraints

-   11 men
-   no injuries (75% chance to play allowed)
-   total cost constraint is removed

``` r
team_df <- df[which(x$solution==1),]

objective <- team_df$total_points

team_constraint <- sapply(unique(team_df$team.y), function(x) ifelse(team_df$team.y == x, 1, 0), USE.NAMES = TRUE) %>% as_tibble %>% t

const_mat <- rbind(team_df$Goalkeeper, team_df$Defender, team_df$Defender, team_df$Midfielder, team_df$Midfielder, team_df$Forward, team_df$Forward, team_df$likelihood, team_constraint, 1)

const_dir <- c("=", "<=",  ">=", "<=", ">=", "<=", ">=", "=", rep("<=", length(unique(team_df$team.y))), "=")
const_rhs <- c( 1, 5, 3, 5, 2, 3, 1, 0, rep(3, length(unique(team_df$team.y))), 11)

team_x <- lp("max", objective, const_mat, const_dir, const_rhs, all.bin=TRUE, all.int=TRUE)

team_df_out <- team_df[which(team_x$solution==1), c("first_name", "second_name", "now_cost", "team.y", "element_type", "total_points")]

knitr::kable(team_df_out[order(team_df_out$element_type),])
```

| first\_name | second\_name |  now\_cost| team.y    | element\_type |  total\_points|
|:------------|:-------------|----------:|:----------|:--------------|--------------:|
| Héctor      | Bellerín     |         65| Arsenal   | DEF           |            172|
| Toby        | Alderweireld |         65| Tottenham | DEF           |            166|
| Wes         | Morgan       |         50| Leicester | DEF           |            139|
| Romelu      | Lukaku       |         90| Everton   | FWD           |            185|
| Odion       | Ighalo       |         75| Watford   | FWD           |            175|
| Troy        | Deeney       |         70| Watford   | FWD           |            166|
| Petr        | Cech         |         55| Arsenal   | GK            |            159|
| Riyad       | Mahrez       |         95| Leicester | MID           |            240|
| Georginio   | Wijnaldum    |         80| Liverpool | MID           |            170|
| André       | Ayew         |         75| West Ham  | MID           |            171|
| Marko       | Arnautovic   |         75| Stoke     | MID           |            165|

### Captain Selection

For the first week this will just be based on highest total score last year - **Mahrez** (*Lukaku VC*)

``` r
team_df_out$second_name[order(team_df_out$total_points, decreasing = TRUE)][1:2]
```

    ## [1] "Mahrez" "Lukaku"
