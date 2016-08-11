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

Using linear programming we set a number of constraints and a variable to optimise our selection on and the computer choses the best possible team based off of last seasons points tallies
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#### credit to \_\_\_ for the script on how to do this!

### Data load

``` r
df <- read_tsv("/Users/dom/Documents/FF/16-17/scrapes/16-08-11-scrape.txt")
```

### The vector to optimize on

``` r
objective <- df$total_points
```

### Fitting Constraints

``` r
num_gk <- 2
num_def <- 5
num_mid <- 5
num_fwd <- 3
max_cost <- 1000
```

### Create vectors to constrain by position

``` r
df$Goalkeeper <- ifelse(df$element_type == "GK", 1, 0)
df$Defender <- ifelse(df$element_type == "DEF", 1, 0)
df$Midfielder <- ifelse(df$element_type == "MID", 1, 0)
df$Forward <- ifelse(df$element_type == "FWD", 1, 0)
```

### Create constraint vectors to constrain by max number of players allowed per team

``` r
team_constraint <- unlist(lapply(unique(df$team.y), function(x, df){
  ifelse(df$team.y==x, 1, 0)
}, df=df))

# next we need the constraint directions
const_dir <- c("=", "=", "=", "=", rep("<=", 21))
```

### Now put the complete matrix together and solve

``` r
const_mat <- matrix(c(df$Goalkeeper, df$Defender, df$Midfielder, df$Forward,
                      df$now_cost, team_constraint),
                    nrow=(5 + length(unique(df$team.y))), byrow=TRUE)
const_rhs <- c(num_gk, num_def, num_mid, num_fwd, max_cost, rep(3, 20))

x <- lp ("max", objective, const_mat, const_dir, const_rhs, all.bin=TRUE, all.int=TRUE)
```

### The team!

``` r
df_out <- df[which(x$solution==1), c("first_name", "second_name", "now_cost", "team.y", "element_type")]

knitr::kable(df_out[order(df_out$element_type),])
```

| first\_name | second\_name |  now\_cost| team.y      | element\_type |
|:------------|:-------------|----------:|:------------|:--------------|
| Héctor      | Bellerín     |         65| Arsenal     | DEF           |
| Toby        | Alderweireld |         65| Tottenham   | DEF           |
| Robert      | Huth         |         50| Leicester   | DEF           |
| Wes         | Morgan       |         50| Leicester   | DEF           |
| Charlie     | Daniels      |         50| Bournemouth | DEF           |
| Romelu      | Lukaku       |         90| Everton     | FWD           |
| Odion       | Ighalo       |         75| Watford     | FWD           |
| Troy        | Deeney       |         70| Watford     | FWD           |
| Petr        | Cech         |         55| Arsenal     | GK            |
| Heurelho    | Gomes        |         50| Watford     | GK            |
| Eric        | Dier         |         55| Tottenham   | MID           |
| Riyad       | Mahrez       |         95| Leicester   | MID           |
| Georginio   | Wijnaldum    |         80| Liverpool   | MID           |
| André       | Ayew         |         75| West Ham    | MID           |
| Marko       | Arnautovic   |         75| Stoke       | MID           |
