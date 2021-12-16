#Execute after the "TableFunction.Rmd" module

Bat_chart_Compare <- function(batsman1, batsman2, batsman3, batsman4){
  filter(don, batsman == batsman1 | batsman == batsman2 | batsman == batsman3 | batsman == batsman4)
}

Bat_Stat_Compare <- function(batsman1, batsman2, batsman3, batsman4){
  a <- filter(don, batsman == batsman1)
  a$match_id <- seq.int(nrow(a))
  b <- filter(don, batsman == batsman2)
  b$match_id <- seq.int(nrow(b))
  c <- filter(don, batsman == batsman3)
  c$match_id <- seq.int(nrow(c))
  d <- filter(don, batsman == batsman4)
  d$match_id <- seq.int(nrow(d))
  a <- rbind(a, b)
  a <- rbind(a, c)
  a <- rbind(a, d)
}

Batting_standalone <- function(batsman1){
  a <- filter(don, batsman == batsman1)
  a$match_id <- seq.int(nrow(a))
  a
}
