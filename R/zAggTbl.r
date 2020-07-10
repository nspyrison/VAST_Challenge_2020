library("dplyr")
library('tidyr')

df <- read.csv2(file = "./SampleData/MC1/ProcurementsSample.csv", sep = ",")
#tgtTimes <- as.integer(names(table(df$Time)[1:5]))
#sub <- df[df$Time %in% tgtTimes, ]
sub <- df

sub <- pivot_longer(sub, cols = c("Source", "Target"), names_to = "Direction", values_to = "NodeID")
sub <- select(sub, NodeID, Direction, Time, Weight)

.t <- table(sub$NodeID)
.t_gt1 <- .t[.t > 1]
tgtNodes <- as.integer(names(.t_gt1))
sub <- sub[sub$NodeID %in% tgtNodes, ]

t_s <- sort(unique(sub$Time))
i_s <- 1:length(t_s)
.l <- length(i_s)
agg_tbl <- NULL
for (i in i_s){
  x <- sub[sub$Time <= t_s[i], ]
  agg_row <- group_by(x, NodeID, Direction, Time) %>%
    try(summarise(sum_weight = sum(Weight),
                  ), 
        silent = T) %>% 
  ungroup()
  print(c(nrow(agg_row), i, i / .l))
  agg_row <- data.frame(agg_row, LastWeight = NaN)
  if(i > 1){
    last_row <- agg_tbl[agg_tbl$Time == t_s[i-1], ]
  }
  
  
  agg_tbl <- rbind(agg_tbl, agg_row)
}



