##### PREAMBLE =====
library(tictoc); library(beepr); library(tidyverse); library(lubridate)

#### the lookup tables: eType_tbl, demographic_tbl, .demographic_tbl_Target,
## .demographic_tbl_Source, nodeType_tbl, .nodeType_tbl_Source, .nodeType_tbl_Target
load(file = "./Data/denormalizationLookupTables.rds")
## Read data frame object "df_templateSuspect"; Denormalized, but unaggregated data frame of the network data
load(file = "./Data/df_templateSuspect.rds") 

### AGGREGATION FOR GGANIMATION =====
.dat <- as.data.frame(df_templateSuspect)

node_long_df <- 
  pivot_longer(.dat,
               cols = Source:Target,
               names_to = "Direction",
               values_to = "NodeID")
node_long_df$Direction <- ifelse(node_long_df$Direction == "Source", "Sent", "Received")
node_long_df <- select(node_long_df, ## MAY NEED DENORMALIZATION LATER
                       DataSource,
                       Datetime,
                       eName,
                       NodeID,
                       Direction,
                       Weight,
                       Weight_unit
)

## Create a df of date levels to aggregate data over by animation frame.
##TODO: NEED TO SELECT SMARTER DATS DIST IS WHY OFF
frame_str <- data.frame(frame = 1:36, 
                        periodName = c(2001:2024, paste0(2025, "-", str_pad(1:12, 2,pad = "0"))),
                        periodEndDate = c(
                          paste0(2002:2025, "-01-01"),
                          paste0(2025, "-", str_pad(2:12, 2,pad = "0"), "01"),
                          "2026-01-01"))
frame_str$periodEndDate <- as_date(ymd(frame_str$periodEndDate) - seconds(1))
i_s <- 1:(nrow(frame_str) - 1)
.startDate <- min(.dat$Datetime)
for (i in i_s) {
  frame_str$periodStartDate[i] <- .startDate
  .startDate <- as_date(ymd(frame_str$periodEndDate[i]) + days(1))
}

## Agg table 1; sums by complex key
n_frames <- nrow(frame_str)
i_s <- 1L:n_frames
agg_tbl1 <- NULL
for (i in i_s) {
  .df <- node_long_df[node_long_df$Datetime <= frame_str$periodEndDate[i] &
                        node_long_df$Datetime >= frame_str$periodStartDate[i], ]
  
  .df_gp <-
    group_by(.df, DataSource, eName, NodeID, Direction, Weight_unit)
  .agg <-
    suppressMessages(
      summarise(.df_gp, 
                frame = i,
                compKey = paste0(i, DataSource, eName, NodeID, Direction),
                inc_Weight  = sum(Weight),
                inc_edges   = sum(1)
      )
    ) %>% 
    ungroup()
  
  
  agg_tbl1 <- rbind(agg_tbl1, .agg)
  print(c(i, i/n_frames, nrow(.agg), nrow(agg_tbl1)))
}
agg_tbl1

## Add in last and incremental values and join
agg_tbl2 <- NULL
.df <- select(agg_tbl1, frame, compKey, inc_Weight, inc_edges)
for (i in i_s) {
  ## Add last values
  if (i > 1){
    ## Select: compKey cumsum_Weight cumcnt_edges
    .this <- .df[.df$frame == i, ]
    .last <- .df[.df$frame == i - 1, ]
    .last <- select(.last, -frame)
    ## Correct compKey for the last frame to join. 
    .last_i_nchar  <- nchar(as.character(i - 1)) + 1
    .compKey_nchar <- nchar(.last$compKey)
    .last$compKey <- paste0(i, substr(.last$compKey, .last_i_nchar, .compKey_nchar))
    colnames(.last) <- c("compKey", "last_Weight", "last_edges")
    
    length(unique(.this$compKey))
    length(unique(.last$compKey))
    
    .lj <- left_join(.this, .last, by = "compKey")
    
    agg_tbl2 <- rbind(agg_tbl2, .lj)
    print(c(i, i/n_frames, nrow(.lj), nrow(agg_tbl2)))
  }
}
compKey_decode <- select(agg_tbl1, compKey, DataSource, eName, NodeID, Direction, Weight_unit)
agg_tbl3 <- left_join(agg_tbl2, compKey_decode, by = "compKey")
agg_tbl3 <- 
  select(agg_tbl3, 
         DataSource, eName, NodeID, Direction, frame, Weight_unit, 
         last_Weight, last_edges, inc_Weight, inc_edges)
### FINAL AGG TBL:
agg_tbl <- agg_tbl3


### CREATE LOOKUP TBL FOR NodeID minumum Datetime
node_minDate <- select(node_long_df, NodeID, Datetime) %>% 
  group_by(NodeID) %>%
  summarise(min_Datetime = min(Datetime)) %>% 
  ungroup()

save(agg_tbl, file = "./Data/agg_tbl.rds")
save(node_minDate, file = "./Data/node_minDate.rds")
cat("NS: saved ./Data/agg_tbl.rds \n")
cat("NS: saved ./Data/node_minDate.rds \n")
