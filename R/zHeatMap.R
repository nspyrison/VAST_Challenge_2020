## working from: https://www.data-to-viz.com/graph/heatmap.html

##### PREAMBLE =====
library(tictoc); library(beepr); library(tidyverse); library(lubridate)

subset_nms <- c("Template", paste0("Suspect", 1:5))
height_in  <- 2 * length(subset_nms) + 1 ## + 1 for template
width_in   <- 8

.template_filepath <- "./Submissions/MC1/data/CGCS-Template.csv"
.suspect_filepath_vect <- paste0("./Submissions/MC1/data/Q1-Graph", 1:5, ".csv")

#### the lookup tables: eType_tbl, demographic_tbl, .demographic_tbl_Target,
## .demographic_tbl_Source, nodeType_tbl, .nodeType_tbl_Source, .nodeType_tbl_Target
load(file = "./Data/denormalizationLookupTables.rds")
## Read data frame object "df_templateSuspect"; Denormalized, but unaggregated data frame of the network data
load(file = "./Data/df_templateSuspect.rds") 
## Read data frame object "agg_tbl"; aggregated data frame of the network data
load(file = "./Data/agg_tbl.rds") 
## Read data frame object "df_templateSuspect"; decode tbl for the min date each NodeID transacts.
load(file = "./Data/node_minDate.rds") 




library(tidyverse)
library(hrbrthemes)
library(viridis)
library(plotly)
library(d3heatmap)

## CLean 
## Rrmove location NULL columns
dat <- select(df_templateSuspect, Source:SourceNodeTypeUsedIn)
## Only complete rows
cl_dat <- dat[complete.cases(dat), ]
cl_dat$Weight <- abs(cl_dat$Weight)
data <- cl_dat

## Select req col
sub <- tibble::as.tibble(select(data, DataSource, eName, Weight))
sub
agg <- group_by(sub, DataSource, eName) %>% 
  summarise(sum_Weight = sum(Weight),
            cnt_edges = n()) %>% 
  ungroup()


ggplot(data = agg, 
       mapping = aes(x = eName,
                     y = DataSource,
                     fill = cnt_edges)) +
  geom_tile() + theme_minimal() + theme(legend.position = "bottom")
