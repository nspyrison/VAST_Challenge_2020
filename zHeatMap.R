## working from: https://www.data-to-viz.com/graph/heatmap.html

##### PREAMBLE =====
library(tictoc); library(beepr); library(tidyverse); library(lubridate)

#do_run_sizable_data  <- F ## See nsKernal.r for full set consumption
do_run_ggraph    <- TRUE
do_run_gganimate <- FALSE
do_run_tsne      <- TRUE
do_save_output   <- FALSE

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

# Load data 
data <- df_templateSuspect
sub <- select(data, DataSource, eName, Weight)
sub <- na.omit(sub)

agg <- group_by(sub, DataSource, eName) %>% 
  summarise(std_Weight = (Weight - mean(Weight)) / sd(Weight)) %>% 
  ungroup()

table(agg$DataSource, )
margin.table(agg,1)/margin.table(smoke)


agg <- na.omit(agg)
z <- agg[agg$std_Weight <5,]
ggplot(data = z, 
       mapping = aes(x = eName,
                     y = DataSource,
                     fill = std_Weight)) +
  geom_tile() 
table()

#+
  #xlab(label = "Sample")
library(heatmaply)
p <- heatmaply(mat, 
               dendrogram = "none",
               xlab = "", ylab = "", 
               main = "",
               scale = "column",
               margins = c(60,100,40,20),
               grid_color = "white",
               grid_width = 0.00001,
               titleX = FALSE,
               hide_colorbar = TRUE,
               branches_lwd = 0.1,
               label_names = c("Country", "Feature:", "Value"),
               fontsize_row = 5, fontsize_col = 5,
               labCol = colnames(mat),
               labRow = rownames(mat),
               heatmap_layers = theme(axis.line=element_blank())
)