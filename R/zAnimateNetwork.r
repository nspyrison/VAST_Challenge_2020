library(tidyr)
library(gganimate)
library(ggraph)
library(igraph)
df <- ggraph::highschool
str(df)
table(df$year)
#View(df)
df$year[1:20] <- df$year[1:20] - 2
.tail <- nrow(df) - 30:nrow(df)
df$year[.tail] <- df$year[.tail] + 3

myWidth <- 1 + scales::rescale(df$year - min(df$year))
# require(tidygraph)
# layout <- create_layout(df, layout = 'igraph', algorithm = 'kk')


ggraph(graph = df, layout = "igraph", algorithm = 'kk') +
  geom_node_point(color = "blue", size =3) +
  geom_edge_link(show.legend = T, width = 1) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  transition_states(year, transition_length = 0, state_length = 3) +
  ggtitle(paste0("time point: ", "{closest_state}"))


########

## Read the lookup tables into the global envirnment:
#### eType_tbl, 
#### demographic_tbl, .demographic_tbl_Target, .demographic_tbl_Source, 
#### nodeType_tbl, .nodeType_tbl_Source, .nodeType_tbl_Target
load(file = "./Data/denormalizationLookupTables.rds")
## Read data frame object "df_templateSuspect" into the global envirnment:
#### Denormalized, but unaggregated data frame of the network data
load(file = "./Data/df_templateSuspect.rds") 
