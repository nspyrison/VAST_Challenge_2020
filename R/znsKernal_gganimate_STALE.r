### Nicholas Spyrison for VAST Challenge 2020 MC1
### April 2020


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

## list some lat long. 
#head(df_templateSuspect[!is.na(df_templateSuspect$SourceLatitude), ])

### FORMATING FUNCTIONS =====
ns_df2igraph <- function(dat){
  if (is.data.frame(dat) == FALSE) try(dat <- as.data.frame(dat))
  if (is.data.frame(dat) == FALSE) stop("dat should be a data.frame.")
  requireNamespace("igraph")
  .nodes <- union(unique(dat$Source), unique(dat$Target))
  ## return network (igraph object)
  igraph::graph_from_data_frame(dat, directed = TRUE, vertices = .nodes)
}

ns_igraph2layout <- function(graph, 
                             layout = 'fr', ## also try 'kk', (circular = F)
                             circular = F, 
                             decode_tbl = NULL,
                             decode_tbl_id = "NodeID"){
  if (!is.null(decode_tbl) & !("igraph" %in% class(graph)) ) ## df or "igraph"
    stop("decode_tbl should be a data.frame.")
  
  .lay   <- create_layout(graph, layout = layout, circular = circular)
  if (!is.na(as.integer(.lay$name)))
    .lay$name <- as.integer(.lay$name)
  
  if (!is.null(decode_tbl)){
    .lay_lj   <- left_join(.lay, decode_tbl, by = c("name" = decode_tbl_id)) 
    ## decode_tbl_id is specific to decode_tbl, "name" is a layout default.
    .lay$nType   <- .lay_lj$NodeType
    .lay$nName   <- .lay_lj$Description
    .lay$nUsedIn <- .lay_lj$Used.in
  }
  ## Must keep layout attributes:
  #str(.lay) ## Good, kept attributes
  return(.lay)
}

ns_decode_tsne_obj <- function(tsne_input, tsne_output){
    decoded_input <- left_join(tsne_input, eType_tbl, by = "eType")
    tibble(x = tsne_output$Y[, 1],
           y = tsne_output$Y[, 2],
           eType       = decoded_input$eType,
           eName       = decoded_input$eName,
           DataSource  = decoded_input$DataSource,
           Weight      = decoded_input$Weight,
           Weight_unit = decoded_input$Weight_unit
    )
  }


### TEMPLATE-SUSPECT NEWTWORK VIS =====
if(do_run_ggraph == T){
  library(ggraph); library(igraph)
  .dat <- df_templateSuspect[df_templateSuspect$SourceDescription == "Person", ]
  .dat <- .dat[.dat$DataSource %in% subset_nms, ]
  
  
  ### _igraph route -----
  .igraph        <- ns_df2igraph(dat = .dat)
  ## want to find a vector of length 5697 giving eName/eType [1:6]
  .igraph_layout <- ns_igraph2layout(graph = .igraph, 
                                     layout = "fr", 
                                     circular = F, 
                                     decode_tbl = nodeType_tbl,
                                     decode_tbl_id = "NodeID")
  
  ## alt: try; layout = 'kk', circular = F
  #str(.lay) ## Good, kept attributes.
  ## For extending see:
  if (F)
    browseURL("http://users.dimi.uniud.it/~massimo.franceschet/ns/syllabus/make/ggraph/ggraph.html")
  
  ## ggraph sample
  .alp <- .35
  ggraph(.igraph_layout) + 
    ## Edges:
    geom_edge_link(aes(color = eName),
                   alpha = .alp, 
                   arrow = arrow(length = unit(2, 'mm')), 
                   end_cap = circle(2, 'mm')
    ) + 
    ## Nodes:
    geom_node_point(alpha = .alp,
                    size = 1.5,
                    aes(shape = nName,
                        color = nName,
                        fill  = nName,)
    ) +
    facet_graph(DataSource ~ nName) + 
    #facet_grid(DataSource ~ eName) +
    th_foreground(foreground = 'grey', border = TRUE) + 
    theme_graph()
  if (do_save_output == TRUE) {
    ggsave(filename = "network_grid_igrpah%02d.png",
           path = file.path("./output/"),
           plot = last_plot(),
           device = "png",
           units = "in",
           width = width_in,
           height = height_in)
    cat("NS: ggsave'd network_grid_igrpah_XX.png \n")
  }
  
  ### _ggraph route: -----

  library(tidygraph)
  .ggpraph <- as_tbl_graph(.dat)
  #attributes(gr) ## notice at active == "nodes"
  
  ## add Node attributes
  .ggpraph <- mutate(.ggpraph, name = as.integer(name))
  .ggpraph <- left_join(.ggpraph, nodeType_tbl, c("name" = "NodeID"))
  .ggpraph <- mutate(.ggpraph,
                     nType   = NodeType,
                     nName   = Description,
                     nUsedIn = Used.in,
                     nPopularity = 
                       as.character(cut(centrality_degree(mode = 'in'),
                                        breaks = 5,
                                        labels = paste0(c("1st",
                                                          "2nd",
                                                          "3rd",
                                                          "4th",
                                                          "5th"), " Quantile")))
  )
  
  ggraph(.ggpraph) +
    geom_edge_link(aes(color = eName),
                   alpha = .alp,
                   arrow = arrow(length = unit(2, 'mm')),
                   end_cap = circle(2, 'mm')
    ) +
    geom_node_point(alpha = .alp,
                    size = 1.5,
                    aes(shape = nName,
                        color = nName,
                        fill  = nName)
    ) +
    facet_graph(DataSource ~ nName) +
    #facet_grid(DataSource ~ eName) +
    th_foreground(foreground = 'grey', border = TRUE) + 
    theme_graph()
  
  if(do_save_output == TRUE){
    ggsave(filename = "network_grid_ggraph%02d.png",
           path = file.path("./output/"),
           plot = last_plot(),
           device = "png",
           units = "in",
           width = width_in,
           height = height_in)
    cat("NS: ggsave'd network_grid_ggrpah_XX.png \n")
  }

}


### TEMPLATE-SUSPECT EDGES tSNE =====
if(do_run_tsne == T){
  library("Rtsne")
  node_long_df <- pivot_longer(df_templateSuspect,
                               cols = Source:Target,
                               names_to = "Direction",
                               values_to = "NodeID")
  .dat <- select(node_long_df,
                 DataSource,
                 eName,
                 SecondsAfterStart,
                 NodeID,
                 Direction,
                 eType,
                 Weight
  )
  .dat$Direction <- as.factor(.dat$Direction)
  
  .DataSource_s <- unique(.dat$DataSource)
  .eName_s      <- unique(.dat$eName)
  table(.dat$eName)
  i_s <- 1:length(.DataSource_s)
  j_s <- 2:4#1:length(.eName_s)
  df_tsne <- NULL
  i<-1;j<-j_s[1]
  for(i in i_s) {
    for(j in j_s) {
      .x <- .dat
      .x <- .x[.x$DataSource == .DataSource_s[i],]
      .x <- .x[.x$eName == .eName_s[j],]
      .x <- select(.x,
                   SecondsAfterStart,
                   NodeID,
                   Direction,
                   eType,
                   Weight
      )

      ## tSNE doesn't like NAs, remove all rows with NA.
      k_s <- 1:ncol(.x)
      for(k in k_s){
        .x[, k] <- .x[, k][!is.na(.x[, k])]
      }
      tsne_edges <- .x
      tsne_obj <- Rtsne::Rtsne(tsne_edges, 
                               dims = 2,
                               perplexity = 1 / 3 * sqrt(nrow(tsne_edges)),
                               max_iter = 500,
                               check_duplicates = F,
                               pca = T,
                               verbose = TRUE,
                               theta = .5  ## [0, 1] increases speed at expense of accuracy
      )
      this_tsne <- ns_decode_tsne_obj(tsne_edges, tsne_obj)
      
      
      
    }
  }
  
  gg_tsne <- 
    ggplot(f_tsne_obj) +
    geom_point(
      aes(x = x,
          y = y,
          pch = DataSource,
          col = DataSource,
          fill = DataSource
      ),
      alpha = .3
    ) + theme_minimal() +
    theme(legend.position = "bottom")
  
  if(do_save_output == TRUE){
    ggsave(filename = "tSNE_%02d.png",
           path = file.path("./output/"),
           plot = last_plot(),
           device = "png",
           units = "in",
           width = width_in,
           height = height_in)
  }
}


