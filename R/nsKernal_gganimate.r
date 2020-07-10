### Nicholas Spyrison for VAST Challenge 2020 MC1
### April 2020


##### PREAMBLE =====
library(tictoc); library(beepr); library(tidyverse); library(lubridate)

#do_run_sizable_data  <- F ## See nsKernal.r for full set consumption
do_run_ggraph                   <- TRUE
do_run_gganimate                <- FALSE
do_run_tsne                     <- TRUE
do_save_output                  <- FALSE

subset_nms <- c("Template", paste0("Suspect", 1:5))
height_in  <- 2 * length(subset_nms) + 1 ## + 1 for template
width_in   <- 8

.template_filepath <- "./Submissions/MC1/data/CGCS-Template.csv"
.suspect_filepath_vect <- paste0("./Submissions/MC1/data/Q1-Graph", 1:5, ".csv")

## Read the lookup tables into the global envirnment:
#### eType_tbl, 
#### demographic_tbl, .demographic_tbl_Target, .demographic_tbl_Source, 
#### nodeType_tbl, .nodeType_tbl_Source, .nodeType_tbl_Target
load(file = "./Data/denormalizationLookupTables.rds")
## Read data frame object "df_templateSuspect" into the global envirnment:
#### Denormalized, but unaggregated data frame of the network data
load(file = "./Data/df_templateSuspect.rds") 
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
  
  # message("Scope of data:")
  # summary(.dat[c("eType", "Datetime", "DataSource")])
  # table(.dat[c("eName","DataSource")])
  
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

### AGGREGATION FOR GGANIMATION =====
if(do_run_gganimate == TRUE){
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
  ##TODO: NEED TO SLECT SMARTER DATS DIST IS WHY OFF
  frame_str <- data.frame(frame = 1:36, 
                          periodName = c(2001:2024, paste0(2025, "-", str_pad(1:12, 2,pad = "0"))),
                          periodEndDate = c(
                            paste0(2002:2025, "-01-01"),
                            paste0(2025, "-", str_pad(2:12, 2,pad = "0"), "01"),
                            "2026-01-01"))
  frame_str$periodEndDate <- as_date(ymd(frame_str$periodEndDate) - seconds(1))
  
  ## Agg table 1; sums by complex key
  n_frames <- nrow(frame_str)
  i_s <- 1L:n_frames
  agg_tbl1 <- NULL
  for (i in i_s) {
    .df <- node_long_df[node_long_df$Datetime <= frame_str$periodEndDate[i], ]
    
    .df_gp <-
      group_by(.df, DataSource, eName, NodeID, Direction, Weight_unit)
    .agg <-
        suppressMessages(
          summarise(.df_gp, 
                    frame = i,
                    compKey = paste0(DataSource, eName, NodeID, Direction, i),
                    sum_Weight  = sum(Weight),
                    inc_Weight  = NaN,
                    last_Weight = NaN,
                    cnt_edges   = sum(1), 
                    inc_Weight  = NaN,
                    last_Weight = NaN)) %>% 
      ungroup()
    
    
    agg_tbl1 <- rbind(agg_tbl1, .agg)
    print(c(i, i/n_frames, nrow(.agg), nrow(agg_tbl1)))
  }
  
  ## Add in last and incremental values and join
  agg_tbl2 <- NULL
  for (i in i_s) {
    ## Add last values
    if(i > 1){
      ## Select: compKey cumsum_Weight cumcnt_edges
      .df   <- select(agg_tbl1, frame, compKey, sum_Weight, cnt_edges)
      .this <- .df[.df$frame == i, ]
      .last <- .df[.df$frame == i - 1, ]
      .last <- select(.last, -frame)
      ## Correct compKey for the last frame to join. 
      .last_i_nchar <- nchar(as.character(i - 1)) + 1
      .compKey_nchar <- nchar(sub_last$compKey)
      .last$compKey <- 
        paste0(i, substr(.last$compKey, .last_i_nchar, .compKey_nchar))
      colnames(.last) <- c("compKey", "last_Weight", "last_edges")
      
      # u_sub      <- unique(sub$compKey)
      # u_sub_last <- unique(sub_last$compKey)
      # length(u_sub)
      # length(u_sub_last)
      
      .lj <- left_join(.this, .last, by = "compKey")
      .lj <- mutate(.lj,
                    inc_Weight = sum_Weight - last_Weight,
                    inc_edges  = cnt_edges  - last_edges)
      
      agg_tbl2 <- rbind(agg_tbl2, .lj)
      print(c(i, i/n_frames, nrow(.lj), nrow(agg_tbl2)))
    }
  }
  ## CONTINUE WORKING HERE, when i =issue on aggrgation; 
  dim(agg_tbl1)
  dim(agg_tbl2)
  
  
  #### due to more than 1 transaction in agg? need to move inc to above?
  
  
  node_minDate <- select(node_long_df, NodeID, Datetime) %>% 
    group_by(NodeID) %>%
    summarise(min_Datetime = min(Datetime)) %>% 
    ungroup()
  
}







### TEMPLATE-SUSPECT EDGES tSNE =====
if(do_run_tsne == T){
  library("Rtsne")
  .dat <- df_templateSuspect
  .dat <- .dat[.dat$DataSource %in% subset_nms, ]
  .dat <- .dat[.dat$eName == "demographic, financial", ]
  
  tsne_edges <- select(.dat,
                       SecondsAfterStart,
                       eType,
                       DataSource,
                       Weight
                       # SourceLocation,
                       # TargetLocation,
                       # SourceLatitude,
                       # SourceLongitude,
                       # TargetLatitude,
                       # TargetLongitude
  )
  ## tSNE doesn't like NAs:
  tsne_edges[is.na(tsne_edges)] <- -99
  message("Make sure -99 isn't going to mess with data with locations")

  tsne_obj <- Rtsne::Rtsne(tsne_edges, dims = 2,
                          perplexity = 1 / 3 * sqrt(nrow(tsne_edges)),
                          max_iter = 500,
                          check_duplicates = F,
                          pca = T,
                          verbose = TRUE,
                          theta = .5  ## [0, 1] increases speed at expense of accuracy
  )

  f_tsne_obj <- ns_decode_tsne_obj(tsne_edges, tsne_obj)

  gg_tsne <- 
    ggplot(f_tsne_obj) +
    geom_point(
      aes(x = x,
          y = y,
          pch = eName,
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


