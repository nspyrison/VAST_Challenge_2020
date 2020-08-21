### Nicholas Spyrison for VAST Challenge 2020 MC1
### April 2020

{
  ##### PREAMBLE =====
  library(tictoc); library(beepr); library(tidyverse); library(lubridate)
  
  #do_run_sizable_data  <- F ## See nsKernal.r for full set consumption
  do_run_ggraph    <- TRUE
  do_run_tsne      <- TRUE
  do_save_output   <- TRUE
  
  subset_nms <- c("Template", paste0("Suspect", 1:5))
  
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
  ns_igraph2layout <- function(graph, 
                               layout = 'fr', ## also try 'kk', (circular = F)
                               circular = F, 
                               decode_tbl = NULL,
                               decode_tbl_id = "NodeID"){
    if (!is.null(decode_tbl) & !("igraph" %in% class(graph)) ) ## df or "igraph"
      stop("decode_tbl should be a data.frame.")
    require("igraph")
    
    .lay   <- create_layout(graph, layout = layout, circular = circular)
    try(.lay$name <- as.integer(.lay$name))
    
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
  
}


### NEWTWORK VIS =====
if(do_run_ggraph == T){
  library("tidygraph"); library("ggraph"); library("igraph");
  ## Rrmove location NULL columns
  dat <- select(df_templateSuspect, Source:SourceNodeTypeUsedIn)
  ## Only complete rows
  cl_dat <- dat[complete.cases(dat), ]
  cl_dat$Weight <- abs(cl_dat$Weight)


  ## _wrangle -----
  u_DataSource <- unique(cl_dat$DataSource)
  u_eName <- unique(cl_dat$eName)
  l_u_DataSource <- length(u_DataSource)
  l_u_eName <- length(u_eName)
  i_s <- 1:l_u_DataSource
  j_s <- 1:l_u_eName
  df_igraph_network <- NULL ## Output df of igraph objects
  df_ggraph_network <- NULL ## Output df of ggraph objects
  cnt_mat <- matrix(0, nrow = l_u_DataSource, ncol = l_u_eName)
  rownames(cnt_mat) <- u_DataSource
  colnames(cnt_mat) <- u_eName
  cnt_mat_igraph <- cnt_mat_ggraph <- cnt_mat
  
  .dat <- cl_dat
  ## rbind to df_*_netwrok if present
  if (nrow(.dat) > 0){
    .ggpraph <- as_tbl_graph(.dat)
    .ggpraph <- mutate(.ggpraph, name = as.integer(name))
    .ggpraph <- left_join(.ggpraph, nodeType_tbl, c("name" = "NodeID"))
    .ggpraph <- mutate(.ggpraph,
                       nType = NodeType,
                       nName = Description,
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
    df_ggraph_network <- .ggpraph
  }
  
  ### _visualize -----
  ## Building from:
  if (F)
    browseURL("http://users.dimi.uniud.it/~massimo.franceschet/ns/syllabus/make/ggraph/ggraph.html")
  .alp <- .34
  .lay <- "igraph"
  .alg <- "lgl"
  gglayout = create_layout(df_ggraph_network, layout = .lay, algorithm = .alg)
  ggraph(gglayout) +
    geom_edge_link(color = "grey40",
                   edge_width = .5,
                   alpha = .alp,
                   arrow = arrow(length = unit(1.5, 'mm')),
                   end_cap = circle(1, 'mm')) +
    geom_node_point(alpha = .alp,
                    size = 2,
                    aes(shape = nName,
                        color = nName,
                        fill  = nName)) +
    facet_grid(nName ~ DataSource) +
    th_foreground(foreground = "lightgrey", border = TRUE) +
    theme(legend.position = "bottom",
          legend.margin = margin(0, 0 , 0, 0),
          legend.box.margin = margin(t = -10, r = -10, b = -5, l = -10))
  
  if(do_save_output == TRUE){
    width_cm  <- 12.6
    height_cm <- 12.6
    dpi <- 450
    fn <- paste0("ggraph_", .lay, .alg, 
                 "_", width_cm, "x", height_cm, "xx", dpi, ".png")
    ggsave(filename = fn,
           path = file.path("./output/"),
           plot = last_plot(),
           device = "png",
           units = "cm",
           dpi = dpi, 
           width  = width_cm,
           height = height_cm)
    cat(paste0("NS: ggsave'd: ", fn))
  }
}


### tSNE on EDGES  =====
if(do_run_tsne == T){
  library("Rtsne")
  ## Rrmove location NULL columns
  .dat <- select(df_templateSuspect, Source:Weight)
  ## Only complete rows
  cl_dat <- .dat[complete.cases(.dat), ]
  cl_dat$Weight <- abs(cl_dat$Weight)
  
  node_long_df <- pivot_longer(cl_dat,
                               cols = Source:Target,
                               names_to = "Direction",
                               values_to = "NodeID")
  .dat <- left_join(node_long_df, nodeType_tbl, by = "NodeID")

  ## STOPPED HERE
  .dat <- select(.dat,
                 DataSource,
                 eType,
                 SecondsAfterStart,
                 NodeID,
                 Direction,
                 eName,
                 Weight
  )
  .dat <- mutate(.dat,
                 DataSource = as.factor(DataSource),
                 eType = eType,
                 SecondsAfterStart,
                 NodeID = as.factor(NodeID),
                 Direction = as.factor(Direction),
                 Weight
  )
  
  .DataSource_s <- unique(.dat$DataSource)
  i_s <- 1:length(.DataSource_s)
  df_tsne <- NULL
  for(i in i_s) {
    tgt_ds <- .DataSource_s
    sub <- .dat[.dat$DataSource == tgt_ds, ]
    
    tsne_edges <- sub
    tsne_obj <- Rtsne::Rtsne(tsne_edges, 
                             dims = 2,
                             perplexity = 1 / 3 * sqrt(nrow(tsne_edges)),
                             max_iter = 500,
                             check_duplicates = F,
                             pca = T,
                             verbose = TRUE,
                             theta = .5  ## [0, 1] increases speed at expense of accuracy
    )
    decoded_input <- left_join(tsne_edges, eType_tbl, by = "eType")
    decoded_output <- 
      tibble(x = tsne_obj$Y[, 1],
             y = tsne_obj$Y[, 2],
             eType       = decoded_input$eType,
             eName       = decoded_input$eName.x,
             DataSource  = decoded_input$DataSource,
             Weight      = decoded_input$Weight,
             Weight_unit = decoded_input$Weight_unit
      )
    df_tsne <- rbind(df_tsne, decoded_output)
  }
  
  ggplot(df_tsne) +
    geom_point(aes(x = x, y = y,
                   pch = DataSource, col = DataSource, fill = DataSource),
               alpha = .2, size = .5) + 
    ggplot2::facet_grid(cols = vars(DataSource)) +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.margin = margin(0, 0 , 0, 0),
          legend.box.margin = margin(t = -10, r = -10, b = -5, l = -10))
  
  width_cm <- 8.4
  height_cm <- width_cm / 6
  dpi <- 300
  if(do_save_output == TRUE){
    fn <- paste0("tSNE_", width_cm, "x", height_cm, "xx", dpi, ".png")
    ggsave(filename = fn,
           path = file.path("./output/"),
           plot = last_plot(),
           device = "png",
           dpi = dpi,
           units = "cm",
           width = width_cm,
           height = height_cm)
    cat(paste0("NS: ggsave'd: ", fn))
  }
}


