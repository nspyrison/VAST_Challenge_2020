### Nicholas Spyrison for VAST Challenge 2020 MC1
### April 2020

{
  ##### PREAMBLE =====
  library(tictoc); library(beepr); library(tidyverse); library(lubridate)
  
  #do_run_sizable_data  <- F ## See nsKernal.r for full set consumption
  do_run_ggraph    <- TRUE
  do_run_gganimate <- FALSE
  do_run_tsne      <- TRUE
  do_save_output   <- TRUE
  
  subset_nms <- c("Template", paste0("Suspect", 1:5))
  asp_r <- 6/6.5
  width_in   <- 4
  height_in  <- 4 * asp_r
  width_px   <- 820
  height_px  <- 820 * asp_r
  
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
is.na()

### NEWTWORK VIS =====
if(do_run_ggraph == T){
  library("tidygraph"); library("ggraph"); library("igraph"); 
  dat <- select(df_templateSuspect, Source:SourceNodeTypeUsedIn)
  cl_dat <- dat[complete.cases(dat), ]


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
  # for(i in i_s) {
  #   for(j in j_s) {
  #     i<-j<-1
  #     print(c("i is", i, ". j is ", j, "."))
      .dat <- cl_dat
      # .dat <- .dat[.dat$DataSource %in% u_DataSource[i], ]
      # .dat <- .dat[.dat$eName %in% u_eName[j], ]
      # cnt_mat[i, j] <- nrow(.dat)
      
      ## rbind to df_*_netwrok if present
      if (nrow(.dat) > 0){
        ## igraph route: !!!
        .u_nodes <- union(unique(.dat$Source), unique(.dat$Target))
        .network_graph <- igraph::graph_from_data_frame(.dat, 
                                                        directed = TRUE, 
                                                        vertices = .u_nodes)
        .layout <- ns_igraph2layout(graph = .network_graph,
                                    layout = "kk", ## Also try "kk"/"fr", both have circular = F
                                    circular = F, 
                                    decode_tbl = nodeType_tbl,
                                    decode_tbl_id = "NodeID")
        .layout$DataSource <- u_DataSource[i]
        .layout$eName <- u_eName[i]
        
        df_igraph_network <- .layout
        #<- rbind(df_igraph_network, .layout)
        #cnt_mat_igraph[i, j] <- nrow(.layout)
        
        ## ggraph route: !!!
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
        # .ggpraph$DataSource <- u_DataSource[i]
        # .ggpraph$eName <- u_eName[i]
        
        df_ggraph_network <- .ggpraph
        #<- rbind(df_ggraph_network, .ggpraph)
        #cnt_mat_ggraph[i, j] <- nrow(.ggpraph)
      }
      
  #     print(c("i= ", i, ". j= ",  j, ". nrow(.dat): ", nrow(.dat)))
  #   }
  # }
  # cnt_mat
  # cnt_mat_ggraph
  # cnt_mat_igraph
  dim(df_igraph_network)
  dim(df_ggraph_network)
  
  ### _visualize -----
  ## Building from:
  if (F)
    browseURL("http://users.dimi.uniud.it/~massimo.franceschet/ns/syllabus/make/ggraph/ggraph.html")
  .alp <- .35
  
  ##### _igraph route !!!
  ggraph(df_igraph_network) + 
    ## Edges:
    geom_edge_link(#aes(color = eName),
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
    facet_grid(DataSource ~ eName) +
    th_foreground(foreground = "lightgrey", border = TRUE) +
    theme(legend.position = "bottom")
  if (do_save_output == TRUE) {
    fn <- paste0("igraph_")
    ggsave(filename = "network_grid_igrpah%02d.png",
           path = file.path("./output/"),
           plot = last_plot(),
           device = "png",
           units = "in",
           width = width_in,
           height = width_in)
    cat("NS: ggsave'd network_grid_igrpah_XX.png \n")
  }
  
  ### _ggraph route: !!!
  ## add Node attributes
  .lay <- 'igraph'
  .alg <- 'nicely'
  gglay = create_layout(.ggpraph, layout = .lay, algorithm = .alg)
  ggraph(gglay) +
    geom_edge_link(#aes(color = eName),
                   color = "black",
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
    facet_graph(DataSource ~ nName) + # vs facet_grid
    th_foreground(foreground = 'lightgrey', border = TRUE) +
    theme(legend.position = "bottom")
  
  if(do_save_output == TRUE){
    library("lubridate")
    fn <- paste0("ggraph_", .lay, .alg, 
                 "_a", width_px, "x", height_px, 
                 "_H:M", hour(now()),":",minute(now()), ".png")
    ggsave(filename = "network_grid_ggraph%02d.png",
           path = file.path("./output/"),
           plot = last_plot(),
           device = "png",
           units = "in",
           width  = width_px,
           height = height_px)
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
  j_s <- 2:4 
  df_tsne <- NULL
  i <- 1;j <- j_s[1]
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


