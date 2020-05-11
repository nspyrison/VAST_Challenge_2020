### Nicholas Spyrison for VAST Challenge 2020 MC1
### April 2020


##### PREAMBLE =====
library(tictoc); library(beepr); library(tidyverse); library(lubridate)

do_run_sample_data   <- T
#do_run_sizable_data  <- F ## See nsKernal.r for full set consumption
do_run_templateSuspect_data <- T
do_run_ggraph <- T
do_run_gganimate <- T
do_run_tsne <- F

.template_filepath <- "./Submissions/MC1/data/CGCS-Template.csv"
.suspect_filepath_vect <- paste0("./Submissions/MC1/data/Q1-Graph", 1:5, ".csv")


### LOOKUP TABLES =====
if (T) {
  eType_tbl <- 
    data.frame("eType" = 0:6,
               "eName" = c("phone",
                           "email",
                           "sell",
                           "purchase",
                           "co-authorship",
                           "demographic, financial",
                           "travel"),
               "Weight_unit" = c("count",
                                 "count",
                                 "value",
                                 "value",
                                 "authorship fraction",
                                 "value",
                                 "duration [days]")
    )
  
  .demographic_filepath <- "./Submissions/MC1/data/DemographicCategories.csv"
  demographic_tbl <- read.csv2(.demographic_filepath, 
                               sep = (","),
                               header = T, 
                               check.names = T, 
                               stringsAsFactors = F,
                               na.strings = ""
  )
  demographic_tbl <- select(demographic_tbl, 
                            Target = NodeID, 
                            TargetDemographicCategory = Category)
  
  .nodeType_graphdata_filepath   <- "./Submissions/MC1/data/CGCS-GraphData-NodeTypes.csv"
  .nodeType_template_filepath    <- "./Submissions/MC1/data/CGCS-Template-NodeTypes.csv"
  .nodeType_description_filepath <- "./Submissions/MC1/data/NodeTypeDescriptions.csv"
  .nodeType_graphdata <- read.csv2(.nodeType_graphdata_filepath, 
                                   sep = (","),
                                   header = T, 
                                   check.names = T, 
                                   stringsAsFactors = F,
                                   na.strings = ""
  )
  .nodeType_template <- read.csv2(.nodeType_template_filepath, 
                                  sep = (","),
                                  header = T, 
                                  check.names = T, 
                                  stringsAsFactors = F,
                                  na.strings = ""
  )
  .nodeType_union <- dplyr::union(.nodeType_graphdata, .nodeType_template)
  .nodeType_description <- read.csv2(.nodeType_description_filepath, 
                                     sep = (","),
                                     header = T, 
                                     check.names = T, 
                                     stringsAsFactors = F,
                                     na.strings = ""
  )
  nodeType_tbl <- left_join(.nodeType_union, .nodeType_description, by = "NodeType")
  unique(nodeType_tbl$NodeID)
  .nodeType_tbl_Source <- select(nodeType_tbl,
                                 Source = NodeID,
                                 SourceNodeType = NodeType,
                                 SourceDescription = Description,
                                 SourceNodeTypeUsedIn = Used.in
  )
  .nodeType_tbl_Target <- select(nodeType_tbl,
                                 Target = NodeID,
                                 TargetNodeType = NodeType,
                                 TargetDescription = Description,
                                 TargetNodeTypeUsedIn = Used.in
  )
  
  
  ### FORMATING FUNCTIONS =====
  ns_format_df <- function(dat){
    dat$Weight <- as.numeric(dat$Weight) ## Was string of a numeric.
    dat$Datetime = lubridate::as_datetime(dat$Time) + lubridate::years(55)
    dat <- left_join(dat, eType_tbl, by = "eType")
    dat <- left_join(dat, demographic_tbl, by = "Target")
    dat <- left_join(dat, .nodeType_tbl_Source, by = "Source")
    dat <- left_join(dat, .nodeType_tbl_Target, by = "Target")
    dat[dat == -99] <- NA
    
    ## reordered, esp for: 'to/Source' and 'from/Target' first.
    dat <- select(dat, 
                  Source, 
                  Target,
                  DataSource,
                  Datetime,
                  ## POSIXt ("Unix timestamps"), but shifted 55 years forward from 1970 to 2025 indexing
                  Second = Time, ## Relative to 2025 Jan 1.
                  eType, 
                  eName,
                  Weight,
                  Weight_unit,
                  SourceNodeType,
                  SourceDescription,
                  SourceNodeTypeUsedIn,
                  SourceLocation,
                  SourceLatitude,
                  SourceLongitude,
                  TargetNodeType,
                  TargetDescription,
                  TargetNodeTypeUsedIn,
                  TargetDemographicCategory,
                  TargetLocation,
                  TargetLatitude,
                  TargetLongitude)
    
    ## return tibble (1 row is 1 edges of the network)
    as_tibble(dat)
  }
  
  ns_df2network <- function(dat){
    requireNamespace("igraph")
    .nodes <- union(unique(dat$Source), unique(dat$Target))
    ## return network (igraph object)
    igraph::graph_from_data_frame(dat, directed = TRUE, vertices = .nodes)
  } 
  
  ns_network2layout <- function(graph, 
                                layout = 'fr', ## also try 'kk', (circular = F)
                                circular = F, 
                                decode_tbl = NULL,
                                decode_tbl_id = "NodeID"){
    if (!is.null(decode_tbl) & !("data.frame" %in% class(anim_df)) )
      stop("decode_tbl should be a data.frame.")
    
    .lay   <- create_layout(.g_dat, layout = layout, circular = circular)
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
}


### LOAD TEMPLATE AND SUSPECTS =====
if (do_run_templateSuspect_data == T) {
  dat_templateSuspect <- read.csv2(.template_filepath, 
                                   sep = (","),
                                   header = T, 
                                   check.names = T, 
                                   stringsAsFactors = F,
                                   na.strings = "",
  )
  dat_templateSuspect$DataSource <- "Template"
  
  for (i in 1:length(.suspect_filepath_vect)){
    .dat <- read.csv2(.suspect_filepath_vect[i], 
                      sep = (","),
                      header = T, 
                      check.names = T, 
                      stringsAsFactors = F,
                      na.strings = "",
    )
    .dat$DataSource <- paste0("Suspect", i)
    dat_templateSuspect <- rbind(dat_templateSuspect, .dat)
  }
  
  dat_templateSuspect <- ns_format_df(dat_templateSuspect)
  table(dat_templateSuspect$DataSource)
  
  dat_templateSuspect
  skimr::skim(dat_templateSuspect)
}


### TEMPLATE-SUSPECT NEWTWORK VIS =====
if(do_run_ggraph == T){
  library(ggraph); library(igraph)
  .dat <- dat_templateSuspect[dat_templateSuspect$SourceDescription == "Person", ]
  .dat <- .dat[.dat$DataSource %in% c("Template", "Suspect1"), ]
  
  # message("Scope of data:")
  # summary(.dat[c("eType", "Datetime", "DataSource")])
  # table(.dat[c("eName","DataSource")])
  
  
  .g_dat <- ns_df2network(dat = .dat)
  ## want to find a vector of length 5697 giving eName/eType [1:6]
  .lay   <- ns_network2layout(graph = .g_dat, 
                              layout = "fr", 
                              circular = F, 
                              decode_tbl = nodeType_tbl,
                              decode_tbl_id = "NodeID")
  
  ## alt: try; layout = 'kk', circular = F
  ##str(.lay) ## Good, kept attributes.
  ## For extending see:
  if (F)
    browseURL("http://users.dimi.uniud.it/~massimo.franceschet/ns/syllabus/make/ggraph/ggraph.html")
  
  ## ggraph sample
  .alp <- .35
  ggraph(.lay) + 
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
  ggsave(filename = "layout_fr%02d.png",
         path = file.path("./output/"),
         plot = last_plot(),
         device = "png",
         units = "in",
         width = 8,
         height = 2)
  
  ### === Looking up ggraph functions let to this approach:

  library(tidygraph)
  gr <- as_tbl_graph(.dat)
  #attributes(gr) ## notice at active == "nodes"
  
  ## add Node attributes
  gr <- mutate(gr, name = as.integer(name))
  gr <- left_join(gr, nodeType_tbl, c("name" = "NodeID"))
  gr <- mutate(gr,
               nType   = NodeType,
               nName   = Description,
               nUsedIn = Used.in,
               nPopularity = as.character(cut(centrality_degree(mode = 'in'),
                                              breaks = 5,
                                              labels = paste0(c("1st",
                                                                "2nd",
                                                                "3rd",
                                                                "4th",
                                                                "5th"), " Quantile")))
  )
  
  ggraph(gr) +
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
  
  ggsave(filename = "gr%02d.png",
         path = file.path("./output/"),
         plot = last_plot(),
         device = "png",
         units = "in",
         width = 8,
         height = 2)

}

### TEMPLATE-SUSPECT GGANIMATE =====
if(do_run_gganimate == T){
  .dat <- dat_templateSuspect
  
  node_long_df <- .dat[, 1:9]
  node_long_df <- .dat
  node_long_df <- 
    pivot_longer(node_long_df,
                 cols = Source:Target,
                 names_to = "Direction",
                 values_to = "NodeID"
    )
  node_long_df <- mutate(node_long_df, Weight_sign = ifelse(Direction == "Source", -1, 1))
  node_long_df <- left_join(node_long_df, nodeType_tbl, by = "NodeID")
  node_long_df <- select(node_long_df,
                         DataSource,
                         Datetime,
                         eName,
                         NodeID,
                         NodeDescription = Description,
                         NodeUsedIn = Used.in,
                         Direction,
                         Weight,
                         Weight_sign,
                         Weight_unit
  )
  
  frame_str <- data.frame(frame = 1:36, 
                          periodName = c(2001:2024, paste0(2025, "-", str_pad(1:12, 2,pad = "0"))),
                          periodEndDate = c(
                            paste0(2002:2025, "-01-01"),
                            paste0(2025, "-", str_pad(2:12, 2,pad = "0"), "01"),
                            "2026-01-01")
  )
  frame_str$periodEndDate <- as_date(ymd(frame_str$periodEndDate) - seconds(1))
  
  .n <- nrow(frame_str)
  anim_df <- NULL
  for (i in 1:.n) {
    sub <- node_long_df
    sub <- sub[sub$Datetime <= frame_str$periodEndDate[i], ]
    
    this_frame <- 
      group_by(sub, DataSource, NodeID, NodeDescription, NodeUsedIn, eName, Weight_unit, Direction) %>%
      summarise(frame = i,
                compKey = paste0(i, DataSource, NodeID, Direction),
                last_sum_Weight      = NA,
                last_signedWeight    = NA, 
                last_n_transactions  = NA,
                inc_sum_Weight       = NA, 
                inc_sum_signedWeight = NA,
                inc_n_transactions   = NA,
                cumsum_Weight        = sum(Weight),
                cumsum_signedWeight  = sum(Weight * Weight_sign),
                n_transactions       = n()
      ) %>% ungroup()

    
    ## CONTINUE WORK HERE, NEED COMP KEY ON LAST FRAME.
    if (i > 1) {
      last_frame <- anim_df[anim_df$frame == i - 1, ]
      last_frame$frame <- last_frame$frame + 1
      last_frame <- mutate(last_frame,
                           compKey = paste0(frame + 1, DataSource, NodeID, Direction),
      )
      lf <- select(last_frame, 
                   compKey, 
                   cumsum_Weight, 
                   cumsum_signedWeight, 
                   n_transactions)
      
      this_frame <- mutate(this_frame,
                           last_sum_Weight      = lf$cumsum_Weight,
                           last_signedWeight    = lf$cumsum_signedWeight,
                           last_n_transactions  = lf$n_transactions,
                           inc_sum_Weight       = cumsum_Weight       - lf$cumsum_Weight,
                           inc_sum_signedWeight = cumsum_signedWeight - lf$cumsum_signedWeight,
                           inc_n_transactions   = n_transactions      - lf$n_transactions
      )
    }
    
    anim_df <- rbind(anim_df, this_frame)
  }
  anim_df <- as_tibble(anim_df)
  
  # ggplot() +
  #   ggplot2::geom_point(data = anim_df,
  #                       size = 2, alpha = .3,
  #                       mapping = ggplot2::aes(x = x, y = y, 
  #                                              shape = eName, 
  #                                              color = eName, fill = eName, 
  #                                              frame = frame)
  # )
}
  
  


### TEMPLATE-SUSPECT EDGES tSNE =====
if(do_run_tsne == T){
  library("Rtsne")
  .dat <- dat_templateSuspect
  .dat <- .dat[.dat$DataSource %in% c("Template", "Suspect1"), ]
  .dat <- .dat[.dat$eName == "demographic, financial", ]
  
  tsne_edges <- select(.dat,
                       Second,
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
}


