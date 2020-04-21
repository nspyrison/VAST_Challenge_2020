### Nicholas Spyrison for VAST Challenge 2020 MC1
### April 2020


##### PREAMBLE =====
library(tictoc); library(beepr); library(tidyverse); library(lubridate)

do_run_sample_data   <- T
do_run_sizable_data  <- F
do_run_templateSuspect_data <- T
do_run_ggraph_examples <- F

.template_filepath <- "./Submissions/MC1/data/CGCS-Template.csv"
.suspect_filepath_vect <- paste0("./Submissions/MC1/data/Q1-Graph", 1:5, ".csv")
if (do_run_sizable_data == T) {
  rowsPerSlice <- 21000000 ## Number of obs to include in each slice
  nSlices <- 6 ## Number of data slices to load, 
}


### LOOKUP TABLES =====
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

ns_format_tsne_obj <- function(tsne_input, tsne_output){
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
if(do_run_templateSuspect_data == T){
  library(ggraph); library(igraph)
  .dat <- dat_templateSuspect[dat_templateSuspect$SourceDescription == "Person", ]
  .dat <- .dat[.dat$DataSource %in% c("Template", "Suspect1"), ]
  
  # message("Scope of data:")
  # summary(.dat[c("eType", "Datetime", "DataSource")])
  # table(.dat[c("eName","DataSource")])
  
  
  .g_dat <- ns_df2network(.dat)
  .lay   <- create_layout(.g_dat, layout = "fr")
  ## alt: try; layout = 'kk', circular = F
  .lay$name <- as.integer(.lay$name)
  .lay_lj   <- left_join(.lay, nodeType_tbl, by = c("name" = "NodeID"))
  .lay$nType   <- .lay_lj$NodeType
  .lay$nName   <- .lay_lj$Description
  .lay$nUsedIn <- .lay_lj$Used.in
  #str(.lay) ## Good, kept attributes.
  
  ## For extending see:
  if (F)
    browseURL("http://users.dimi.uniud.it/~massimo.franceschet/ns/syllabus/make/ggraph/ggraph.html")
  
  ## ggraph sample
  .alp <- .35
  tic("ggraph on .lay")
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
                        fill  = nName)
    ) +
    facet_grid(DataSource~eName) +
    th_foreground(foreground = 'grey', border = TRUE) + 
    theme_graph()
  toc() ## ~ 
}

### Template-Suspect gganimate
if(do_run_templateSuspect_data == T){
  .dat <- dat_templateSuspect
  
  node_long_tbl <- .dat[, 1:9]
  node_long_tbl <- 
    pivot_longer(node_long_tbl,
                 cols = Source:Target,
                 names_to = "Direction",
                 values_to = "NodeID"
    )
  node_long_tbl[node_long_tbl == -99] <- NA
  node_long_tbl <- mutate(node_agg_tbl, Weight_sign = ifelse(Direction == "Source", -1, 1))
  node_long_tbl <- left_join(node_long_tbl, nodeType_tbl, by = "NodeID")
  node_long_tbl <- select(node_long_tbl,
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
    .sub <- node_long_tbl
    .sub <- .sub[.sub$Datetime <= frame_str$periodEndDate[i], ]
    
    .row <- 
      group_by(.sub, DataSource, NodeID, NodeDescription, NodeUsedIn, eName, Weight_unit, Direction) %>%
      summarise(cumsum_Weight = sum(Weight),
                cumsum_Signed_Weight = sum(Weight * Weight_sign),
                n_transactions = n()
      )
    .row$frame <- i
    
    anim_df <- rbind(anim_df,
                     inner_join(frame_str, .row, by = "frame")
    )
  }
  anim_df <- as_tibble(anim_df)
  
  ggplot2::geom_point( 
    data = data_slides,
    shape = pch, color = col, fill = col, size = cex, alpha = alpha,
    mapping = ggplot2::aes(x = x, y = y, frame = slide)
  )
}
  
  


### TEMPLATE-SUSPECT EDGES tSNE =====
if(do_run_templateSuspect_data == T){
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

  f_tsne_obj <- ns_format_tsne_obj(tsne_edges, tsne_obj)

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

# ### GGRAPH EXAMPLES =====
# ## excerpt from ./R/zGgraph.r
# if (do_run_ggraph_examples == T) {
#   graph <- graph_from_data_frame(highschool)
#   
#   # Not specifying the layout - defaults to "auto"
#   ggraph(graph) + 
#     geom_edge_link(aes(colour = factor(year))) + 
#     geom_node_point()
#   
#   # add a layout 'kk'
#   ggraph(graph, layout = 'kk') +
#     geom_edge_link(aes(colour = factor(year))) +
#     geom_node_point()
#   
#   # add parameter maxiter, (specific to layout?)
#   ggraph(graph, layout = 'kk', maxiter = 100) +
#     geom_edge_link(aes(colour = factor(year))) +
#     geom_node_point()
#   
#   # layout drl, can return layout as an obj
#   layout <- create_layout(graph, layout = 'drl')
#   # A coord diagram
#   ggraph(graph, layout = 'linear', circular = TRUE) + 
#     geom_edge_arc(aes(colour = factor(year)))
#   
#   graph <- graph_from_data_frame(flare$edges, vertices = flare$vertices)
#   
#   # An icicle plot
#   ggraph(graph, 'partition') + 
#     geom_node_tile(aes(fill = depth), size = 0.25)
#   
#   # A sunburst plot
#   ggraph(graph, 'partition', circular = TRUE) + 
#     geom_node_arc_bar(aes(fill = depth), size = 0.25)
# }
