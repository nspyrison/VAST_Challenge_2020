library(tictoc); library(beepr); library(tidyverse)

do_run_sample_data   <- T
do_run_sizable_data  <- F
# do_run_template_data <- T
# do_run_suspect_data  <- T
data_filepath <- "./Data/MC1 Data/CGCS-GraphData.csv" ## full data, 123.9 million obs x 11 var
if (do_run_sizable_data == T) {
  rowsPerSlice <- 21000000 ## Number of obs to include in each slice
  nSlices <- 6 ## Number of data slices to load, 
}

### LOAD SAMPLE DATA =====
if (do_run_sample_data == T){
    samp <- read.csv2(data_filepath, ## Read first 10000 rows
                      sep = (","),
                      header = T, 
                      check.names = T, 
                      stringsAsFactors = F,
                      na.strings = "",
                      nrows = 10000)
    as_tibble(samp)
    message("Note 10000 NA's in last 4 columns.")
    message("Time is negative int ( on order -1E9), doesn't look like a concat of granularities.
        Google-foo suggests Unix time (POSIX time), or 'the number of 
        seconds that have passed since 00:00:00 UTC Thursday, 1 January 1970' ")
    
    message("Seems to be many 16:00 hr, might be able to infer a timezone from this") 
    
    broom::glance(samp) ## na.fraction: .545 = 6/11, good
    skimr::skim(samp)
}

### LOAD FULL DATA =====
## On to a real slice
dat <- NULL
if (do_run_sizable_data == T){
  warning("Going to read a 5.9 GB csv into ram.")
  ## Full sample is more than 123 million rows of 11 var, break into 6 slides of 21 million obs?
  
  beep(0)
  tic("Reading full dataset")
  for (i in 1:nSlices) {
    tic(paste0("Read data for data slice ", i, "(read and append n-th 21 million rows)"))
    .head <- if (i == 1) T else F
    .dat <- read.csv2(data_filepath, 
                      sep = (","),
                      header = .head, 
                      check.names = T, 
                      stringsAsFactors = F,
                      na.strings = "",
                      nrows = rowsPerSlice,
                      skip = (i - 1) * rowsPerSlice
    )
    ## .dat$Time <- lubridate::as_datetime(.dat$Time) 
    ## warning("this seems to cause a hang for 21 million obs slices.")
    rbind(dat, .dat)
    rm(.dat)
    toc() ## tooks ~61sec on ns Dell laptop for first 21 million row slice.
    beep(i)
  }
  beep(4)
  toc()
  
  summary(dat1)
  message("not all source and target locations are NA, ")
  broom::glance(dat1) ## na.fraction = .241  less than 3/11 = .2727
  skimr::skim(dat1) ## Histograms for time and source look promising.
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

demographic_filepath <- "./Submissions/MC1/data/DemographicCategories.csv"
demographic_tbl <- read.csv2(demographic_filepath, 
                     sep = (","),
                     header = T, 
                     check.names = T, 
                     stringsAsFactors = F,
                     na.strings = ""
)
demographic_tbl <- select(demographic_tbl, 
                          Target = NodeID, 
                          TargetDemographicCategory = Category)

nodeType_graphdata_filepath   <- "./Submissions/MC1/data/CGCS-GraphData-NodeTypes.csv"
#nodeType_template_filepath    <- "./Submissions/MC1/data/CGCS-Template-NodeTypes.csv"
nodeType_description_filepath <- "./Submissions/MC1/data/NodeTypeDescriptions.csv"
nodeType_graphdata <- read.csv2(nodeType_graphdata_filepath, ## A superset of the template.
                                sep = (","),
                                header = T, 
                                check.names = T, 
                                stringsAsFactors = F,
                                na.strings = ""
)
nodeType_description <- read.csv2(nodeType_description_filepath, 
                                  sep = (","),
                                  header = T, 
                                  check.names = T, 
                                  stringsAsFactors = F,
                                  na.strings = ""
)
nodeType_tbl <- left_join(nodeType_graphdata, nodeType_description, by = "NodeType")
nodeType_tbl_Source <- select(nodeType_tbl,
                              Source = NodeID,
                              SourceNodeType = NodeType,
                              SourceDescription = Description,
                              SourceNodeTypeUsedIn = Used.in
)
nodeType_tbl_Target <- select(nodeType_tbl,
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
  dat <- left_join(dat, nodeType_tbl_Source, by = "Source")
  dat <- left_join(dat, nodeType_tbl_Target, by = "Target")
  
  ## reordered, esp for: 'to' and 'from' first.
  dat <- select(dat, 
                Source, 
                Target,
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

(samp <- ns_format_df(samp))
(g_samp <- ns_df2network(samp))

ns_format_tsne_obj <- function(tsne_input, tsne_output){
  decoded_input <- left_join(tsne_input, eType_tbl, by = "eType")
  tibble(x = tsne_output$Y[, 1],
         y = tsne_output$Y[, 2],
         eType       = decoded_input$eType,
         eName       = decoded_input$eName,
         Weight      = decoded_input$Weight,
         Weight_unit = decoded_input$Weight_unit
  )
}

##### VIS

library(ggraph); library(igraph)
if(do_run_sample_data == T){
  
  summary(samp[c("eType", "Datetime", "Weight")])
  
  .ssamp <- samp[samp$Weight > .24,]
  .g_ssamp <- ns_df2network(.ssamp)
  
  ## ggraph sample
  tic("ggraph, layout = 'kk', circular = F")
  ggraph(.g_ssamp, layout = 'kk', circular = F) + 
    geom_edge_link(alpha  = .33, aes(color = eType, fill = eType)) + 
    geom_node_point(alpha = .3)
  toc()
  
  ## tsne sample:
  requireNamespace("Rtsne")
  tsne_edges <- select(.ssamp,
                       Second,
                       eType,
                       Weight,
                       SourceLocation,
                       TargetLocation,
                       SourceLatitude,
                       SourceLongitude,
                       TargetLatitude,
                       TargetLongitude
  )
  ## tSNE doesn't like NAs:
  tsne_edges[is.na(tsne_edges)] <- -99 
  message("Make sure -99 isn't going to mess with data with locations")
  
  ##### Perform tSNE on edges
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
          col = eName,
          fill = eName
      ),
      alpha = .3
    ) + theme_minimal()
}
  
### GGRAPH EXAMPLES =====
## FROM ./R/zGgraph.r
if (F) {
  graph <- graph_from_data_frame(highschool)
  
  # Not specifying the layout - defaults to "auto"
  ggraph(graph) + 
    geom_edge_link(aes(colour = factor(year))) + 
    geom_node_point()
  
  # add a layout 'kk'
  ggraph(graph, layout = 'kk') +
    geom_edge_link(aes(colour = factor(year))) +
    geom_node_point()
  
  # add parameter maxiter, (specific to layout?)
  ggraph(graph, layout = 'kk', maxiter = 100) +
    geom_edge_link(aes(colour = factor(year))) +
    geom_node_point()
  
  # layout drl, can return layout as an obj
  layout <- create_layout(graph, layout = 'drl')
  # A coord diagram
  ggraph(graph, layout = 'linear', circular = TRUE) + 
    geom_edge_arc(aes(colour = factor(year)))
  
  graph <- graph_from_data_frame(flare$edges, vertices = flare$vertices)
  
  # An icicle plot
  ggraph(graph, 'partition') + 
    geom_node_tile(aes(fill = depth), size = 0.25)
  
  # A sunburst plot
  ggraph(graph, 'partition', circular = TRUE) + 
    geom_node_arc_bar(aes(fill = depth), size = 0.25)
}
