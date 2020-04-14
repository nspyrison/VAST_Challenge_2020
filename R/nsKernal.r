library(tictoc); library(beepr); library(tidyverse)

do_run_sample_data  <- T
do_run_sizable_data <- F
filepath <- "./Data/MC1 Data/CGCS-GraphData.csv" ## >123 million obs x 11 var
if (do_run_sizable_data == T) {
  rowsPerSlice <- 21000000 ## Number of obs to include in each slice
  nSlices <- 6 ## Number of data slices to load, 
}


if (do_run_sample_data == T){
    samp <- read.csv2(filepath, #select a random 10000 rows
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
    samp$Time <- lubridate::as_datetime(samp$Time) ## Where x is POSIXt
    message("Seems to be many 16:00 hr, might be able to infer a timezone from this") 
    
    broom::glance(samp) ## na.fraction: .545 = 6/11, good
    skimr::skim(samp)
}

## On to a real slice
dat <- NULL
if (do_run_sizable_data == T){
  warning("Going to read a 5.9 GB csv into ram.")
  ## Full sample is more than 123 million rows of 11 var, break into 6 slides of 21 million obs?
  
  for (i in 1:nSlices) {
    beep()
    tic(paste0("Read data for the: ", i, "-th 21 million rows and append."))
    .head <- if (i == 1) T else F
    .dat <- read.csv2(filepath, 
                      sep = (","),
                      header = .head, 
                      check.names = T, 
                      stringsAsFactors = F,
                      na.strings = "",
                      nrows = rowsPerSlice,
                      skip = (i - 1) * rowsPerSlice
    )
    ## .dat$Time <- lubridate::as_datetime(.dat$Time) ##warning("this seems to cause a hang for 21 million obs slices.")
    cbind(dat, .dat)
    rm(.dat)
    toc() ## took 61sec on ns Dell laptop for dat1
    beep()
  }
  
  summary(dat1)
  message("not all source and target locations are NA, ")
  broom::glance(dat1) ## na.fraction = .241  less than 3/11 = .2727
  skimr::skim(dat1) ## Histograms for time and source look promising.
}



##### VIS

library(ggraph); library(igraph)
if(do_run_sample_data == T){
  ssamp <- samp[samp$Weight > .24,]
  nodes <- data.frame(id = union(unique(ssamp$Source), unique(ssamp$Target)))
  edges <- data.frame(from     = ssamp$Source,
                      to       = ssamp$Target,
                      datetime = ssamp$Time,
                      eType    = ssamp$eType,
                      weight   = as.numeric(ssamp$Weight),
                      fromLoc  = ssamp$SourceLocation,
                      toLoc    = ssamp$TargetLocation,
                      fromLat  = ssamp$SourceLatitude,
                      fromLon  = ssamp$SourceLongitude,
                      toLat    = ssamp$TargetLatitude,
                      toLon    = ssamp$SourceLongitude
  )
  g_ssamp <- graph_from_data_frame(edges, directed = TRUE, vertices = nodes)
  
  # Not specifying the layout - defaults to "auto"
  ggraph(g_ssamp, layout = 'kk', circular = F) + 
    geom_edge_link(alpha  = .33, aes(color = eType, fill = eType)) + 
    geom_node_point(alpha = .3)
  
  
  ##### Perform tSNE on edges
  library(Rtsne)
  tsne_samp <- data.frame(datetime = ssamp$Time,
                          eType    = ssamp$eType,
                          weight   = as.numeric(ssamp$Weight),
                          fromLoc  = ssamp$SourceLocation,
                          toLoc    = ssamp$TargetLocation,
                          fromLat  = ssamp$SourceLatitude,
                          fromLon  = ssamp$SourceLongitude,
                          toLat    = ssamp$TargetLatitude,
                          toLon    = ssamp$SourceLongitude)
  ## tSNE doesn't like NAs:
  # is.na(tsne_samp)
  tsne_samp[is.na(tsne_samp)] <- 0
  
  tsne_out <- Rtsne(tsne_samp, dims = 2, 
                    perplexity = 1 / 3 * sqrt(nrow(tsne_samp)), 
                    max_iter = 500, 
                    check_duplicates = F,
                    pca = T, 
                    verbose = TRUE, 
                    theta = .5  ## [0, 1] increases speed at expense of accuracy
  ) 
  plot(tsne_out$Y, pch = tsne_samp$eType+15, col = alpha(tsne_samp$eType, 0.2))
}
  
##### EXAMPLE
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

