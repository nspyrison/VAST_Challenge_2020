### NS 02/04/2020
# ggprah by Thomas Lin Pedersen, dev through (atleast) 2020
# https://github.com/thomasp85/ggraph

### ggraph layout vignette -----
message("Working from the post:")
browseURL("https://www.data-imaginist.com/2017/ggraph-introduction-layouts/")

library(ggraph)
library(igraph)
graph <- graph_from_data_frame(highschool)

# Not specifying the layout - defaults to "auto"
ggraph(graph) + 
  geom_edge_link(aes(colour = factor(year))) + 
  geom_node_point()

# # add a layout 'kk'
# ggraph(graph, layout = 'kk') + 
#   geom_edge_link(aes(colour = factor(year))) + 
#   geom_node_point()
# 
# # add parameter maxiter, (specific to layout?)
# ggraph(graph, layout = 'kk', maxiter = 100) + 
#   geom_edge_link(aes(colour = factor(year))) + 
#   geom_node_point()

# layout drl, can return layout as an obj
layout <- create_layout(graph, layout = 'drl')
ggraph(layout) + 
  geom_edge_link(aes(colour = factor(year))) + 
  geom_node_point()

head(layout)
attributes(layout)


# An arc diagram
ggraph(graph, layout = 'linear') + 
  geom_edge_arc(aes(colour = factor(year)))

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

# Hive plot
V(graph)$friends <- degree(graph, mode = 'in')
V(graph)$friends <- ifelse(V(graph)$friends < 5, 'few', 
                           ifelse(V(graph)$friends >= 15, 'many', 'medium'))
ggraph(graph, 'hive', axis = 'friends', sort.by = 'degree') + 
  geom_edge_hive(aes(colour = factor(year), alpha = ..index..)) + 
  geom_axis_hive(aes(colour = friends), size = 3, label = FALSE) + 
  coord_fixed()

### Hierarchical layouts
# Area hier
graph <- graph_from_data_frame(flare$edges, vertices = flare$vertices)
set.seed(1)
ggraph(graph, 'circlepack', weight = 'size') + 
  geom_node_circle(aes(fill = depth), size = 0.25, n = 50) + 
  coord_fixed()

# Network hier
set.seed(1)
ggraph(graph, 'circlepack', weight = 'size') + 
  geom_edge_link() + 
  geom_node_point(aes(colour = depth)) +
  coord_fixed()

# Treemap mosaic
ggraph(graph, 'treemap', weight = 'size') + 
  geom_node_tile(aes(fill = depth), size = 0.25)

# Treemap link-node
ggraph(graph, 'treemap', weight = 'size') + 
  geom_edge_link() + 
  geom_node_point(aes(colour = depth))

# Dendrogram "diagonal" (more loess than diag imo)
ggraph(graph, 'dendrogram') + 
  geom_edge_diagonal()

# Dendrogram "elbow"
dendrogram <- as.dendrogram(hclust(dist(iris[, 1:4])))
ggraph(dendrogram, 'dendrogram') + 
  geom_edge_elbow()

# Dendrogram, radial elbow
ggraph(dendrogram, 'dendrogram', circular = TRUE) + 
geom_edge_elbow() + 
  coord_fixed()

# Radial loess hier. 
warning("takes a lot of cycles to make.")
if (F){
  gr <- graph_from_data_frame(flare$edges, vertices = flare$vertices)
  l <- ggraph(gr, layout = 'partition', circular = TRUE)
  l + geom_edge_diagonal(aes(width = ..index.., alpha = ..index..), lineend = 'round') + 
    scale_edge_width(range = c(0.2, 1.5)) + 
    geom_node_point(aes(colour = depth)) + 
    coord_fixed()
}

## GIF like animation
##TODO: NOT WORKING, but looks great online, make as aanimate?
if (F) {
  #install.packages("tweenr");install.packages("ggraph");install.packages("igraph")
  library(tweenr);library(ggraph);library(igraph)
  igraph_layouts <- c('star', 'circle', 'gem', 'dh', 'graphopt', 'grid', 'mds', 
                      'randomly', 'fr', 'kk', 'drl', 'lgl')
  graph <- graph_from_data_frame(highschool)
  V(graph)$degree <- degree(graph)
  layouts <- lapply(igraph_layouts, create_layout, graph = graph)
  layouts_tween <- tween_states(c(layouts, layouts[1]), tweenlength = 1, 
                                statelength = 1, ease = 'cubic-in-out', 
                                nframes = length(igraph_layouts) * 16 + 8)
  title_transp <- tween_t(c(0, 1, 0, 0, 0), 16, 'cubic-in-out')[[1]]
  for (i in seq_len(length(igraph_layouts) * 16)) {
    tmp_layout <- layouts_tween[layouts_tween$.frame == i, ]
    layout <- igraph_layouts[ceiling(i / 16)]
    title_alpha <- title_transp[i %% 16]
    p <- ggraph(graph, 'manual', node.position = tmp_layout) + 
      geom_edge_fan(aes(alpha = ..index.., colour = factor(year)), n = 15) +
      geom_node_point(aes(size = degree)) + 
      scale_edge_color_brewer(palette = 'Dark2') + 
      ggtitle(paste0('Layout: ', layout)) + 
      theme_void() + 
      theme(legend.position = 'none', 
            plot.title = element_text(colour = alpha('black', title_alpha)))
    plot(p)
  }
}


### ggraph edges vignette -----
message("Working from the post:")
browseURL("https://www.data-imaginist.com/2017/ggraph-introduction-edges/")
message("I won't include most of the repeat content.")

library(ggraph)
library(igraph)

# let's make some of the student love themselves
hairball <- graph_from_data_frame(highschool)
loopy_hairball <- add_edges(hairball, rep(1:5, each=2), year = rep('1957', 5))
ggraph(loopy_hairball, layout = 'kk') + 
  geom_edge_link(aes(colour = year), alpha = 0.25) + 
  geom_edge_loop(aes(colour = year))

# Arrows
simple <- make_graph('bull')
# Random names - I swear
V(simple)$name <- c('Thomas', 'Bob', 'Hadley', 'Winston', 'Baptiste')
E(simple)$type <- sample(c('friend', 'foe'), 5, TRUE)

ggraph(simple, layout = 'graphopt') + 
  geom_edge_link(arrow = arrow(length = unit(4, 'mm')), 
                 end_cap = circle(3, 'mm')) + 
  geom_node_point(size = 5)

# Text nodes rather than point nodes
ggraph(simple, layout = 'graphopt') + 
  geom_edge_link(aes(start_cap = label_rect(node1.name),
                     end_cap = label_rect(node2.name)), 
                 arrow = arrow(length = unit(4, 'mm'))) + 
  geom_node_text(aes(label = name))

# Radial layout
ggraph(simple, layout = 'linear', circular = TRUE) + 
  geom_edge_arc(arrow = arrow(length = unit(4, 'mm')), 
                start_cap = circle(3, 'mm'),
                end_cap = circle(3, 'mm')) + 
  geom_node_point(size = 5) + 
  coord_fixed()

# Edge labels
ggraph(simple, layout = 'graphopt') + 
  geom_edge_link(aes(label = type), 
                 arrow = arrow(length = unit(4, 'mm')), 
                 end_cap = circle(3, 'mm')) + 
  geom_node_point(size = 5)

# "Flare graph" coord diagram with alpha
flaregraph <- graph_from_data_frame(flare$edges, vertices = flare$vertices)
from <- match(flare$imports$from, flare$vertices$name)
to <- match(flare$imports$to, flare$vertices$name)
ggraph(flaregraph, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from, to = to), alpha = 0.1) + 
  coord_fixed()

