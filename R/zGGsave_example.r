library(ggplot2)
ggsave(filename = "test.png", ggplot(), height = 100, width = 400, units)

ggsave(filename = "foo300.png", ggplot(mtcars, aes(x=wt, y=mpg)) +
         geom_point(size=2, shape=23) + theme_bw(base_size = 10),
       width = 8.4, height = 2.1, dpi = 300, units = "cm", device='png')
ggsave(filename = "foo150.png", ggplot(mtcars, aes(x=wt, y=mpg)) +
         geom_point(size=2, shape=23) + theme_bw(base_size = 10),
       width = 8.4, height = 2.1, dpi = 150, units = "cm", device='png')
