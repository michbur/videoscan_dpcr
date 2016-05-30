library(dplyr)
library(binom)
source("plot_tools.R")

raw_dat <- read.csv2("Chamber02_000020.csv", check.names = FALSE)
colnames(raw_dat) <- c("id", "x", "y", "size", "ch_loc", "ch_1")

dat <- select(raw_dat, -7) %>% 
  mutate(ratio = ch_1/ch_loc,
         pos = ratio > 1,
         status = factor(pos, labels = c("Negative", "Positive")))

ggplot(dat, aes(x = x, y = y, color = pos, shape = pos)) +
  geom_point(size = 5) +
  my_theme

# map_dat <- lapply(1L:1370, function(x)
#   data.frame(x = x, y = 1L:1018, pos = NA)) %>% 
#   do.call(rbind, .)
# 
# for(i in 1L:nrow(dat)) {
#   map_dat[map_dat[["x"]] == dat[i, "x"] & map_dat[["y"]] == dat[i, "y"], "pos"] <- dat[i, "pos"]
# }
# 
# ggplot(map_dat, aes(x = x, y = y, fill = pos)) +
#   geom_point()


ggplot(dat, aes(x = ch_loc, fill = pos)) +
  geom_density(alpha = 0.5) +
  my_theme

ggplot(dat, aes(x = ch_1, fill = pos)) +
  geom_density(alpha = 0.5) +
  my_theme

ggplot(dat, aes(x = size, y = ratio, color = pos)) +
  geom_point(size = 4) +
  my_theme

dpcR:::fl(sum(dat[["pos"]])/nrow(dat))
binom.confint(x = sum(dat[["pos"]]), n = nrow(dat), methods = "wilson")

plot_dens <- function(average) {
  conf <- dpcR::dpcr_density(k = sum(dat[["pos"]]), n = nrow(dat), average = average, 
                             methods = "wilson", plot = FALSE)
  
  dens <- data.frame(dpcR:::dpcr_calculator(k = sum(dat[["pos"]]), n = nrow(dat), average = average))
  colnames(dens) <- c("x", "y")
  
  dens[["conf_low"]] <- dens[["x"]] <= conf[["lower"]] 
  dens[["conf_up"]] <- dens[["x"]] >= conf[["upper"]]
  
  ggplot(dens, aes(x = x, y = y)) + 
    #geom_area(aes(fill = y > 0)) + 
    geom_area(aes(fill = conf_up)) + 
    geom_area(aes(fill = conf_low)) +
    geom_line(colour = "black", size = 1.2) + 
    scale_fill_manual(values = c("FALSE" = NA, "TRUE" = "grey"), guide = FALSE) +
    scale_y_continuous("Density") +
    my_theme
}

# plot_dens(TRUE) + scale_x_continuous(expression(lambda))

p_scatter <- ggplotGrob(ggplot(dat, aes(x = ch_loc, y = ch_1, color = status, shape = status)) +
  geom_point(size = 2) +
  geom_abline(slope = 1, linetype = "dashed") +
  scale_x_continuous("Location channel") +
  scale_y_continuous("Droplet detection channel") +
  scale_shape_discrete("Droplet status") +
  scale_color_manual("Droplet status", values = c("red", "blue")) +
  my_theme)

p_dens <- ggplotGrob(plot_dens(FALSE) + 
  #geom_vline(xintercept = sum(dat[["pos"]]), linetype = "dotted", size = 2) +
  scale_x_continuous("Number of positive partitions"))

tiff("R_Diagramm.tiff", height = 10.32, width = 20, units = "cm", res = 150)
grid.arrange(textGrob("A", x = 0.75, y = 0.9, gp=gpar(fontsize=24)), p_scatter,
             textGrob("B", x = 0.75, y = 0.9, gp=gpar(fontsize=24)), p_dens,
             nrow = 1, ncol = 4, widths = c(0.05, 0.95, 0.05, 0.95))
dev.off()
