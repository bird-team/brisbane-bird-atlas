# Initialization
## load parameters
parameters <- yaml::read_yaml("data/parameters/parameters.yaml")

# Main processing
p <- ggplot2::ggplot(data.frame(x = 0, y = 0),
                     ggplot2::aes(x = x, y = y)) +
     ggplot2::geom_text(label = "?", size = 30) +
     ggplot2::theme(
       axis.line = ggplot2::element_blank(),
       axis.text.x = ggplot2::element_blank(),
       axis.text.y = ggplot2::element_blank(),
       axis.ticks = ggplot2::element_blank(),
       axis.title.x = ggplot2::element_blank(),
       axis.title.y = ggplot2::element_blank(),
       legend.position="none",
       panel.background = ggplot2::element_rect(color = "grey70",
                                                fill = "grey70"),
       panel.border = ggplot2::element_blank(),
       panel.grid.major = ggplot2::element_blank(),
       panel.grid.minor = ggplot2::element_blank(),
       plot.background = ggplot2::element_blank(),
       plot.margin = ggplot2::unit(c(0, 0, 0, 0), "null"),
       panel.spacing = ggplot2::unit(c(0, 0, 0, 0), "null"),
       axis.ticks.length = ggplot2::unit(0, "null"))

# Exports
## save profile image
ggplot2::ggsave(paste0("assets/misc/missing-profile.png"), p,
                height = 2, width = 5, units = "in")

## save square image
ggplot2::ggsave(paste0("assets/misc/missing.png"), p,
                width = 1, height = 1, units = "in")
