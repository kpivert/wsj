require(ggplot2)
require(cowplot)

inset <- ~{
  counts <- table(mpg$drv)
  par(
    cex = 0.8,
    mar = c(3, 3, 1, 1),
    mgp = c(2, 1, 0)
  )
  barplot(counts, xlab = "drv", ylab = "count")
}

p <- ggplot(mpg, aes(displ, cty)) +
  geom_point() +
  theme_minimal_grid(12) # +
  # labs(
  #   title = "Plot Title"
  # )

ggdraw(p + theme_half_open(12)) +
  draw_plot(inset, .45, .45, .5, .5) +
  draw_plot_label(
    c("A", "B"),
    c(0, 0.45),
    c(1, 0.95),
    size = 12
  ) +
  draw_figure_label(
    label = "Federal Medicaid funding from hospital & nursing-\nhome taxes, forecast for fiscal year 2026, %",
    position = "top.left",
    fontface = "bold"
  )


ggdraw(get_title(p)) +
  draw_plot(p)
