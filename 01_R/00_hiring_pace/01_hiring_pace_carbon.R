require(extrafont)
loadfonts(quiet = TRUE)
require(ggthemes)
require(tidyverse)

# Data from St. Louis Federal Reserve Bank
# https://fred.stlouisfed.org/series/PAYEMS

bls <- read_csv(
  here::here("00_data", "00_hiring_pace/PAYEMS.csv") 
) 

# Convert Values to Millions &
# Calculate Cumulative Change Since February 2020
# Add Row Number Variable to Use Numeric X axis scale

bls <- bls %>% 
  mutate(
    PAYEMS = PAYEMS * 1000
  ) %>% 
  slice(-1) %>% 
  mutate(
    prev_month = lead(PAYEMS),
    change = PAYEMS - 152523000,
    dt = row_number()
  )

ggplot(
  bls,
  aes(
    x = dt,
    y = change
  )
) +
  geom_step(
    color = "#ff8200",
    lwd = 1.2
  ) +
  geom_rect(
    aes(
      ymin = 0,
      ymax = change,
      xmin = dt,
      xmax = lead(dt)
    ),
    fill = "#ff8200",
    alpha = 0.5
  ) +
  scale_y_continuous(
    breaks = seq(0, -25e6, -5e6),
    labels = c("0", str_c("\u2212", seq(5, 25, 5)))
  ) +
  expand_limits(
    y = -25.1e6
  ) +
  theme_tufte(
    base_family = "Lato",
    base_size = 12
  ) +
  geom_hline(
    aes(
      yintercept = -8.2e6
    ),
    linetype = "dotted"
  ) +
  geom_hline(
    aes(
      yintercept = 0
    )
  ) +
  annotate(
    geom = "text",
    x = 10,
    y = -6e6,
    label = "8.2 million fewer jobs",
    family = "Lato Bold"
  ) +
  geom_curve(
    aes(
      x = 8,
      y = -6e6,
      xend = 7.5,
      yend = -8e6
    ),
    curvature = 0.4,
    arrow = arrow(
      length = unit(0.15, units = "cm"),
      type = "closed")
  ) +
  labs(
    title = "Total nonfarm payrolls, cumulative change since February 2020",
    y = "", 
    x = ""
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    # text = element_text(family = "Lato", size = 12),
    axis.text.y = element_text(
      family = "Lato",
      size = 9, 
      vjust = -.8),
    axis.ticks.y = element_blank(), 
    panel.grid.major.y = element_line(color = "#efefef")
  ) +
  scale_x_continuous(
    breaks = 1:15,
    labels = c("Feb. 2020", rep("", 10), "2021", rep("", 3))
  ) 

ggsave(
  filename = here::here("02_figs/hiring_pace_r2.png"),
  width = 7,
  height = 7 * 0.618,
  units = "in",
  dpi = 600,
  type = "cairo"
)
