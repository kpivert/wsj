require(ggthemes)
require(ggtext)
require(tictoc)
require(tidyverse)

df <- data.frame(
  DATE = seq(as.Date("1913-01-01"), as.Date("2020-12-01"), "month")
  ) %>% 
  mutate(
    var = rnorm(n = nrow(.), mean = 0, sd = 0.5)
  )

# Years for X Axis Labels

yr_labs <- seq(as.Date("1915-01-01"), as.Date("2020-01-01"), by = "5 years") %>% 
  str_sub(start = 1, end = 4) 

yr_labs <- case_when(
  yr_labs == "1915" ~ "1915",
  yr_labs == "2020" ~ "2020",
  TRUE ~ str_c("'", str_sub(yr_labs, 3, 4))
)

tic()
ggplot(
  df, 
  aes(
    x = DATE,
    y = var
    )
  ) +
  geom_line(
    lwd = 0.2,
    color = "blue"
  ) +
  scale_x_continuous(
    breaks = seq(as.Date("1915-01-01"), as.Date("2020-01-01"), by = "5 years"), 
    labels = yr_labs
  ) +
  # theme_tufte() +
  theme_minimal(base_family = "Roboto") +
  theme(
    panel.grid.major.y = element_line(size = .1),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(
      vjust = - 0.8, 
      margin = margin(l = 20, r = -20)
    ),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(hjust = 0)
  ) +
  labs(
    x = "", 
    y = "",
    caption = "Source: Randomness",
    title = "Reprex Title",
    subtitle = "Reprex Subtitle"
  ) +
  # Annotations ----
# 1. WW I ----
# geom_segment(
#   aes(
#     x = as.Date("1914-09-01"), 
#     y = 1.25,
#     xend = as.Date("1918-11-01"),
#     yend = 1.25
#   ),
#   lwd = 1
# ) +
geom_segment(
  data = tibble(
    x = as.Date("1914-09-01"), 
    y = 1.25,
    xend = as.Date("1918-11-01"),
    yend = 1.25
  ),
  aes(
    x = x, 
    y = y,
    xend = xend,
    yend = yend
  ),
  lwd = 1
) +
  # geom_segment(
  #   aes(
  #     x = as.Date("1916-08-01"), 
  #     y = 1.25,
  #     xend = as.Date("1916-08-01"),
  #     yend = 2.28
  #   ),
  #   linetype = "dotted"
  # ) +
  geom_segment(
    data = tibble(
      x = as.Date("1916-08-01"), 
      y = 1.25,
      xend = as.Date("1916-08-01"),
      yend = 2.28
    ),
    aes(
      x = x, 
      y = y,
      xend = xend,
      yend = yend
    ),
    linetype = "dotted"
  ) +
# geom_richtext(
#   x = as.Date("1916-01-01"),
#   y = 1.28,
#   label = "<b>1914-18</b><br>World<br>War I",
#   fill = NA,
#   label.color = NA
# ) 
  # annotate(
  #   "segment",  
  #   x = as.Date("1914-09-01"),   
  #   y = 1.25, 
  #   xend = as.Date("1918-11-01"), 
  #   yend = 1.25, 
  #   lwd = 1 
  #   ) + 
  annotate(
    "richtext", 
    x = as.Date("1916-01-01"), 
    y = 1.28, 
    label = "<b>1914-18</b><br>World<br>War I", 
    fill = NA, 
    label.color = NA,
    family = "Roboto",
    size = 4
    )

toc()



