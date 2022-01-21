ggplot(
  df, 
  aes(
    x = DATE, 
    y = pct_change
  )
) +
  geom_line(
    color = "#0077c8"
  ) +
  geom_hline(
    yintercept = 0, 
    lwd = 0.5, 
    color = "#000000"
  ) +
  # scale_x_date(
  #   # date_breaks = "5 years",
  #   # date_labels = "'%y" #,
  #   # limits = c(as.Date("1916-01-01"), as.Date("2019-01-01"))
  # ) +
  scale_x_continuous(
    breaks = seq(as.Date("1915-01-01"), as.Date("2020-01-01"), by = "5 years"), 
    labels = yr_labs
  ) +
  scale_y_continuous(
    breaks = seq(-.2, 0.25, 0.05),
    labels = scales::percent_format(accuracy = 1)
  ) +
  theme_tufte(
    base_family = "Mukta"
  ) +
  expand_limits(
    x = as.Date("2021-06-01"),
    y = .29
  ) +
  geom_polygon(
    inherit.aes = FALSE,
    data = recession,
    aes(
      x = x, 
      y = y,
      group = id
    ),
    fill = "grey50",
    alpha = 0.2
  ) +
  theme(
    panel.grid.major.y = element_line(size = .1),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(
      vjust = - 0.8, 
      margin = margin(l = 20, r = -20),
      family = "Roboto Condensed"
    ),
    plot.title = element_text(family = "Mukta SemiBold", size = 16),
    plot.subtitle = element_markdown(family = "Mukta", size = 10),
    plot.caption = element_text(hjust = 0)
  ) +
  labs(
    x = "", 
    y = "",
    caption = "Source: U.S. Bureau of Labor Statistics",
    title = "Inflation's Ups and Downs",
    subtitle = "Consumer Price Index, monthly percentage change from previous year &emsp; <img src='/Users/kurtispivert/Documents/00_gh/wsj/02_figs/sp.png' width='7'/> RECESSION"
  ) +
  # Annotations ----
# 1. WW I ----
geom_segment(
  data = tibble(
    x = as.Date("1914-09-01"), 
    y = 0.25,
    xend = as.Date("1918-11-01"),
    yend = 0.25
  ),
  aes(
    x = x, 
    y = y,
    xend = xend,
    yend = yend
  ),
    lwd = 1
  ) +
  geom_segment(
    data = tibble(
      x = as.Date("1916-08-01"), 
      y = .25,
      xend = as.Date("1916-08-01"),
      yend = .28
    ),
    aes(
      x = x, 
      y = y,
      xend = xend,
      yend = yend
    ),
    linetype = "dotted"
  ) +
  # Uncomment before plotting ----
geom_richtext(
  data = tibble(
    x = as.Date("1916-01-01"),
    y = .3,
    label = "<b>1914-18</b><br>World<br>War I"
    ),
  aes(
    x = x, 
    y = y, 
    label = label
  ),
  family = "Mukta",
  # size = 1,
  fill = NA,
  label.color = NA
) +
  # 2. Great Depression ----
geom_segment(
  data = tibble(
    x = as.Date("1929-09-01"), 
    y = 0.25,
    xend = as.Date("1938-07-01"),
    yend = 0.25
  ),
  aes(
    x = x, 
    y = y,
    xend = xend,
    yend = yend
  ),
  lwd = 1
) +
  geom_segment(
    data = tibble(
      x = as.Date("1934-08-01"), 
      y = .25,
      xend = as.Date("1934-08-01"),
      yend = .28
    ),
    aes(
      x = x, 
      y = y,
      xend = xend,
      yend = yend
    ),
    linetype = "dotted"
  ) +
  # Uncomment before plotting ----
geom_richtext(
  data = tibble(
    x = as.Date("1934-08-01"),
    y = .3,
    label = "<b>1929-39</b><br>Great<br>Depression"
    ),
  aes(
    x = x, 
    y = y, 
    label = label
  ),
  family = "Mukta",
  # size = 1,
  fill = NA,
  label.color = NA
) +
# 3. WW II  ----
geom_segment(
  data = tibble(
    x = as.Date("1939-09-01"), 
    y = 0.25,
    xend = as.Date("1945-08-01"),
    yend = 0.25
  ),
  aes(
    x = x, 
    y = y,
    xend = xend,
    yend = yend
  ),
  lwd = 1
) +
  geom_segment(
    data = tibble(
      x = as.Date("1942-10-01"), 
      y = .25,
      xend = as.Date("1942-10-01"),
      yend = .28
    ),
    aes(
      x = x, 
      y = y,
      xend = xend,
      yend = yend
    ),
    linetype = "dotted"
  ) +
  # Uncomment before plotting ----
geom_richtext(
  data = tibble(
    x = as.Date("1942-10-01"),
    y = .3,
    label = "<b>1939-45</b><br>World<br>War II"
    ),
  aes(
    x = x, 
    y = y, 
    label = label
  ),
  family = "Mukta",
  # size = 1,
  fill = NA,
  label.color = NA
) +
# 4. Korean War  ----
geom_segment(
  data = tibble(
    x = as.Date("1950-06-01"), 
    y = 0.25,
    xend = as.Date("1953-07-01"),
    yend = 0.25
  ),
  aes(
    x = x, 
    y = y,
    xend = xend,
    yend = yend
  ),
  lwd = 1
) +
  geom_segment(
    data = tibble(
      x = as.Date("1951-12-01"), 
      y = .25,
      xend = as.Date("1951-12-01"),
      yend = .28
    ),
    aes(
      x = x, 
      y = y,
      xend = xend,
      yend = yend
    ),
    linetype = "dotted"
  ) +
  # Uncomment before plotting ----
  geom_richtext(
    data = tibble(
      x = as.Date("1951-12-01"),
      y = .3,
      label = "<b>1950-53</b><br>Korean<br>War"
      ),
    aes(
      x = x, 
      y = y, 
      label = label
    ),
  family = "Mukta",
  # size = 1,
  fill = NA,
  label.color = NA
) +
# 5. January 1964 LBJ Great Society  ----
geom_segment(
  data = tibble(
    x = as.Date("1964-01-01"), 
    y = -.20,
    xend = as.Date("1964-01-01"),
    yend = .28
  ),
  aes(
    x = x, 
    y = y,
    xend = xend,
    yend = yend
  ),
  linetype = "dotted"
) +  
  # Uncomment before plotting ----
geom_richtext(
  data = tibble(
    x = as.Date("1955-12-01"),
    y = .3,
    label = "<b>1964</b>:LBJ's Great<br>Society domestic<br>programs start"
    ),
  aes(
    x = x, 
    y = y, 
    label = label
  ),
  family = "Mukta",
  # size = 1,
  fill = NA,
  label.color = NA
) +
# 6. October 1966 Food Price Protests ----
geom_segment(
  data = tibble(
    x = as.Date("1966-10-01"), 
    y = -.20,
    xend = as.Date("1966-10-01"),
    yend = .28
  ),
  aes(
    x = x, 
    y = y,
    xend = xend,
    yend = yend
  ),
  linetype = "dotted"
) +  
  # Uncomment before plotting ----
geom_richtext(
  data = tibble(
    x = as.Date("1966-09-01"),
    y = .3,
    label = "<b>1966</b><br>Food price<br>protests"
    ),
  aes(
    x = x, 
    y = y, 
    label = label
  ),
  family = "Mukta",
  # size = 1,
  fill = NA,
  label.color = NA
) +
# 7. January 1970 Burns Fed Chair ----
geom_segment(
  data = tibble(
    x = as.Date("1970-01-01"), 
    y = -.20,
    xend = as.Date("1970-01-01"),
    yend = .16
  ),
  aes(
    x = x, 
    y = y,
    xend = xend,
    yend = yend
  ),
  linetype = "dotted"
) +  
geom_textbox(
  data = tibble(
    label = "**1970**<br>Arthur Burns<br>becomes Fed<br>chairman",
    x = as.Date("1967-01-01"),
    y = .22,
    box.color = "white",
    fill = "white"
  ),
  aes(
    label = label,
    x = x,
    y = y,
    box.color = box.color,
    fill = NULL
  ),
    hjust = 0,
    vjust = 1,
    family = "Mukta",
    size = 3,
    maxwidth = unit(.5, "strwidth", "Arthur Burns   ")
) +
# 8. August 1971 wage & Price Controls ----
geom_segment(
  data = tibble(
    x = as.Date("1971-08-01"), 
    y = -.20,
    xend = as.Date("1971-08-01"),
    yend = -.05
  ),
  aes(
    x = x, 
    y = y,
    xend = xend,
    yend = yend
  ),
  linetype = "dotted"
) +  
geom_textbox(
  data = tibble(
    label = "**1971**<br>Wage and<br>price controls;<br>U.S. delinks<br>dollar from<br>gold",
    x = as.Date("1971-06-01"),
    y = -.06,
    box.color = "white",
    fill = "white"
  ),
  aes(
    label = label,
    x = x,
    y = y,
    box.color = box.color,
    fill = NULL
  ),
    hjust = 0,
    vjust = 1,
    family = "Mukta",
    size = 3,
    maxwidth = unit(.5, "strwidth", "Arthur Burns   ")
) +

# 9. October 1973 OPEC ----
geom_segment(
  data = tibble(
    x = as.Date("1973-10-01"), 
    y = -.20,
    xend = as.Date("1973-10-01"),
    yend = .28
  ),
  aes(
    x = x, 
    y = y,
    xend = xend,
    yend = yend
  ),
  linetype = "dotted"
) +  
  # Uncomment before plotting ----
geom_richtext(
  data = tibble(
    x = as.Date("1973-08-01"),
    y = .3,
    label = "<b>1973</b><br>OPEC oil<br>Embargo"
    ),
  aes(
    x = x, 
    y = y, 
    label = label
  ),
  family = "Mukta",
  # size = 1,
  fill = NA,
  label.color = NA
) +
# 10. August 1979 Volker ----
geom_segment(
  data = tibble(
    x = as.Date("1979-08-01"), 
    y = -.20,
    xend = as.Date("1979-08-01"),
    yend = .28
  ),
  aes(
    x = x, 
    y = y,
    xend = xend,
    yend = yend
  ),
  linetype = "dotted"
) +  
  # Uncomment before plotting ----
geom_richtext(
  data = tibble(
    x = as.Date("1979-07-01"),
    y = .3,
    label = "<b>1979</b><br>Paul Volker becomes<br>Fed Chairman"
    ),
  aes(
    x = x, 
    y = y, 
    label = label
  ),
  family = "Mukta",
  # size = 1,
  fill = NA,
  label.color = NA
) +
# 11. January 2012 Fed 2% Target ----
geom_segment(
  data = tibble(
    x = as.Date("2012-01-01"), 
    y = -.20,
    xend = as.Date("2012-01-01"),
    yend = .28
  ),
  aes(
    x = x, 
    y = y,
    xend = xend,
    yend = yend
  ),
  linetype = "dotted"
) # +  
# Uncomment before plotting ----
geom_richtext(
  data = tibble(
    x = as.Date("2011-10-01"),
    y = .3,
    label = "<b>2012</b><br>Fed adopts formal<br>2% inflation target"
    ),
  aes(
    x = x, 
    y = y, 
    label = label
  ),
  family = "Mukta",
  # size = 1,
  fill = NA,
  label.color = NA
) 



# ggsave(
#   filename = "test_cpi_2.jpeg",
#   width = 12,
#   height = 12 * 0.618,
#   units = "in",
#   dpi = 600
# )

