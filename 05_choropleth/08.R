# Economist U.S, State Grid Choropleth Function

# 00 Libraries ------------------------------------------------------------

require(ggtext)
require(extrafont)
loadfonts(quiet = TRUE)
require(patchwork)
require(tidyverse)

# 01 Data -----------------------------------------------------------------

df <-
  read_rds(
    file = "05_choropleth/hilltop.rds"
  )

# 02 State Grid Choropleth Data -------------------------------------------

# Economist is 8 x 11 Square Grid

viz_df <-
  tribble(
    ~State, ~x, ~y,
    "AL", 7, 7, 
    "AK", 1, 1,
    "AZ", 2, 6,
    "AR", 5, 6,
    "CA", 1, 5,
    "CO", 3, 5,
    "CT", 10, 4,
    "DC", 9, 6,
    "DE", 10, 5,
    "FL", 9, 8,
    "GA", 8, 7,
    "HI", 1, 8,
    "ID", 2, 3,
    "IL", 6, 3,
    "IN", 6, 4,
    "IA", 5, 4,
    "KS", 4, 6,
    "KY", 6, 5,
    "LA", 5, 7,
    "ME", 11, 1,
    "MD", 9, 5,
    "MA", 10, 3,
    "MI", 7, 3,
    "MN", 5, 3,
    "MS", 6, 7,
    "MO", 5, 5,
    "MT", 3, 3,
    "NE", 4, 5,
    "NV", 2, 4,
    "NH", 11, 2, 
    "NJ", 9, 4, 
    "NM", 3, 6, 
    "NY", 9, 3, 
    "NC", 7, 6, 
    "ND", 4, 3, 
    "OH", 7, 4,
    "OK", 4, 7,
    "OR", 1, 4,
    "PA", 8, 4,
    "RI", 11, 4,
    "SC", 8, 6,
    "SD", 4, 4,
    "TN", 6, 6,
    "TX", 4, 8,
    "UT", 2, 5,
    "VT", 10, 2,
    "VA", 8, 5,
    "WA", 1, 3,
    "WV", 7, 5,
    "WI", 6, 2,
    "WY", 3, 4
  ) |> 
  mutate(
    y = case_when(
      y == 1 ~ 12, 
      y == 2 ~ 11, 
      y == 3 ~ 10, 
      y == 4 ~ 9, 
      y == 5 ~ 8, 
      y == 6 ~ 7, 
      y == 7 ~ 6, 
      y == 8 ~ 5, 
      y == 9 ~ 4, 
      y == 10 ~ 3, 
      y == 11 ~ 2, 
      y == 12 ~ 1
    )
  ) 

pal <-
  c("#EF1B26", "#F58469", "#FAAA90", "#FCC8B4", "#EEECE6", "#EDEBE3")

choropleth <-
  viz_df |> 
    left_join(
      df, 
      by = "State"
    ) |> 
    mutate(
      fill_color = case_when(
        between(Pct, 30, 40) ~ pal[1],
        between(Pct, 20, 30) ~ pal[2],
        between(Pct, 10, 20) ~ pal[3],
        between(Pct, 0.1, 10) ~ pal[4],
        Pct == 0 ~ pal[5]
      )
    ) |>
    mutate(
      text_color = if_else(
        fill_color == pal[1], 
        "#ffffff",
        "#000000"
      )
    ) |> 
    ggplot(
      aes(
        x = x, 
        y = y
      )
    ) +
    geom_tile(
      aes(
        fill = fill_color
      ),
      color = "#000",
      linewidth = 0.2
    ) +
    scale_fill_identity() +
    theme_void() +
    coord_equal() +
    geom_text(
      aes(
        label = State,
        color = text_color
      ),
      family = "Arial Narrow", 
      size = 3.4
    ) +
    scale_color_identity() +
    labs(
      caption = "Source: The Hilltop Institute"
    ) +
    theme(
      plot.caption = element_text(
        hjust = 0.06,
        family = "Arial Narrow", 
        color = "#5c5c5c"
      ),
      plot.caption.position = "plot",
      plot.background = element_rect(
        color = pal[6],
        fill = pal[6]
      ),
      panel.background = element_rect(
        fill = pal[6],
        color = pal[6]
      )
    ) 

scale_df <-
  tibble(
    x = seq(10, 40, 10),
    y = rep(0.1, 4),
    fill_color = c(pal[4], pal[3], pal[2], pal[1])
  )

zero_na_df <-
  tibble(
    x = 1,
    y = 0.1,
    fill_color = c(pal[6])
  )


top_legend <-
  ggplot(
    scale_df,
    aes(
      x = x, 
      y = y
    )
  ) +
  geom_tile(
    color = "#000000",
    aes(
      fill = fill_color
    ),
    linewidth = 0.4
  ) +
  scale_fill_identity() +
  theme_void(
    base_family = "Arial Narrow"
  ) +
  scale_x_continuous(
    breaks = seq(15, 45, 10),
    labels = seq(10, 40, 10)  |> as.character()
  ) +
  theme(
    axis.text.x.bottom = element_text(
      vjust = 0.5,
      size = 10
    ),
    plot.background = element_rect(
      color = pal[6],
      fill = pal[6]
    ),
    panel.background = element_rect(
      fill = pal[6],
      color = pal[6]
    )
  )

zero_na_legend <-
  ggplot(
    zero_na_df,
    aes(
      x = x, 
      y = y
    )
  ) +
  geom_tile(
    color = "#000000",
    aes(
      fill = fill_color
    ),
    linewidth = 0.4
  ) +
  scale_fill_identity() +
  theme_void(
    base_family = "Arial Narrow"
  ) +
  scale_x_continuous(
    breaks = 1,
    labels = "0"
  ) +
  theme(
    axis.text.x.bottom = element_text(
      vjust = 0.5,
      size = 10
    ),
    plot.background = element_rect(
      color = pal[6],
      fill = pal[6]
    ),
    panel.background = element_rect(
      fill = pal[6],
      color = pal[6]
    )
  ) +
  coord_fixed(ratio = 1)  


(
  (plot_spacer() +
    zero_na_legend + 
      theme(
        plot.margin = unit(c(0,0,0,30), "pt")
      )
     + 
    top_legend +
    plot_spacer()
  ) + 
  plot_layout(widths = c(0.5,1,5,1), nrow = 1)
) /
  choropleth +
  plot_layout(
    heights = c(0.05, 1)#, 
    # widths = c(0.6, 1)
  ) +  # Legend gets 0.2, main plot gets 1
  plot_annotation(
    title = "Federal Medicaid funding from hospital & nursing-<br>home taxes, forecast for fiscal year 2026, %"
  ) &
  theme(
    plot.title = element_textbox_simple(
      size = 11,
      family = "Arial Narrow Bold",
      hjust = 0.25,
      margin = margin(0, 0, 5, 0, unit = "pt")
    ),
    # plot.title.position = "plot",
    plot.background = element_rect(
        color = pal[6],
        fill = pal[6]
      ),
      panel.background = element_rect(
        fill = pal[6],
        color = pal[6]
      )
  )


# ggsave(
#   filename = "05_choropleth/economist_choropleth_ggplot_R5.png",
#   # width = 3.5, 
#   width = 3.4, 
#   height = 3.4,
#   units = "in",
#   dpi = 300
# )


(
  (plot_spacer() +
    zero_na_legend + 
      theme(
        plot.margin = unit(c(0,0,0,30), "pt")
      )
     + 
    top_legend +
    plot_spacer()
  ) + 
  plot_layout(widths = c(0.3,1,5,1.4), nrow = 1)
) /
  choropleth +
  plot_layout(
    heights = c(0.05, 1)#, 
    # widths = c(0.6, 1)
  ) +  # Legend gets 0.2, main plot gets 1
  plot_annotation(
    title = "Federal Medicaid funding from hospital & nursing-<br>home taxes, forecast for fiscal year 2026, %"
  ) &
  theme(
    plot.title = element_textbox_simple(
      size = 11,
      family = "Arial Narrow Bold",
      hjust = 0.25,
      margin = margin(0, 0, 5, 0, unit = "pt")
    ),
    # plot.title.position = "plot",
    plot.background = element_rect(
        color = pal[6],
        fill = pal[6]
      ),
      panel.background = element_rect(
        fill = pal[6],
        color = pal[6]
      )
  )

# ggsave(
#   filename = "05_choropleth/economist_choropleth_ggplot_R6.png",
#   # width = 3.5, 
#   width = 3.4, 
#   height = 3.4,
#   units = "in",
#   dpi = 300
# )
