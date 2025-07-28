# 2025-07-27 Economist Choropleth in ggplot2

# 00 Libraries ------------------------------------------------------------

require(tabulapdf)
require(systemfonts)
require(extrafont)
loadfonts(quiet = TRUE)
require(patchwork)
require(tidyverse)

# 01 Data -----------------------------------------------------------------

hilltop_df <-
  extract_tables(
    file = "05_choropleth/Eliminating Provider Taxes May 6 2025.pdf",
    pages = 2
  )

locate_areas(
  file = "05_choropleth/Eliminating Provider Taxes May 6 2025.pdf",
  pages = 2
)

first_table <- c(122.40887, 52.40394, 584.73399, 297.22167)

second_table <- c(120.4581, 304.0493, 569.1281, 563.4975)

tbl_part_1 <-
  extract_tables(
    file = "05_choropleth/Eliminating Provider Taxes May 6 2025.pdf",
    pages = 2, 
    area = list(first_table), 
    guess = FALSE
  )

tbl_part_2 <-
  extract_tables(
    file = "05_choropleth/Eliminating Provider Taxes May 6 2025.pdf",
    pages = 2, 
    area = list(second_table), 
    guess = FALSE
  ) 

tbl_part_1 <-
  tbl_part_1 |> 
  pluck(1) |> 
  set_names(paste0("col_", 1:3))

tbl_part_2 <-
  tbl_part_2 |> 
  pluck(1) |> 
  select(1, 3, 5) |> 
  set_names(paste0("col_", 1:3))  

# 02 Cleaned Data ---------------------------------------------------------

tbl_part_1_cleaned_df <-
  tbl_part_1 |> 
  slice(4:29) |> 
  mutate(
    col_3 = str_extract(
      col_3,
      "([0-9\\.\\%])*$"
    )
  ) |> 
  select(
    State = col_1,
    Pct = col_3
  ) |> 
  mutate(
    Pct = parse_number(Pct)
  )

tbl_part_2_cleaned_df <-
  tbl_part_2 |> 
    mutate(
      State = str_extract(
        col_1,
        "([A-Z]){2}"
      ),
      Pct = parse_number(col_3)
    ) |> 
    select(
      State, Pct
    )

df <-
  bind_rows(
    tbl_part_1_cleaned_df,
    tbl_part_2_cleaned_df
  )

# write_rds(
#   df, 
#   file = "05_choropleth/hilltop.rds"
# )

df <-
  read_rds(
    file = "05_choropleth/hilltop.rds"
  )

# 03 Viz ------------------------------------------------------------------

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

ggplot(
  viz_df,
  aes(
    x = x, 
    y = y, 
    group = State
    )
  ) +
  geom_tile(
    # fill = "#fff",
    fill = pal[6],
    color = "#000"
  ) +
  theme_void() +
  coord_equal() +
  geom_text(
    aes(
      label = State
    )
  ) +
  theme(
    plot.background = element_rect(
      color = pal[6],
      fill = pal[6]
    ),
    panel.background = element_rect(
      fill = pal[6],
      color = pal[6]
    )
  ) 

a <-
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
      color = "#000"
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
      plot.caption = element_text(hjust = 0.1),
      plot.background = element_rect(
        color = pal[6],
        fill = pal[6]
      ),
      panel.background = element_rect(
        fill = pal[6],
        color = pal[6]
      ),
      text = element_text(family = "Arial Narrow")
    ) 


a +
  labs(
    title = "   Federal Medicaid funding from hospital & nursing-\nhome taxes, forecast for fiscal year 2026, %"
  ) +
  theme(
    plot.title = element_text(
      size = 10,
      face = "bold",
      hjust = 0.2
    ),
    plot.title.position = "plot"
  )

# ggsave(
#   filename = "05_choropleth/first_take_2.png",
#   width = 3,
#   height = 3,
#   units = "in",
#   dpi = 300
# )

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
  # expand_limits(
  #   y = 10
  # ) +
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

# a +
#   labs(
#     title = "   Federal Medicaid funding from hospital & nursing-\nhome taxes, forecast for fiscal year 2026, %"
#   ) +
#   guides(
#     fill = "legend"
#   ) +
#   theme(
#     plot.title = element_text(
#       size = 10,
#       face = "bold",
#       hjust = 0.2
#     ),
#     plot.title.position = "plot",
#     legend.position = "top"
#   )

top_legend / a


a

(top_legend + coord_fixed(ratio = .65)) / a

b <-
  top_legend +
  coord_fixed(
    ratio = 0.7
  ) +
  theme(
    plot.margin = margin(10, 5, 10, 40, "pt")
  )

(b / a) +
  plot_annotation(
    title = "Federal Medicaid funding from hospital & nursing-\nhome taxes, forecast for fiscal year 2026, %"
  ) &
  theme(
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
#   filename = "05_choropleth/third_take.png",
#   width = 3, 
#   height = 3.1,
#   units = "in",
#   dpi = 300
# )
