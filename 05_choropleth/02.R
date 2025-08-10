# 2025-07-27 Economist Choropleth in ggplot2

# Part 1: Data Extraction -------------------------------------------------

# 00 Libraries ------------------------------------------------------------

require(tabulapdf)
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