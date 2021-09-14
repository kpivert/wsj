         
# Create Function to Add Recession Bars to ggplots -----

# Load Packages -----

require(tidyverse)
         
recession <-
function(
  Peak = "2020-02-01",   # Last Recession Start
  Trough = "2021-05-01", # Last Recession End or Current Date
  y_min = -.2,           # Minimum Y Axis Value
  y_max = .25            # Maximum Y Axis Value
  ) {
  
# Recession Data from 
# https://fredhelp.stlouisfed.org/fred/data/understanding-the-data/recession-bars/
  
  recession <- tibble::tribble(
    ~Peak, ~Trough,
    "1857-06-01", "1858-12-01",
    "1860-10-01", "1861-06-01",
    "1865-04-01", "1867-12-01",
    "1869-06-01", "1870-12-01",
    "1873-10-01", "1879-03-01",
    "1882-03-01", "1885-05-01",
    "1887-03-01", "1888-04-01",
    "1890-07-01", "1891-05-01",
    "1893-01-01", "1894-06-01",
    "1895-12-01", "1897-06-01",
    "1899-06-01", "1900-12-01",
    "1902-09-01", "1904-08-01",
    "1907-05-01", "1908-06-01",
    "1910-01-01", "1912-01-01",
    "1913-01-01", "1914-12-01",
    "1918-08-01", "1919-03-01",
    "1920-01-01", "1921-07-01",
    "1923-05-01", "1924-07-01",
    "1926-10-01", "1927-11-01",
    "1929-08-01", "1933-03-01",
    "1937-05-01", "1938-06-01",
    "1945-02-01", "1945-10-01",
    "1948-11-01", "1949-10-01",
    "1953-07-01", "1954-05-01",
    "1957-08-01", "1958-04-01",
    "1960-04-01", "1961-02-01",
    "1969-12-01", "1970-11-01",
    "1973-11-01", "1975-03-01",
    "1980-01-01", "1980-07-01",
    "1981-07-01", "1982-11-01",
    "1990-07-01", "1991-03-01",
    "2001-03-01", "2001-11-01",
    "2007-12-01", "2009-06-01" # ,
    # "2020-02-01", "2021-05-01" # Note: Leaving this as the current observation.
    #                            # Still in a recession. 
  )
  
  recession <- bind_rows(
    recession,
    tibble(
      Peak = Peak, 
      Trough = Trough
      )
    ) %>% 
    mutate(across(everything(), as.Date))
  
  pt_1 <- recession %>% 
    mutate(id = row_number()) %>% 
    relocate(id) %>% 
    pivot_longer(
      names_to = "recession_phase",
      values_to = "x", 
      -id
    ) 
  
  pt_2 <- recession %>% 
    mutate(id = row_number()) %>% 
    relocate(id) %>% 
    pivot_longer(
      names_to = "recession_phase",
      values_to = "x", 
      -id
    ) %>% 
    group_by(id) %>% 
    arrange(id, desc(x)) %>% 
    ungroup()
  
  
  recession_bars <- bind_rows(pt_1, pt_2) %>% 
    arrange(id) %>% 
    mutate(
      y = rep(c(y_min, y_min, y_max, y_max), nrow(.) / 4)
    )  

  return(recession_bars)
}
