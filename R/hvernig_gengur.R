# Shiny app sem ber saman stöðu verðbólgunnar, atvinnuleysis, kaupmáttar og fleira til að átta sig á 
# hvernig staðan breytist m.v. upphaf nýrra ríkisstjórna



# 1.0.0 - SETUP ----

library(tidyverse)
library(zoo)
library(tempdisagg)
library(timetk)

# 1.0.1 Helper functions ----

fix_date <- function(string, quarterly = FALSE) {
  
  if (quarterly) {
    temp_string <- str_replace(string, "Á", "Q")
    date(as.yearqtr(temp_string))
  } else {
    make_date(str_sub(string, 1, 4), str_sub(string, 6, 7))
  }
  
}



# 2.0.0 - DATA ----

# 2.1.0 Ríkisstjórnir ----

rikisstjornir_tbl <- tibble(
  date = as.Date(c(
    "2024-12-21",  # Kristrún Frostadóttir
    "2024-04-09",  # Bjarni Benediktsson 2 - sama flokkar og Katrín
    "2021-11-28",  # Katrín Jakobsdóttir 2 - endurnýjun
    "2017-11-30",  # Katrín Jakobsdóttir 1
    "2017-01-11",  # Bjarni Benediktsson 1
    "2016-04-07",  # Sigurður Ingi - bætti við Viðreisn og Björt framtíð
    "2013-05-23",  # Sigmundur Davíð
    "2009-02-01",  # Jóhanna Sigurðardóttir
    "2007-05-24",  # Geir H. Haarde - skipti Framsókn út fyrir Samfylkingu
    "1995-04-23"   # Davíð Oddsson → Halldór Ásgrímsson → Geir (með Framsókn)
  )),
  government = c(
    "Samfylkingin, Viðreisn, Flokkur fólksins",
    "Sjálfstæðisflokkur, Framsókn, Vinstri græn",
    "Sjálfstæðisflokkur, Framsókn, Vinstri græn",
    "Sjálfstæðisflokkur, Framsókn, Vinstri græn",
    "Sjálfstæðisflokkur, Viðreisn, Björt framtíð",
    "Sjálfstæðisflokkur, Framsókn, Viðreisn, Björt framtíð",
    "Sjálfstæðisflokkur, Framsókn",
    "Samfylkingin, Vinstri græn",
    "Sjálfstæðisflokkur, Samfylkingin",
    "Sjálfstæðisflokkur, Framsóknarflokkur"
  )
) |>
  mutate(date = floor_date(date, "month")) |>
  arrange(date) |>
  pad_by_time(.date_var = date, .by = "month") |>
  fill(government, .direction = "down")

# Núvernadi ríkisstjórn
# nuverandi_rikisstjorn_tbl <- tibble(
#   date = seq.Date(from = as.Date("2024-12-01"), by = "month", length.out = 12*4),
#   government = "Samfylking, Viðreisn, Flokkur fólksins"
# )

# Finalize
rikisstjornir_tbl <- rikisstjornir_tbl |> 
  #bind_rows(nuverandi_rikisstjorn_tbl) |> 
  mutate(change_flag = government != lag(government, default = first(government))) |>
  mutate(period_group = cumsum(change_flag)) |>
  group_by(government, period_group) |>
  mutate(new_name = paste0(government, " (", min(date), " - ", max(date), ")")) |>
  ungroup() |>
  select(-c(change_flag, period_group))


# rikisstjornir_tbl <- rikisstjornir_tbl |> 
#   group_by(new_name) |> 
#   mutate(manudir = row_number()) |> 
#   ungroup()

# 2.2.0 Hagstærðir ----


# 2.2.1 Mánaðarlegar ----

# Vísitala neysluverðs
vnv_tbl <- read_csv2("https://px.hagstofa.is:443/pxis/sq/d37f3711-20e0-47ef-924b-8691f3cb6f11") |> 
  set_names("date", "vnv", "vnvh") |> 
  select(-vnvh) |> 
  mutate(
    date = fix_date(date),
    vnv = vnv / 10
  )

# Atvinnuleysi og starfandi
vmsk_tbl <- read_csv2("https://px.hagstofa.is:443/pxis/sq/8b9531d3-8044-4da0-80c4-d3e25bda3de5") |> 
  set_names("date", "mannfjoldi", "atvinnulausir", "starfandi") |> 
  mutate(
    atvinnuleysi = atvinnulausir / mannfjoldi,
    epop = starfandi / mannfjoldi,
    date = fix_date(date)
  ) |> 
  select(date, atvinnuleysi, epop)


# Húsnæðisverð
husnaedisverd_tbl <- read_csv2("https://px.hagstofa.is:443/pxis/sq/e7cdd362-9b81-4d56-a7a8-756c0a830a19") |> 
  set_names("date", "Fjölbýli (hbs)") |> 
  mutate(date = fix_date(date))


# Stýrivextir
styrivextir_tbl <- read_csv2("data/meginvextir.csv") |> 
  set_names("date", "Stýrivextir") |> 
  mutate(
    date = floor_date(dmy(date), "month"),
    Stýrivextir = Stýrivextir / 100
  ) |> 
  group_by(date) |> 
  summarise(Stýrivextir = max(Stýrivextir))


# Combine
data_m_tbl <- vnv_tbl |> 
  left_join(vmsk_tbl) |> 
  #left_join(husnaedisverd_tbl) |> 
  left_join(styrivextir_tbl) |> 
  set_names("date", "Verðlag", "Atvinnuleysi", "Hlutfall starfandi", "Stýrivextir") |> 
  pivot_longer(cols = -date) |> 
  drop_na() |> 
  left_join(rikisstjornir_tbl) |> 
  drop_na()


# 2.2.2 Ársfjórðungslegar ---------------------------------------------------------------------

# mannfjoldi
mannfjoldi_tbl <- read_csv2("https://px.hagstofa.is:443/pxis/sq/2642478f-0f2f-4162-a5fd-14813651e01d") |> 
  set_names("date", "mannfjoldi") |> 
  mutate(
    date = make_date(year = date)
  )

mannfjoldi_quarterly <- td(mannfjoldi_tbl ~ 1, to = "quarterly", method = "fast")

mannfjoldi_q_tbl <- predict(mannfjoldi_quarterly) |> 
  set_names("date", "mannfjoldi")

# vnv
vnv_q_tbl <- vnv_tbl |> 
  mutate(date = floor_date(date, "quarter")) |> 
  group_by(date) |> 
  summarise(vnv = mean(vnv))


# Landsframleiðsla
gdp_tbl <- read_csv2("https://px.hagstofa.is:443/pxis/sq/9848ed8b-4a87-4b3b-9e91-2d2ba1ca4afa") |> 
  select(-2) |> 
  set_names("date", "gdp") |> 
  mutate(date = fix_date(date, quarterly = TRUE))

gdp_tbl <- gdp_tbl |> 
  left_join(mannfjoldi_q_tbl) |> 
  mutate(gdp_per_capita = gdp / mannfjoldi)


# ráðstöfunartekjur
radst_tbl <- read_csv2("https://px.hagstofa.is:443/pxis/sq/7c5a4053-f8fd-4549-8bdc-ccbdcc97a9f3", na = "..") |>
  drop_na() |> 
  set_names("date", "radstofunartekjur") |> 
  mutate(
    date = fix_date(date, quarterly = TRUE)
  ) |> 
  left_join(vnv_q_tbl) |> 
  mutate(radstofunartekjur_fast = radstofunartekjur / vnv) |> 
  select(-c(radstofunartekjur, vnv))


# Combine quarterly
data_q_tbl <- gdp_tbl |>
  select(date, gdp_per_capita) |>
  left_join(radst_tbl) |>
  set_names("date", "Landsframleiðsla", "Ráðstöfunartekjur") |>
  pivot_longer(cols = -date) |>
  drop_na() |>
  left_join(rikisstjornir_tbl) |>
  drop_na()


# 2.2.3 Disaggregate quarterly to monthly and merge with data_m_tbl -------------------------

disagg_to_monthly <- function(df_q, col_name) {
  df_wide <- df_q |>
    filter(name == col_name) |>
    select(date, value) |>
    drop_na()

  # Convert to ts object (quarterly)
  start_yr  <- year(min(df_wide$date))
  start_qtr <- quarter(min(df_wide$date))
  ts_q <- ts(df_wide$value, start = c(start_yr, start_qtr), frequency = 4)

  # Disaggregate to monthly using denton-cholette (interpolates, does NOT divide by 4)
  model <- td(ts_q ~ 1, to = "monthly", method = "denton-cholette")

  predict(model) |>
    as_tibble() |>
    set_names("value") |>
    mutate(
      date = seq.Date(
        from  = as.Date(paste0(start_yr, "-", sprintf("%02d", (start_qtr - 1) * 3 + 1), "-01")),
        by    = "month",
        length.out = n()
      ),
      name = col_name
    ) |>
    select(date, name, value)
}

quarterly_series <- c("Landsframleiðsla", "Ráðstöfunartekjur")

data_q_monthly_tbl <- map(quarterly_series, ~ disagg_to_monthly(data_q_tbl, .x)) |>
  bind_rows() |>
  left_join(rikisstjornir_tbl) |>
  drop_na()

# Combined monthly dataset (original monthly + disaggregated quarterly)
data_all_m_tbl <- bind_rows(data_m_tbl, data_q_monthly_tbl)



# 3.0.0 Hvernig gengur ------------------------------------------------------------------------

data_m_tbl |>
  #filter(name == "Verðlag") |>
  group_by(name, new_name) |>
  mutate(
    manudir = row_number(),
    delta = value - value[1]
  ) |>
  ungroup() |>
  filter(manudir <= 60) |> 
  ggplot(aes(x = manudir, y = delta, col = new_name)) +
  geom_line() +
  facet_wrap(~name, scales = "free", ncol = 2) +
  theme_minimal() +
  theme(
    legend.title = element_blank()
  )


data_q_tbl |> 
  #filter(name == "Ráðstöfunartekjur") |> 
  group_by(name, new_name) |>
  mutate(
    manudir = row_number(),
    delta = value - value[1]
  ) |>
  ungroup() |>
  ggplot(aes(x = manudir, y = delta, col = new_name)) +
  geom_line() +
  facet_wrap(~ name, scales = "free_y") +
  theme(legend.position = "bottom")
