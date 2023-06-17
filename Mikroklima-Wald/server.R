library(tidyverse)
library(readxl)
library(ggmap)
library(shiny)

theme_set(theme_minimal(base_size = 15))

# ETL Preprocessing -----

stations <- readxl::read_excel("data/WZE-Punkte.xlsx", sheet = "Koordinaten_LoggerSN_LNT") |>
  transmute(
    station_id = WZE_ID,
    lon = Longitude_WGS84_E,
    lat = Latitude_WGS84_N,
    hobo_SN, tms_SN, LRT, WZE_Punkt, WZE_ID
  )

read_logger_excel <- function(path, stations) {
  wze_id <- path |> str_remove("[-]") |> str_extract(stations$WZE_ID |> paste0(collapse = "|"))
  wze_pkt <- path |> str_extract(stations$WZE_Punkt |> paste(collapse = "|"))
  if (is.na(wze_id)) {
    wze_id <- stations |>
      filter(WZE_Punkt == wze_pkt) |>
      pull(WZE_ID) |>
      first()
  }

  readxl::read_excel(path) |>
    mutate(across(everything(), as.character)) |>
    rename(all_of(c("observation_id" = "#"))) |>
    pivot_longer(-observation_id) |>
    mutate(
      # TODO: add channel names, needs some additional pivoting
      # channel = name |> str_extract("Ch:.*") |> str_extract("[0-9]+") |> as.numeric(),
      # TODO: Check summer time: CEST vs CET
      name = case_when(
        str_detect(name, "Temperature") ~ "temperature_celsius",
        str_detect(name, "RH.*Avg") ~ "average_relative_humidity_percent",
        str_detect(name, "RH") ~ "relative_humidity_percent",
        str_detect(name, "Dew Point") ~ "dew_point_celsius",
        str_detect(name, "Date|Time") ~ "datetime"
      )
    ) |>
    distinct(observation_id, name, .keep_all = TRUE) |>
    pivot_wider() |>
    mutate(
      station_id = wze_id
    )
}

observations <-
  list.files("data/measurements", full.names = TRUE, pattern = "xlsx$") |>
  map(~ read_logger_excel(.x, stations)) |>
  bind_rows() |>
  type_convert()

# 

function(input, output, session) {
  clicked_station_id <- reactiveVal(value = "K1")
  
  observeEvent(
    eventExpr = c(input$map_plot_click$x, input$map_plot_click$y),
    handlerExpr = {
      click_point <- c(input$map_plot_click$x, input$map_plot_click$y)
      max_click_dist_degrees <- 0.03
      
      stations |>
        mutate(
          dist_to_click = lon |> map2_dbl(lat, ~ rbind(c(.x, .y), click_point) |> dist() |> as.numeric())
        ) |>
        #filter(dist_to_click < max_click_dist_degrees) |>
        arrange(dist_to_click) |>
        pull(station_id) |>
        first() |>
        clicked_station_id()
    }
  )
  
  stations_annotated <- reactive({
    stations |>
      mutate(is_clicked = station_id == clicked_station_id())
  })
  
  observations_filtered <- reactive({
    if(is.null(clicked_station_id())) {
      observations |>
        left_join(stations)
    } else {
      observations |>
        left_join(stations) |>
        filter(station_id == clicked_station_id())
    }
  })

  output$map_plot <- renderPlot({
    c(left = 11.525, bottom = 50.87, right = 11.67, top = 50.97) |>
      get_stamenmap(zoom = 13, maptype = "toner-lite") |>
      ggmap() +
      geom_point(data = stations_annotated(), mapping = aes(color = LRT, shape = is_clicked), size = 5) +
      scale_color_viridis_d()
  })

  output$heatmap_plot <- renderPlot({
    observations_filtered() |>
      left_join(stations) |>
      pivot_longer(c(temperature_celsius, relative_humidity_percent, dew_point_celsius)) |>
      group_by(name) |>
      mutate(value = scale(value)) |>
      ggplot(aes(datetime, name, color = value)) +
      geom_tile() +
      scale_color_viridis_c()
  })
  
  output$clicked_station_description <- renderTable({
    stations |>
      filter(station_id == clicked_station_id())
  })
}
