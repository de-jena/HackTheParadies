library(shiny)

fluidPage(
    titlePanel("Mikroklima im Jenaer Wald"),
    fluidRow(
        tableOutput("clicked_station_description"),
        plotOutput("map_plot", click = "map_plot_click"),
        plotOutput("heatmap_plot")
    )
)
