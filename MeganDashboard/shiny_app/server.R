server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    leaflet(data = df_sample) |>
      addTiles() |>
      addCircleMarkers(
        radius = 5,
        stroke = FALSE,
        fillOpacity = 0.7,
        popup = ~paste(
          "<b>Country:</b>", name_en, "<br>",
          "<b>Voice & Accountability:</b>", round(Voice_account.sc, 2)
        )
      )
  })
}
