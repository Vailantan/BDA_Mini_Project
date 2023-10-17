# Install and load required libraries if not already done

library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library(fmsb)
library(png)
createRadarChart <- function(top_df_1,top_df,i) {
  max_values <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  min_values <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  df <- rbind(max_values, min_values, top_df_1[i,])
  radarchart(df, axistype = 1, 
      pcol = rgb(0.2, 0.5, 0.5, 0.9), pfcol = rgb(0.2, 0.5, 0.5, 0.5), plwd = 4, 
      cglcol = "grey", cglty = 1, axislabcol = "grey")
  chart_title <- top_df[i,][2]
  title(chart_title)

}
normalize_data <- function(file_path) {
  df <- read.csv(file_path)
  df <- df[df$time > 0, ]
  
  per90Cols <- c("goals", "assists", "key_passes", "time", "xG", "xA", "shots", "npxG", "xGChain", "xGBuildup")
  
  for (col in per90Cols) {
    df[paste0(col, "Per90")] <- (df[, col] / df$time) * 90
  }
  
  cols_for_radar <- paste0(per90Cols, "Per90")
  cols_for_radar1 <- character(0)
  
  for (i in per90Cols) {
    cols_for_radar1 <- c(cols_for_radar1, i)
  }
  
  cols_for_radar <- paste0(per90Cols, "Per90")
  cols_for_radar1 <- character(0)
  
  for (i in per90Cols) {
    cols_for_radar1 <- c(cols_for_radar1, i)
  }
  
  df  <- df  %>%
    mutate(across(all_of(cols_for_radar1), ~ (.-min(.)) / (max(.) - min(.))))
  my_list <- list(value1 = df, value2 = cols_for_radar1)
  return(my_list)
}

createRadarChartCompare <- function(top_df_1,top_df,top_player,i) {
  max_values <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  min_values <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  data <- rbind(max_values, min_values, top_df_1[top_player,], top_df_1[i,])
  colors <- c(rgb(0.2, 0.5, 0.5, 0.4), rgb(0.8, 0.2, 0.5, 0.4))
  colors_border <- c(rgb(0.2, 0.5, 0.5, 0.9), rgb(0.8, 0.2, 0.5, 0.9))
  radarchart(data, pfcol = colors, plwd = 4, plty = 1, pcol = colors_border,
             cglcol = "grey", cglty = 1, axislabcol = "grey", axistype = 1)
  
  legend("topright", legend = c(top_df[top_player,][2],top_df[i,][2]), pch = 20, col = colors_border,
         pt.cex = 2, cex = 0.8, text.col = "black")
}

createHeatMap <- function(shots_df,heat_number){
  
  player_id_list <- shots_df[["id"]]
  player_name_lists <- shots_df[["player_name"]]
  csv_paths <- list()
  for (player_id in player_id_list) {
    csv_paths <- c(csv_paths, sprintf("player%d.csv", player_id))
  }
  df_shots <- list()
  for (csv_path in csv_paths) {
    df <- read.csv(csv_path)
    df_shots <- c(df_shots, list(df))
  }
  count <-1
  background_img <- readPNG("soccer_pitch.png")
  
  p_df <- df_shots[[heat_number]]
  
  x_min <- min(p_df$X)
  x_max <- max(p_df$X)
  y_min <- min(p_df$Y)
  y_max <- max(p_df$Y)
  gg <- ggplot(p_df, aes(x = X, y = Y)) +
    annotation_raster(background_img, xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max, interpolate = TRUE) +
    geom_density_2d_filled(alpha = 0.5) +
    xlim(x_min, x_max) +
    ylim(y_min, y_max) +
    ggtitle(player_name_lists[[heat_number]]) +
    theme_void()
  
  print(gg)
  
}

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("Soccer Player Analysis"),
  tabsetPanel(
    tabPanel("Radar Plot", 
             numericInput("numeric_input", "Enter the number of players to analyze:", value = 5),
             uiOutput("radarPlotUI")
    ),
    tabPanel("Radar Plot Comparison", 
             numericInput("top_player", "Enter topmost player to be compared:", value = 1),
             numericInput("comp_number", "Enter the number of players to be compared with:", value = 5),
             uiOutput("radarComparisonUI")
    ),
 
      tabPanel("Heat Map", 
               numericInput("heat_number", "Enter the number of players for the Heat Map:", value = 1),
               uiOutput("heatMapUI")
      )
)
  )


# Define server logic
server <- function(input, output) {
  normalized_data <- normalize_data("La_liga-2017.csv")
  
  output$radarPlotUI <- renderUI({
    plot_output_list <- lapply(1:input$numeric_input, function(i) {
      plotOutput(paste0("radarPlot", i))
    })
    
    do.call(tagList, plot_output_list)
  })
  
  observe({
    lapply(1:input$numeric_input, function(i) {
      output[[paste0("radarPlot", i)]] <- renderPlot({
        numeric_input <- isolate(input$numeric_input)
        if (!is.na(numeric_input)) {
          top_df <- head(normalized_data$value1, numeric_input)
          top_df_1 <- top_df[normalized_data$value2]
          createRadarChart(top_df_1, top_df, i)
        }
      })
    })
  })
  
  output$radarComparisonUI <- renderUI({
    plot_output_list <- lapply(1:input$comp_number, function(i) {
      plotOutput(paste0("radarComparison", i))
    })
    
    do.call(tagList, plot_output_list)
  })
  
  observe({
    lapply(1:input$comp_number, function(i) {
      output[[paste0("radarComparison", i)]] <- renderPlot({
        top_player <- isolate(input$top_player)
        comp_number <- isolate(input$comp_number)
        if (!is.na(top_player) && !is.na(comp_number)) {
          top_df <- head(normalized_data$value1, comp_number)
          top_df_1 <- top_df[normalized_data$value2]
          if (top_player != i) {
            createRadarChartCompare(top_df_1, top_df, top_player, i)
          }
          else
          {
            i <-i +1
            createRadarChartCompare(top_df_1, top_df, top_player, i)
          }
    
        }
      })
    })
  })
  
  
  output$heatMapUI <- renderUI({
    plot_output_list <- lapply(1:input$heat_number, function(i) {
      plotOutput(paste0("heatMap", i))
    })
    
    do.call(tagList, plot_output_list)
  })
  
  observe({
    lapply(1:input$heat_number, function(i) {
      output[[paste0("heatMap", i)]] <- renderPlot({
        heat_number <- isolate(input$heat_number)
        if (!is.na(heat_number)) {
          shots_df <- head(normalized_data$value1,10)
          createHeatMap(shots_df,i)
        }
      })
    })
  })
  
}




# Run the Shiny app
shinyApp(ui, server)
