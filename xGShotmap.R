# R Shiny Application for Football Match Analysis
# Based on Understat data

library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(ggplot2)
library(ggsoccer)
library(patchwork)
library(ggtext)
library(ggrepel)
library(worldfootballR)

# ------------------------
# Utility Functions
# ------------------------

get_match_data_dynamic <- function(match_id, results) {
  match_row <- results %>% filter(match_id == !!match_id)
  
  tryCatch({
    shots <- understat_match_shots(paste0("https://understat.com/match/", match_id))
    
    if (!is.null(shots) && nrow(shots) > 0) {
      # Handle different column names for date
      match_date_col <- if("date" %in% names(match_row)) match_row$date
      else if("datetime" %in% names(match_row)) match_row$datetime
      else if("match_date" %in% names(match_row)) match_row$match_date
      else "Unknown date"
      
      match_info <- data.frame(
        match_id = match_id,
        home_team = match_row$home_team,
        away_team = match_row$away_team,
        home_goals = match_row$home_goals,
        away_goals = match_row$away_goals,
        match_date = match_date_col,
        forecast_win_home = if("forecast_win" %in% names(match_row)) match_row$forecast_win else NA,
        forecast_draw = if("forecast_draw" %in% names(match_row)) match_row$forecast_draw else NA,
        forecast_win_away = if("forecast_loss" %in% names(match_row)) match_row$forecast_loss else NA
      )
      return(list(shots = shots, match_info = match_info))
    } else {
      message("⚠️ Match ", match_id, " has no shot data.")
      return(NULL)
    }
  }, error = function(e) {
    message("Error retrieving data for match ", match_id, ": ", e$message)
    return(NULL)
  })
}

process_shots <- function(shots_data) {
  shots_data %>%
    mutate(
      x_pitch = as.numeric(X)*100,
      y_pitch = as.numeric(Y)*100,
      team_name = ifelse(home_away=="h",home_team,away_team),
      minute = as.numeric(minute),
      xG = as.numeric(xG),
      result_clean = case_when(
        grepl("Goal", result, ignore.case = TRUE) & !grepl("Own", result, ignore.case = TRUE) ~ "Goal",
        grepl("Own", result, ignore.case = TRUE) ~ "OwnGoal",
        grepl("Saved", result, ignore.case = TRUE) ~ "Saved",
        grepl("Block", result, ignore.case = TRUE) ~ "Blocked",
        grepl("Miss", result, ignore.case = TRUE) ~ "Missed",
        grepl("Post", result, ignore.case = TRUE) ~ "Post",
        TRUE ~ as.character(result)
      )
    )
}

calculate_cumulative_xg <- function(shots) {
  timeline_by_team <- shots %>%
    group_by(team_name) %>%
    arrange(minute) %>%
    mutate(xG_cumulative = cumsum(xG)) %>%
    select(minute, team_name, xG_cumulative, xG, result_clean, player)
  
  full_timeline <- expand.grid(
    minute = 0:90,
    team_name = unique(shots$team_name),
    stringsAsFactors = FALSE
  )
  
  timeline_complete <- full_timeline %>%
    left_join(timeline_by_team, by=c("minute","team_name")) %>%
    group_by(team_name) %>%
    arrange(minute) %>%
    fill(xG_cumulative, .direction="down") %>%
    mutate(xG_cumulative = replace_na(xG_cumulative, 0))
  
  return(timeline_complete)
}

get_team_colors <- function(home_team, away_team, team_names) {
  colors <- c("red", "blue")
  names(colors) <- c(home_team, away_team)
  return(colors)
}

create_xg_timeline_plot <- function(timeline, shots, team_colors) {
  goals <- shots %>% filter(result_clean %in% c("Goal","OwnGoal")) %>%
    left_join(
      timeline %>% group_by(minute,team_name) %>% slice(1) %>% select(minute,team_name,xG_cumulative),
      by=c("minute","team_name")
    ) %>%
    mutate(label=paste0(player," (",minute,"'): ",sprintf("%.2f xG",xG)))
  
  final_values <- timeline %>% filter(minute==90) %>% group_by(team_name) %>% slice(1)
  
  ggplot(timeline, aes(x=minute, y=xG_cumulative, color=team_name)) +
    geom_step(size=1.5, alpha=0.9) +
    geom_point(data=goals, aes(x=minute, y=xG_cumulative, color=team_name), 
               size=2, fill="white", shape=21, stroke=2) +
    ggrepel::geom_label_repel(data=goals, aes(x=minute, y=xG_cumulative, label=label),
                              nudge_y=0.5, size=3.5, min.segment.length=0.3, 
                              box.padding = 0.8, label.padding = 0.2, 
                              point.padding = 0.5, force = 2, max.overlaps = Inf,
                              direction = "both", seed = 123,
                              fill = "white", color = "black") +
    geom_text(data=final_values, aes(x=91, y=xG_cumulative, label=sprintf("%.2f",xG_cumulative)),
              hjust=0, size=5, fontface="bold") +
    scale_color_manual(values = team_colors) +
    scale_x_continuous(breaks=c(0,45,90), labels=c("0","HT","FT"), limits=c(0,94)) +
    scale_y_continuous(limits=c(0,max(timeline$xG_cumulative)+0.5), breaks=0:ceiling(max(timeline$xG_cumulative))) +
    labs(x="",y="Expected Goals") +
    theme_minimal(base_size=12) +
    theme(legend.position="none")
}

create_shot_map <- function(shots) {
  result_colors <- c("Goal"="orange","Saved"="green","Blocked"="grey","Missed"="black")
  
  shots_viz <- shots %>%
    mutate(
      x_viz = ifelse(team_name == unique(shots$home_team)[1], x_pitch, 100-x_pitch),
      y_viz = y_pitch
    )
  
  goals_only <- shots_viz %>% 
    filter(result_clean == "Goal") %>%
    mutate(label_text = paste0(player, " (", minute, "')"))
  
  ggplot(shots_viz) +
    annotate_pitch(colour="white", fill="darkgreen", limits=FALSE) +
    theme_pitch() +
    coord_flip(xlim=c(-5,105), ylim=c(-5,105)) +
    geom_point(aes(x=x_viz, y=y_viz, fill=result_clean),
               shape=21, color="black", size=4, stroke=0.8, alpha=0.9) +
    ggrepel::geom_text_repel(data=goals_only, 
                             aes(x=x_viz, y=y_viz, label=label_text),
                             color="white", size=3, fontface="bold",
                             bg.color="black", bg.r=0.1,
                             min.segment.length=0.2,
                             box.padding=0.5,
                             point.padding=0.3,
                             force=3,
                             max.overlaps=Inf,
                             seed=42) +
    scale_fill_manual(values=result_colors, name="") +
    facet_wrap(~team_name, ncol=2) +
    theme_void() +
    theme(
      strip.text = element_text(size=12, face="bold", margin=margin(b=10)),
      legend.position = "bottom",
      legend.text = element_text(size=10),
      panel.background = element_rect(fill="darkgreen", color=NA),
      plot.background = element_rect(fill="white", color=NA)
    )
}

create_header <- function(shots, match_info, league, season_start_year, team_colors) {
  home_team <- match_info$home_team
  away_team <- match_info$away_team
  home_goals <- match_info$home_goals
  away_goals <- match_info$away_goals
  match_date <- match_info$match_date
  
  # Format forecasts for display
  home_forecast <- if (!is.na(match_info$forecast_win_home)) {
    paste0(" (", round(as.numeric(match_info$forecast_win_home) * 100, 1), "%)")
  } else {
    ""
  }
  
  away_forecast <- if (!is.na(match_info$forecast_win_away)) {
    paste0(" (", round(as.numeric(match_info$forecast_win_away) * 100, 1), "%)")
  } else {
    ""
  }
  
  header_text <- paste0(
    "<span style='color:", team_colors[home_team], "; font-weight:bold; font-size:18px'>", home_team, home_forecast, "</span>",
    " <span style='font-size:16px'>", home_goals, " - ", away_goals, "</span> ",
    "<span style='color:", team_colors[away_team], "; font-weight:bold; font-size:18px'>", away_team, away_forecast, "</span><br>",
    "<span style='font-size:12px'>", league, " ", season_start_year, "/", season_start_year+1, " - ", match_date, "</span>"
  )
  
  ggplot() +
    annotate("richtext", x=0.5, y=0.5, label=header_text, hjust=0.5, vjust=0.5, size=6, fill=NA, label.color=NA) +
    xlim(0,1) + ylim(0,1) + theme_void()
}

create_footer <- function(shots, team_colors) {
  stats <- shots %>%
    group_by(team_name) %>%
    summarise(total_shots=n(), shots_on_target=sum(result_clean %in% c("Goal","Saved","OwnGoal")),
              total_xG=sum(xG), .groups="drop")
  
  stats_text <- paste0(
    "<span style='color:", team_colors[stats$team_name[1]], "; font-weight:bold'>", stats$team_name[1], "</span>", 
    " | Shots: ", stats$total_shots[1], " | On Target: ", stats$shots_on_target[1], " | xG per shot: ", round(stats$total_xG[1]/stats$total_shots[1],2), "<br>",
    "<span style='color:", team_colors[stats$team_name[2]], "; font-weight:bold'>", stats$team_name[2], "</span>", 
    " | Shots: ", stats$total_shots[2], " | On Target: ", stats$shots_on_target[2], " | xG per shot: ", round(stats$total_xG[2]/stats$total_shots[2],2)
  )
  
  ggplot() +
    annotate("richtext",x=0.5,y=0.5,label=stats_text, hjust=0.5, vjust=0.5, size=5, fill=NA, label.color=NA) +
    xlim(0,1) + ylim(0,1) + theme_void()
}

create_match_plot <- function(match_id, league, season_start_year, results) {
  match_data <- get_match_data_dynamic(match_id, results)
  if (is.null(match_data)) return(NULL)
  
  shots_processed <- process_shots(match_data$shots)
  timeline_data <- calculate_cumulative_xg(shots_processed)
  
  team_colors <- get_team_colors(match_data$match_info$home_team, match_data$match_info$away_team, unique(shots_processed$team_name))
  
  p_header <- create_header(shots_processed, match_data$match_info, league, season_start_year, team_colors)
  p_timeline <- create_xg_timeline_plot(timeline_data, shots_processed, team_colors)
  p_shotmap <- create_shot_map(shots_processed)
  p_footer <- create_footer(shots_processed, team_colors)
  
  final_plot <- p_header / p_timeline / p_shotmap / p_footer +
    plot_layout(heights = c(0.12, 0.35, 0.45, 0.08)) +
    plot_annotation(theme = theme(plot.background = element_rect(fill="white",color=NA)))
  
  return(final_plot)
}

# ------------------------
# User Interface (UI)
# ------------------------

ui <- dashboardPage(
  dashboardHeader(title = "xG and Shotmap"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Team Analysis", tabName = "analysis", icon = icon("futbol"))
    ),
    
    # Input controls
    div(style = "padding: 20px;",
        selectInput("league", 
                    "Select a league:",
                    choices = c("EPL" = "EPL", 
                                "Ligue 1" = "Ligue 1",
                                "Bundesliga" = "Bundesliga",
                                "Serie A" = "Serie A",
                                "La Liga" = "La liga"),
                    selected = "Ligue 1"),
        
        numericInput("season", 
                     "Season:",
                     value = 2025,
                     min = 2014,
                     max = 2025,
                     step = 1),
        
        selectInput("team_name", 
                    "Team Name:",
                    choices = NULL,
                    selected = NULL),
        
        checkboxInput("show_all_matches", 
                      "Show all matches", 
                      value = FALSE),
        
        conditionalPanel(
          condition = "!input.show_all_matches",
          numericInput("num_matches", 
                       "Number of matches to display:",
                       value = 10,
                       min = 1,
                       max = 50,
                       step = 1)
        ),
        
        actionButton("analyze", 
                     "Analyze", 
                     class = "btn-primary",
                     style = "width: 100%; margin-top: 10px;")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "analysis",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    title = "Match List",
                    DT::dataTableOutput("matches_table")
                )
              ),
              
              fluidRow(
                box(width = 12, status = "info", solidHeader = TRUE,
                    title = "Selected Match Analysis",
                    plotOutput("match_plot", height = "800px")
                )
              )
      )
    )
  )
)

# ------------------------
# Server Logic
# ------------------------

server <- function(input, output, session) {
  
  # Reactive data
  team_data <- reactiveValues(
    results = NULL,
    team_matches = NULL,
    selected_match = NULL
  )
  
  # Observer to load teams when league or season changes
  observeEvent(c(input$league, input$season), {
    tryCatch({
      # Get league data to obtain team list
      results_raw <- understat_league_match_results(
        league = input$league, 
        season_start_year = input$season
      )
      
      if (!is.null(results_raw) && nrow(results_raw) > 0) {
        # Extract unique team list
        all_teams <- unique(c(results_raw$home_team, results_raw$away_team))
        all_teams <- sort(all_teams)  # Sort alphabetically
        
        # Update team choices
        updateSelectInput(session, "team_name",
                          choices = all_teams,
                          selected = all_teams[1])  # Select first team by default
        
        # Store results to avoid reloading
        team_data$results <- results_raw
      }
    }, error = function(e) {
      showNotification(paste("Error loading teams:", e$message), type = "error")
    })
  }, ignoreInit = FALSE)
  
  # Observer for analysis
  observeEvent(input$analyze, {
    
    # Input validation
    if (is.null(input$team_name) || input$team_name == "") {
      showNotification("Please select a team", type = "error")
      return()
    }
    
    # Check if league data is already loaded
    if (is.null(team_data$results)) {
      showNotification("Loading league data...", type = "default", duration = NULL, id = "loading")
      
      tryCatch({
        # Retrieve league results
        results_raw <- understat_league_match_results(
          league = input$league, 
          season_start_year = input$season
        )
        
        # Check data structure and clean
        if (!is.null(results_raw) && nrow(results_raw) > 0) {
          team_data$results <- results_raw
        } else {
          stop("No data found for this league/season")
        }
      }, error = function(e) {
        removeNotification("loading")
        showNotification(paste("Error:", e$message), type = "error")
        return()
      })
    }
    
    # Filter team matches
    all_team_matches <- team_data$results %>% 
      filter(home_team == input$team_name | away_team == input$team_name) %>%
      # Handle different possible column names for date
      {if("date" %in% names(.)) arrange(., date) 
        else if("datetime" %in% names(.)) arrange(., datetime)
        else if("match_date" %in% names(.)) arrange(., match_date)
        else .} %>%
      mutate(
        match_label = paste(home_team, "vs", away_team),
        score = paste(home_goals, "-", away_goals),
        # Use first available date column
        date_display = if("date" %in% names(.)) date
        else if("datetime" %in% names(.)) datetime  
        else if("match_date" %in% names(.)) match_date
        else "Unknown date",
        result_display = paste(match_label, score, "(", date_display, ")")
      )
    
    # Limit number of matches based on user selection
    if (input$show_all_matches) {
      team_data$team_matches <- all_team_matches
      showNotification(
        paste("Displaying all", nrow(all_team_matches), "matches for", input$team_name, "this season"), 
        type = "default"
      )
    } else {
      team_data$team_matches <- all_team_matches %>% head(input$num_matches)
      showNotification(
        paste("Displaying", nrow(team_data$team_matches), "matches out of", nrow(all_team_matches), "available for", input$team_name), 
        type = "default"
      )
    }
    
    removeNotification("loading")
    
    if (nrow(team_data$team_matches) == 0) {
      showNotification(
        paste("No matches found for", input$team_name, "in", input$league, input$season), 
        type = "warning"
      )
    }
  })
  
  # Match table
  output$matches_table <- DT::renderDataTable({
    if (is.null(team_data$team_matches) || nrow(team_data$team_matches) == 0) {
      return(data.frame(Message = "No data available. Click 'Analyze' to load matches."))
    }
    
    display_table <- team_data$team_matches %>%
      select(
        Date = date_display,
        `Match` = match_label,
        Score = score,
        `Match ID` = match_id
      )
    
    DT::datatable(
      display_table,
      selection = "single",
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        scrollY = "400px",
        dom = 'lfrtip',
        lengthMenu = c(10, 15, 25, 50, -1),
        lengthChange = TRUE
      )
    )
  })
  
  # Observer for match selection
  observeEvent(input$matches_table_rows_selected, {
    if (!is.null(input$matches_table_rows_selected) && 
        !is.null(team_data$team_matches) && 
        nrow(team_data$team_matches) > 0) {
      
      selected_row <- input$matches_table_rows_selected
      team_data$selected_match <- team_data$team_matches$match_id[selected_row]
    }
  })
  
  # Match plot
  output$match_plot <- renderPlot({
    if (is.null(team_data$selected_match) || is.null(team_data$results)) {
      # Default plot
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Select a match from the table above\nto view detailed analysis", 
                 hjust = 0.5, vjust = 0.5, size = 6, color = "gray50") +
        xlim(0, 1) + ylim(0, 1) +
        theme_void()
    } else {
      # Generate plot for selected match
      create_match_plot(
        match_id = team_data$selected_match, 
        league = input$league, 
        season_start_year = input$season, 
        results = team_data$results
      )
    }
  })
}

# ------------------------
# Launch Application
# ------------------------

shinyApp(ui = ui, server = server)
