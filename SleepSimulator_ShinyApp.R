# Sleep Simulator Shiny App
# Three scenarios: Regular Sleep, Social Jetlag, and Progressive Delay

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)

# Suppress R CMD check warnings for dplyr/ggplot2 variables
utils::globalVariables(c("Date", "Hour", "SleepState", "DayOfWeek", "DayNumber", "Weekend", 
                          "Timestamp", "SleepHours", "AwakeHours", "FirstSleepStart"))

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Sleep Pattern Simulator"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Regular Sleep", tabName = "regular", icon = icon("bed")),
      menuItem("Social Jetlag", tabName = "jetlag", icon = icon("calendar-week")),
      menuItem("Progressive Delay", tabName = "progressive", icon = icon("clock"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Tab 1: Regular Sleep Pattern
      tabItem(tabName = "regular",
        fluidRow(
          box(
            title = "Regular Sleep Pattern Parameters", 
            status = "primary", 
            solidHeader = TRUE,
            width = 4,
            dateInput("reg_start_date", "Start Date:", value = "2020-04-01"),
            dateInput("reg_end_date", "End Date:", value = "2020-05-15"),
            numericInput("reg_sleep1_hours", "First Sleep Duration (hours):", value = 6, min = 1, max = 12),
            numericInput("reg_awake_hours", "Awake Duration (hours):", value = 16, min = 8, max = 20),
            numericInput("reg_sleep2_hours", "Second Sleep Duration (hours):", value = 2, min = 0, max = 6),
            actionButton("generate_regular", "Generate Pattern", class = "btn-primary"),
            br(), br(),
            radioButtons("reg_time_interval", "CSV Time Interval:",
                        choices = list("Minutes" = "min", "Seconds" = "sec"),
                        selected = "min", inline = TRUE),
            downloadButton("download_regular", "Download CSV", class = "btn-success")
          ),
          
          box(
            title = "Sleep Pattern Visualization",
            status = "success",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("regular_plot", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "Pattern Summary",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("regular_summary")
          )
        )
      ),
      
      # Tab 2: Social Jetlag (Weekend Delays)
      tabItem(tabName = "jetlag",
        fluidRow(
          box(
            title = "Social Jetlag Parameters",
            status = "warning",
            solidHeader = TRUE,
            width = 4,
            dateInput("sj_start_date", "Start Date:", value = "2020-04-01"),
            dateInput("sj_end_date", "End Date:", value = "2020-05-15"),
            numericInput("sj_sleep1_hours", "Weekday First Sleep (hours):", value = 6, min = 1, max = 12),
            numericInput("sj_awake_hours", "Weekday Awake Duration (hours):", value = 16, min = 8, max = 20),
            numericInput("sj_sleep2_hours", "Weekday Second Sleep (hours):", value = 2, min = 0, max = 6),
            numericInput("sj_weekend_delay", "Weekend Delay (hours):", value = 2, min = 0, max = 6),
            actionButton("generate_jetlag", "Generate Pattern", class = "btn-warning"),
            br(), br(),
            radioButtons("sj_time_interval", "CSV Time Interval:",
                        choices = list("Minutes" = "min", "Seconds" = "sec"),
                        selected = "min", inline = TRUE),
            downloadButton("download_jetlag", "Download CSV", class = "btn-success")
          ),
          
          box(
            title = "Social Jetlag Visualization",
            status = "success",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("jetlag_plot", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "Weekend vs Weekday Comparison",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("jetlag_summary")
          )
        )
      ),
      
      # Tab 3: Progressive Delay (Circadian Rhythm Study)
      tabItem(tabName = "progressive",
        fluidRow(
          box(
            title = "Progressive Delay Parameters",
            status = "danger",
            solidHeader = TRUE,
            width = 4,
            dateInput("pd_start_date", "Start Date:", value = "2020-04-01"),
            dateInput("pd_end_date", "End Date:", value = "2020-05-15"),
            numericInput("pd_sleep1_hours", "Initial First Sleep (hours):", value = 6, min = 1, max = 12),
            numericInput("pd_awake_hours", "Initial Awake Duration (hours):", value = 16, min = 8, max = 20),
            numericInput("pd_sleep2_hours", "Initial Second Sleep (hours):", value = 2, min = 0, max = 6),
            numericInput("pd_daily_delay", "Daily Delay (hours):", value = 0.5, min = 0, max = 4, step = 0.25),
            actionButton("generate_progressive", "Generate Pattern", class = "btn-danger"),
            br(), br(),
            radioButtons("pd_time_interval", "CSV Time Interval:",
                        choices = list("Minutes" = "min", "Seconds" = "sec"),
                        selected = "min", inline = TRUE),
            downloadButton("download_progressive", "Download CSV", class = "btn-success")
          ),
          
          box(
            title = "Progressive Delay Visualization",
            status = "success",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("progressive_plot", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "Daily Sleep Time Progression",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("progressive_summary")
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Reactive values to store generated data
  regular_data <- reactiveVal(NULL)
  jetlag_data <- reactiveVal(NULL)
  progressive_data <- reactiveVal(NULL)
  
  # Helper function to create time series
  create_timeseries <- function(start_date, end_date) {
    start <- as.POSIXct(paste(start_date, "00:00:00"), tz = "UTC")
    end <- as.POSIXct(paste(end_date, "23:59:59"), tz = "UTC")
    seq(from = start, to = end, by = "1 hour")  # Using hourly intervals for better performance
  }
  
  # Helper function to create detailed timeseries for CSV export
  create_detailed_timeseries <- function(start_date, end_date, interval = "min") {
    start <- as.POSIXct(paste(start_date, "00:00:00"), tz = "UTC")
    end <- as.POSIXct(paste(end_date, "23:59:59"), tz = "UTC")
    
    if (interval == "sec") {
      seq(from = start, to = end, by = "1 sec")
    } else {
      seq(from = start, to = end, by = "1 min")
    }
  }
  
  # Helper function to interpolate sleep state for detailed timeseries
  interpolate_sleep_state <- function(hourly_data, detailed_timestamps) {
    # Create a lookup table for sleep states by hour
    hourly_data$HourKey <- as.POSIXct(format(hourly_data$Timestamp, "%Y-%m-%d %H:00:00"), tz = "UTC")
    
    # For each detailed timestamp, find the corresponding hour and assign sleep state
    detailed_hours <- as.POSIXct(format(detailed_timestamps, "%Y-%m-%d %H:00:00"), tz = "UTC")
    
    # Match detailed timestamps to hourly data
    sleep_states <- hourly_data$SleepState[match(detailed_hours, hourly_data$HourKey)]
    
    # Handle any NA values by setting them to 0 (awake)
    sleep_states[is.na(sleep_states)] <- 0
    
    return(sleep_states)
  }
  
  # Helper function to create basic sleep pattern
  create_sleep_pattern <- function(sleep1_hours, awake_hours, sleep2_hours) {
    c(
      rep(1, sleep1_hours),   # First sleep
      rep(0, awake_hours),    # Awake
      rep(1, sleep2_hours)    # Second sleep
    )
  }
  
  # Regular Sleep Pattern Generation
  observeEvent(input$generate_regular, {
    timeseries <- create_timeseries(input$reg_start_date, input$reg_end_date)
    
    one_day_pattern <- create_sleep_pattern(
      input$reg_sleep1_hours,
      input$reg_awake_hours,
      input$reg_sleep2_hours
    )
    
    # Repeat pattern for all time points
    sleep_state <- rep(one_day_pattern, length.out = length(timeseries))
    
    df <- data.frame(
      Timestamp = timeseries,
      SleepState = sleep_state,
      Hour = as.numeric(format(timeseries, "%H")),
      Date = as.Date(timeseries),
      DayOfWeek = weekdays(timeseries)
    )
    
    regular_data(df)
  })
  
  # Social Jetlag Pattern Generation
  observeEvent(input$generate_jetlag, {
    timeseries <- create_timeseries(input$sj_start_date, input$sj_end_date)
    
    one_day_pattern <- create_sleep_pattern(
      input$sj_sleep1_hours,
      input$sj_awake_hours,
      input$sj_sleep2_hours
    )
    
    sleep_state <- rep(one_day_pattern, length.out = length(timeseries))
    
    df <- data.frame(
      Timestamp = timeseries,
      SleepState = sleep_state,
      Hour = as.numeric(format(timeseries, "%H")),
      Date = as.Date(timeseries),
      DayOfWeek = weekdays(timeseries)
    )
    
    # Apply weekend delays
    df$DayNum <- as.POSIXlt(df$Timestamp)$wday
    df$AdjustedSleepState <- df$SleepState
    
    # Weekend adjustments (simplified for hourly data)
    weekend_mask <- df$DayNum %in% c(0, 6)  # Sunday and Saturday
    
    # Shift weekend sleep pattern by delay amount
    weekend_hours <- which(weekend_mask)
    delay_hours <- input$sj_weekend_delay
    
    for(i in weekend_hours) {
      if(i + delay_hours <= nrow(df)) {
        if(df$SleepState[i] == 1) {  # If originally sleep
          df$AdjustedSleepState[i] <- 0  # Make awake
          if(i + delay_hours <= nrow(df)) {
            df$AdjustedSleepState[i + delay_hours] <- 1  # Delay sleep
          }
        }
      }
    }
    
    df$SleepState <- df$AdjustedSleepState
    df$AdjustedSleepState <- NULL
    df$DayNum <- NULL
    
    jetlag_data(df)
  })
  
  # Progressive Delay Pattern Generation
  observeEvent(input$generate_progressive, {
    timeseries <- create_timeseries(input$pd_start_date, input$pd_end_date)
    
    df <- data.frame(
      Timestamp = timeseries,
      SleepState = 0,  # Initialize as awake
      Hour = as.numeric(format(timeseries, "%H")),
      Date = as.Date(timeseries),
      DayOfWeek = weekdays(timeseries),
      DayNumber = as.numeric(as.Date(timeseries) - min(as.Date(timeseries))) + 1
    )
    
    # Calculate sleep times for each day with progressive delay
    daily_delay <- input$pd_daily_delay
    sleep1_duration <- input$pd_sleep1_hours
    sleep2_duration <- input$pd_sleep2_hours
    
    for(day in unique(df$DayNumber)) {
      day_mask <- df$DayNumber == day
      
      # Calculate delay for this day
      total_delay <- (day - 1) * daily_delay
      
      # Original sleep start times (0-based hours)
      sleep1_start <- 0 + total_delay
      sleep1_end <- sleep1_start + sleep1_duration
      sleep2_start <- sleep1_end + input$pd_awake_hours
      sleep2_end <- sleep2_start + sleep2_duration
      
      # Apply modulo 24 for hour wrapping
      sleep1_start_mod <- sleep1_start %% 24
      sleep1_end_mod <- sleep1_end %% 24
      sleep2_start_mod <- sleep2_start %% 24
      sleep2_end_mod <- sleep2_end %% 24
      
      # Set sleep states for this day
      for(i in which(day_mask)) {
        hour <- df$Hour[i]
        
        # Check if in first sleep period
        if(sleep1_start_mod < sleep1_end_mod) {
          if(hour >= sleep1_start_mod && hour < sleep1_end_mod) {
            df$SleepState[i] <- 1
          }
        } else {  # Sleep period wraps around midnight
          if(hour >= sleep1_start_mod || hour < sleep1_end_mod) {
            df$SleepState[i] <- 1
          }
        }
        
        # Check if in second sleep period
        if(sleep2_start_mod < sleep2_end_mod) {
          if(hour >= sleep2_start_mod && hour < sleep2_end_mod) {
            df$SleepState[i] <- 1
          }
        } else {  # Sleep period wraps around midnight
          if(hour >= sleep2_start_mod || hour < sleep2_end_mod) {
            df$SleepState[i] <- 1
          }
        }
      }
    }
    
    progressive_data(df)
  })
  
  # Regular Sleep Plot
  output$regular_plot <- renderPlotly({
    req(regular_data())
    
    df <- regular_data()
    
    # Create sample week for visualization
    sample_start <- min(df$Date) + 7  # Skip first week for better visualization
    sample_end <- sample_start + 6
    
    sample_data <- df %>%
      filter(Date >= sample_start, Date <= sample_end)
    
    p <- ggplot(sample_data, aes(x = Hour, y = Date, fill = factor(SleepState))) +
      geom_tile() +
      scale_fill_manual(values = c("0" = "lightblue", "1" = "darkblue"),
                       labels = c("Awake", "Sleep"),
                       name = "State") +
      scale_x_continuous(breaks = seq(0, 23, 4)) +
      labs(title = "Regular Sleep Pattern (Sample Week)",
           x = "Hour of Day",
           y = "Date") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Jetlag Plot
  output$jetlag_plot <- renderPlotly({
    req(jetlag_data())
    
    df <- jetlag_data()
    
    # Create sample week for visualization
    sample_start <- min(df$Date) + 7
    sample_end <- sample_start + 6
    
    sample_data <- df %>%
      filter(Date >= sample_start, Date <= sample_end) %>%
      mutate(Weekend = ifelse(DayOfWeek %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))
    
    p <- ggplot(sample_data, aes(x = Hour, y = Date, fill = factor(SleepState))) +
      geom_tile(aes(alpha = Weekend)) +
      scale_fill_manual(values = c("0" = "lightblue", "1" = "darkblue"),
                       labels = c("Awake", "Sleep"),
                       name = "State") +
      scale_alpha_manual(values = c("Weekday" = 0.7, "Weekend" = 1)) +
      scale_x_continuous(breaks = seq(0, 23, 4)) +
      labs(title = "Social Jetlag Pattern (Sample Week)",
           subtitle = "Weekend sleep patterns are shifted",
           x = "Hour of Day",
           y = "Date") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Progressive Delay Plot
  output$progressive_plot <- renderPlotly({
    req(progressive_data())
    
    df <- progressive_data()
    
    # Create sample period for visualization
    sample_data <- df %>%
      filter(DayNumber <= 14)  # First 14 days
    
    p <- ggplot(sample_data, aes(x = Hour, y = DayNumber, fill = factor(SleepState))) +
      geom_tile() +
      scale_fill_manual(values = c("0" = "lightblue", "1" = "darkblue"),
                       labels = c("Awake", "Sleep"),
                       name = "State") +
      scale_x_continuous(breaks = seq(0, 23, 4)) +
      scale_y_continuous(breaks = seq(1, 14, 2)) +
      labs(title = "Progressive Sleep Delay Pattern (First 14 Days)",
           subtitle = "Sleep timing shifts progressively each day",
           x = "Hour of Day",
           y = "Day Number") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Regular Sleep Summary
  output$regular_summary <- DT::renderDataTable({
    req(regular_data())
    
    df <- regular_data()
    
    summary_data <- df %>%
      group_by(Date, DayOfWeek) %>%
      summarise(
        SleepHours = sum(SleepState),
        AwakeHours = sum(1 - SleepState),
        .groups = 'drop'
      ) %>%
      head(10)
    
    DT::datatable(summary_data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Jetlag Summary
  output$jetlag_summary <- DT::renderDataTable({
    req(jetlag_data())
    
    df <- jetlag_data()
    
    summary_data <- df %>%
      mutate(Weekend = ifelse(DayOfWeek %in% c("Saturday", "Sunday"), "Weekend", "Weekday")) %>%
      group_by(Date, DayOfWeek, Weekend) %>%
      summarise(
        SleepHours = sum(SleepState),
        AwakeHours = sum(1 - SleepState),
        .groups = 'drop'
      ) %>%
      head(10)
    
    DT::datatable(summary_data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Progressive Summary
  output$progressive_summary <- DT::renderDataTable({
    req(progressive_data())
    
    df <- progressive_data()
    
    summary_data <- df %>%
      group_by(DayNumber, Date) %>%
      summarise(
        SleepHours = sum(SleepState),
        AwakeHours = sum(1 - SleepState),
        FirstSleepStart = ifelse(any(SleepState == 1), 
                                min(Hour[SleepState == 1]), NA),
        .groups = 'drop'
      ) %>%
      head(14)
    
    DT::datatable(summary_data, options = list(pageLength = 14, scrollX = TRUE))
  })
  
  # Download handlers for CSV export
  output$download_regular <- downloadHandler(
    filename = function() {
      interval_text <- if(input$reg_time_interval == "sec") "seconds" else "minutes"
      paste("regular_sleep_pattern_", interval_text, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(regular_data())
      hourly_df <- regular_data()
      
      # Create detailed timestamps based on selected interval
      detailed_timestamps <- create_detailed_timeseries(
        input$reg_start_date, 
        input$reg_end_date, 
        input$reg_time_interval
      )
      
      # Interpolate sleep states for detailed timestamps
      detailed_sleep_states <- interpolate_sleep_state(hourly_df, detailed_timestamps)
      
      # Create export dataframe
      export_data <- data.frame(
        Timestamp = detailed_timestamps,
        SleepState = detailed_sleep_states
      )
      
      write.csv(export_data, file, row.names = FALSE)
    }
  )
  
  output$download_jetlag <- downloadHandler(
    filename = function() {
      interval_text <- if(input$sj_time_interval == "sec") "seconds" else "minutes"
      paste("social_jetlag_pattern_", interval_text, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(jetlag_data())
      hourly_df <- jetlag_data()
      
      # Create detailed timestamps based on selected interval
      detailed_timestamps <- create_detailed_timeseries(
        input$sj_start_date, 
        input$sj_end_date, 
        input$sj_time_interval
      )
      
      # Interpolate sleep states for detailed timestamps
      detailed_sleep_states <- interpolate_sleep_state(hourly_df, detailed_timestamps)
      
      # Create export dataframe
      export_data <- data.frame(
        Timestamp = detailed_timestamps,
        SleepState = detailed_sleep_states
      )
      
      write.csv(export_data, file, row.names = FALSE)
    }
  )
  
  output$download_progressive <- downloadHandler(
    filename = function() {
      interval_text <- if(input$pd_time_interval == "sec") "seconds" else "minutes"
      paste("progressive_delay_pattern_", interval_text, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(progressive_data())
      hourly_df <- progressive_data()
      
      # Create detailed timestamps based on selected interval
      detailed_timestamps <- create_detailed_timeseries(
        input$pd_start_date, 
        input$pd_end_date, 
        input$pd_time_interval
      )
      
      # Interpolate sleep states for detailed timestamps
      detailed_sleep_states <- interpolate_sleep_state(hourly_df, detailed_timestamps)
      
      # Create export dataframe
      export_data <- data.frame(
        Timestamp = detailed_timestamps,
        SleepState = detailed_sleep_states
      )
      
      write.csv(export_data, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
