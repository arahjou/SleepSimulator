# Sleep Simulator Shiny App
# Four scenarios: Regular Sleep, Social Jetlag, Progressive Delay, and Random Shift
#
# Developed by: Dr. Ali Rahjouei
# Circadian Medicine Lab | Working Group: Prof. Dr. Luísa Klaus Pilz
# Department of Anesthesiology and Intensive Care Medicine
# Charité - Universitätsmedizin Berlin
# Lab website: https://anaesthesieintensivmedizin.charite.de/forschung/arbeitsgruppen/circadiane_medizin

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)

# Suppress R CMD check warnings for dplyr/ggplot2 variables
utils::globalVariables(c("Date", "Hour", "SleepState", "DayOfWeek", "DayNumber", "Weekend", 
                          "Timestamp", "SleepHours", "AwakeHours", "FirstSleepStart", "DayType",
                          "RandomShift"))

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = "Sleep Pattern Simulator",
    titleWidth = 350,
  # Home button to jump back to Regular Sleep tab
  tags$li(class = "dropdown",
      actionLink("go_home", label = HTML('<i class="fa fa-home"></i> Home'),
             style = "padding: 15px 12px; font-size:16px; font-weight:600; color: #fff;")),
    tags$li(class = "dropdown",
            tags$a(href = "https://anaesthesieintensivmedizin.charite.de/forschung/arbeitsgruppen/circadiane_medizin",
                   target = "_blank",
                   HTML('<img src="https://1drv.ms/i/c/0aacee7d5fcf53f8/IQSgNBrdg_JESZJsEueGsDR1AWgsLp4opQ-9jPl05EzabM0?height=1024" width="auto" height="80" style="margin-top: 10px; margin-right: 10px;" />'),
                   title = "Circadian Medicine Lab - Charité")),
    tags$li(class = "dropdown",
            tags$span(style = "color: white; margin-top: 15px; display: inline-block; font-size: 12px;",
                     "Circadian Medicine Lab | Prof. Dr. Luísa Klaus Pilz"))
  ),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      selected = "regular",  # Ensure default selected tab is explicitly set
      menuItem("Regular Sleep", tabName = "regular", icon = icon("bed")),
      menuItem("Social Jetlag", tabName = "jetlag", icon = icon("calendar-week")),
      menuItem("Progressive Delay", tabName = "progressive", icon = icon("clock")),
      menuItem("Random Shift", tabName = "random", icon = icon("random")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    # CSS adjustments: ensure sidebar clickable and style for home link & optional floating button
    tags$head(tags$style(HTML('
      .main-sidebar { z-index: 900 !important; }
      #go_home { color: #fff !important; }
      #go_home:hover { background: rgba(255,255,255,0.15) !important; text-decoration: none; }
      .fab-home { position: fixed; bottom: 25px; right: 25px; z-index: 1001; }
    '))),

    # Optional floating home button (uncomment if desired in addition to header)
    # tags$div(class = 'fab-home', actionButton('fab_home', label = HTML('<i class="fa fa-home"></i>')), style = 'background: #3c8dbc; border-radius: 50%; padding:10px;'),
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
      ),
      
      # Tab 4: Random Shift Pattern
      tabItem(tabName = "random",
        fluidRow(
          box(
            title = "Random Shift Parameters",
            status = "info",
            solidHeader = TRUE,
            width = 4,
            dateInput("rs_start_date", "Start Date:", value = "2020-04-01"),
            dateInput("rs_end_date", "End Date:", value = "2020-05-15"),
            numericInput("rs_sleep1_hours", "First Sleep Duration (hours):", value = 6, min = 1, max = 12),
            numericInput("rs_awake_hours", "Awake Duration (hours):", value = 16, min = 8, max = 20),
            numericInput("rs_sleep2_hours", "Second Sleep Duration (hours):", value = 2, min = 0, max = 6),
            numericInput("rs_min_shift", "Minimum Shift (hours):", value = 0, min = -6, max = 6, step = 0.25),
            numericInput("rs_max_shift", "Maximum Shift (hours):", value = 3, min = -6, max = 6, step = 0.25),
            numericInput("rs_seed", "Random Seed (for reproducibility):", value = 123, min = 1, max = 10000),
            actionButton("generate_random", "Generate Pattern", class = "btn-info"),
            br(), br(),
            radioButtons("rs_time_interval", "CSV Time Interval:",
                        choices = list("Minutes" = "min", "Seconds" = "sec"),
                        selected = "min", inline = TRUE),
            downloadButton("download_random", "Download CSV", class = "btn-success")
          ),
          
          box(
            title = "Random Shift Visualization",
            status = "success",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("random_plot", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "Daily Random Shifts Summary",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("random_summary")
          )
        )
      ),
      
      # Tab 5: About
      tabItem(tabName = "about",
        fluidRow(
          box(
            title = "About the Sleep Pattern Simulator",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            div(style = "text-align: center; margin-bottom: 20px;",
                HTML('<img src="https://1drv.ms/i/c/0aacee7d5fcf53f8/IQSgNBrdg_JESZJsEueGsDR1AWgsLp4opQ-9jPl05EzabM0?height=1024" width="auto" height="160" style="margin-bottom: 15px;" />')),
            
            h4("Developer Information", style = "color: #337ab7; margin-bottom: 15px;"),
            p(strong("Dr. Ali Rahjouei"), style = "font-size: 16px; margin-bottom: 10px;"),
            p("Circadian Medicine Lab", style = "margin-bottom: 5px;"),
            p("Department of Anesthesiology and Intensive Care Medicine", style = "margin-bottom: 5px;"),
            p("Charité - Universitätsmedizin Berlin", style = "margin-bottom: 15px;"),
            
            h4("Principal Investigator", style = "color: #337ab7; margin-bottom: 15px;"),
            p(strong("Prof. Dr. Luísa Klaus Pilz"), style = "font-size: 16px; margin-bottom: 10px;"),
            p("Working Group Leader - Circadian Medicine", style = "margin-bottom: 15px;"),
            
            div(style = "text-align: center;",
                tags$a(href = "https://anaesthesieintensivmedizin.charite.de/forschung/arbeitsgruppen/circadiane_medizin",
                       target = "_blank",
                       class = "btn btn-primary",
                       icon("external-link-alt"), " Visit Lab Website"))
          ),
          
          box(
            title = "Application Overview",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            h4("Sleep Pattern Simulation Tool", style = "color: #449d44; margin-bottom: 15px;"),
            p("This application simulates various sleep patterns commonly studied in circadian medicine research. 
              It provides researchers and clinicians with tools to model different sleep scenarios and export data 
              for further analysis."),
            
            h5("Available Simulation Types:", style = "margin-top: 20px; margin-bottom: 10px;"),
            tags$ul(
              tags$li(strong("Regular Sleep:"), " Consistent daily sleep patterns"),
              tags$li(strong("Social Jetlag:"), " Weekend sleep delays with weekday constraints"),
              tags$li(strong("Progressive Delay:"), " Gradual daily shifts in sleep timing"),
              tags$li(strong("Random Shift:"), " Variable daily shifts within specified ranges")
            ),
            
            h5("Features:", style = "margin-top: 20px; margin-bottom: 10px;"),
            tags$ul(
              tags$li("Interactive parameter adjustment"),
              tags$li("Real-time visualization"),
              tags$li("Detailed summary statistics"),
              tags$li("CSV export for data analysis"),
              tags$li("Minute or second resolution export")
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Research Applications",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            h4("Circadian Medicine Research", style = "color: #ec971f; margin-bottom: 15px;"),
            p("The Circadian Medicine Lab focuses on understanding how biological rhythms affect health and disease. 
              This sleep pattern simulator supports research in:"),
            
            div(style = "display: flex; flex-wrap: wrap; justify-content: space-around; margin-top: 20px;",
                div(style = "flex: 1; min-width: 250px; margin: 10px; padding: 15px; background-color: #f9f9f9; border-radius: 5px;",
                    h5("Sleep Disorders", style = "color: #337ab7;"),
                    p("Modeling irregular sleep patterns and their impact on circadian rhythms")),
                div(style = "flex: 1; min-width: 250px; margin: 10px; padding: 15px; background-color: #f9f9f9; border-radius: 5px;",
                    h5("Shift Work Studies", style = "color: #337ab7;"),
                    p("Simulating the effects of rotating schedules and night work")),
                div(style = "flex: 1; min-width: 250px; margin: 10px; padding: 15px; background-color: #f9f9f9; border-radius: 5px;",
                    h5("Chronotherapy", style = "color: #337ab7;"),
                    p("Optimizing treatment timing based on circadian principles")),
                div(style = "flex: 1; min-width: 250px; margin: 10px; padding: 15px; background-color: #f9f9f9; border-radius: 5px;",
                    h5("Social Jetlag", style = "color: #337ab7;"),
                    p("Investigating misalignment between social and biological time"))
            ),
            
            br(),
            p(style = "text-align: center; margin-top: 20px;",
              "For more information about our research, please visit the ",
              tags$a(href = "https://anaesthesieintensivmedizin.charite.de/forschung/arbeitsgruppen/circadiane_medizin",
                     target = "_blank",
                     "Circadian Medicine Lab website"))
          )
        )
      )
    ),
    
    # Footer with attribution
    tags$footer(
      style = "background-color: #f4f4f4; padding: 15px; margin-top: 20px; border-top: 1px solid #ddd;",
      fluidRow(
        column(12,
          div(style = "text-align: center;",
            h5("Sleep Pattern Simulator", style = "margin-bottom: 10px; color: #333;"),
            p(style = "margin-bottom: 5px; color: #666;",
              "Developed by ", 
              strong("Dr. Ali Rahjouei"), 
              " at the Circadian Medicine Lab"),
            p(style = "margin-bottom: 5px; color: #666;",
              "Working Group: ", 
              strong("Prof. Dr. Luísa Klaus Pilz")),
            p(style = "margin-bottom: 5px;",
              tags$a(href = "https://anaesthesieintensivmedizin.charite.de/forschung/arbeitsgruppen/circadiane_medizin",
                     target = "_blank",
                     style = "color: #337ab7; text-decoration: none;",
                     "Circadian Medicine Lab - Charité Berlin")),
            p(style = "margin-bottom: 0; color: #999; font-size: 12px;",
              "Department of Anesthesiology and Intensive Care Medicine | Charité - Universitätsmedizin Berlin")
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {

  # Debug: log tab changes in server console
  observeEvent(input$tabs, {
    cat("[DEBUG] Active tab:", input$tabs, "\n")
  })

  # Home button (header) -> navigate to Regular Sleep tab
  observeEvent(input$go_home, {
    updateTabItems(session, "tabs", "regular")
  })

  # Floating action button (if uncommented in UI)
  observeEvent(input$fab_home, {
    updateTabItems(session, "tabs", "regular")
  }, ignoreNULL = TRUE)
  
  # Reactive values to store generated data
  regular_data <- reactiveVal(NULL)
  jetlag_data <- reactiveVal(NULL)
  progressive_data <- reactiveVal(NULL)
  random_data <- reactiveVal(NULL)
  
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
    
    df <- data.frame(
      Timestamp = timeseries,
      SleepState = 0,  # Initialize as awake
      Hour = as.numeric(format(timeseries, "%H")),
      Date = as.Date(timeseries),
      DayOfWeek = weekdays(timeseries),
      DayNumber = as.numeric(as.Date(timeseries) - min(as.Date(timeseries))) + 1
    )
    
    # Add day type indicator
    df$DayNum <- as.POSIXlt(df$Timestamp)$wday
    df$IsWeekend <- df$DayNum %in% c(0, 6)  # Sunday and Saturday
    
    # Create sleep patterns for each day
    sleep1_duration <- input$sj_sleep1_hours
    awake_duration <- input$sj_awake_hours
    sleep2_duration <- input$sj_sleep2_hours
    weekend_delay <- input$sj_weekend_delay
    
    for(day in unique(df$DayNumber)) {
      day_mask <- df$DayNumber == day
      is_weekend_day <- any(df$IsWeekend[day_mask])
      is_friday <- any(df$DayOfWeek[day_mask] == "Friday")
      
      # Calculate sleep timing based on day type
      if (is_weekend_day) {
        # Weekend: apply delay to sleep start times
        sleep1_start <- 0 + weekend_delay
        sleep1_end <- sleep1_start + sleep1_duration
        sleep2_start <- sleep1_end + awake_duration
        sleep2_end <- sleep2_start + sleep2_duration
      } else if (is_friday) {
        # Friday: regular pattern but skip second sleep episode (stay awake)
        sleep1_start <- 0
        sleep1_end <- sleep1_start + sleep1_duration
        sleep2_start <- -1  # No second sleep
        sleep2_end <- -1    # No second sleep
      } else {
        # Regular weekday: normal pattern
        sleep1_start <- 0
        sleep1_end <- sleep1_start + sleep1_duration
        sleep2_start <- sleep1_end + awake_duration
        sleep2_end <- sleep2_start + sleep2_duration
      }
      
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
        
        # Check if in second sleep period (skip if Friday - no second sleep)
        if(sleep2_start >= 0 && sleep2_end >= 0) {
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
    }
    
    # Clean up temporary columns
    df$DayNumber <- NULL
    df$DayNum <- NULL
    df$IsWeekend <- NULL
    
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
  
  # Random Shift Pattern Generation
  observeEvent(input$generate_random, {
    timeseries <- create_timeseries(input$rs_start_date, input$rs_end_date)
    
    df <- data.frame(
      Timestamp = timeseries,
      SleepState = 0,  # Initialize as awake
      Hour = as.numeric(format(timeseries, "%H")),
      Date = as.Date(timeseries),
      DayOfWeek = weekdays(timeseries),
      DayNumber = as.numeric(as.Date(timeseries) - min(as.Date(timeseries))) + 1
    )
    
    # Set seed for reproducibility
    set.seed(input$rs_seed)
    
    # Get parameters
    sleep1_duration <- input$rs_sleep1_hours
    awake_duration <- input$rs_awake_hours
    sleep2_duration <- input$rs_sleep2_hours
    min_shift <- input$rs_min_shift
    max_shift <- input$rs_max_shift
    
    # Generate random shifts for each day
    unique_days <- unique(df$DayNumber)
    random_shifts <- runif(length(unique_days), min = min_shift, max = max_shift)
    
    # Create lookup table for random shifts by day
    shift_lookup <- data.frame(
      DayNumber = unique_days,
      RandomShift = random_shifts
    )
    
    for(day in unique_days) {
      day_mask <- df$DayNumber == day
      daily_shift <- shift_lookup$RandomShift[shift_lookup$DayNumber == day]
      
      # Calculate sleep timing with random shift
      sleep1_start <- 0 + daily_shift
      sleep1_end <- sleep1_start + sleep1_duration
      sleep2_start <- sleep1_end + awake_duration
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
    
    # Add shift information to the dataframe
    df <- merge(df, shift_lookup, by = "DayNumber", all.x = TRUE)
    
    random_data(df)
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
      mutate(DayType = case_when(
        DayOfWeek == "Friday" ~ "Friday",
        DayOfWeek %in% c("Saturday", "Sunday") ~ "Weekend",
        TRUE ~ "Weekday"
      )) %>%
      group_by(Date, DayOfWeek, DayType) %>%
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
  
  # Random Shift Plot
  output$random_plot <- renderPlotly({
    req(random_data())
    
    df <- random_data()
    
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
      labs(title = "Random Shift Sleep Pattern (First 14 Days)",
           subtitle = "Sleep timing shifts randomly each day within specified range",
           x = "Hour of Day",
           y = "Day Number") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Random Summary
  output$random_summary <- DT::renderDataTable({
    req(random_data())
    
    df <- random_data()
    
    summary_data <- df %>%
      group_by(DayNumber, Date) %>%
      summarise(
        RandomShift = first(RandomShift),
        SleepHours = sum(SleepState),
        AwakeHours = sum(1 - SleepState),
        FirstSleepStart = ifelse(any(SleepState == 1), 
                                min(Hour[SleepState == 1]), NA),
        .groups = 'drop'
      ) %>%
      mutate(RandomShift = round(RandomShift, 2)) %>%
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
  
  output$download_random <- downloadHandler(
    filename = function() {
      interval_text <- if(input$rs_time_interval == "sec") "seconds" else "minutes"
      paste("random_shift_pattern_", interval_text, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(random_data())
      hourly_df <- random_data()
      
      # Create detailed timestamps based on selected interval
      detailed_timestamps <- create_detailed_timeseries(
        input$rs_start_date, 
        input$rs_end_date, 
        input$rs_time_interval
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
