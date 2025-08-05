# Sleep Pattern Simulator Shiny App

This Shiny application provides an interactive interface to simulate and visualize three different sleep pattern scenarios. Each scenario is designed to study different aspects of circadian rhythms and sleep behavior using an interactive dashboard built with Shiny and shinydashboard.

## Features

The app includes three main tabs with real-time visualization and CSV export capabilities:

### 1. Regular Sleep Pattern
- **Purpose**: Simulate a basic, consistent sleep-wake cycle
- **Parameters**:
  - Start and end dates for the simulation period
  - First sleep duration (e.g., night sleep)
  - Awake duration 
  - Second sleep duration (e.g., nap)
- **Visualization**: Heat map showing sleep/wake states across time
- **Use Case**: Baseline sleep pattern analysis

### 2. Social Jetlag
- **Purpose**: Simulate weekend sleep delays (social jetlag effect)
- **Parameters**:
  - Same as regular pattern plus weekend delay hours
  - Weekend delay shifts sleep timing on Saturdays and Sundays
- **Visualization**: Heat map with different opacity for weekdays vs weekends
- **Use Case**: Study the effects of social jetlag on circadian rhythms

### 3. Progressive Delay
- **Purpose**: Simulate gradually shifting sleep times (circadian rhythm dyssynchrony)
- **Parameters**:
  - Initial sleep pattern parameters
  - Daily delay amount (hours to delay sleep each day)
- **Visualization**: Shows progressive shift of sleep timing over days
- **Use Case**: Forced desynchrony protocols, jet lag simulation

## Installation and Setup

### Prerequisites
- R (version 4.0 or higher recommended)
- RStudio (optional but recommended)

### Required R Packages
The app requires the following packages to be installed:
- `shiny` - Core Shiny framework
- `shinydashboard` - Dashboard layout and styling
- `dplyr` - Data manipulation
- `ggplot2` - Static plotting
- `plotly` - Interactive visualizations
- `DT` - Interactive data tables

Install them with:
```r
install.packages(c("shiny", "shinydashboard", "dplyr", "ggplot2", "plotly", "DT"))
```

### Running the App

#### Method 1: Direct execution in R/RStudio
```r
# Load and run the app:
source("SleepSimulator_ShinyApp.R")
```

#### Method 2: Command line
```bash
# From terminal/command prompt:
Rscript SleepSimulator_ShinyApp.R
```

## How to Use

1. **Launch the app** using one of the methods above
2. **Select a tab** corresponding to the sleep pattern you want to simulate:
   - **Regular Sleep** - Basic consistent sleep pattern
   - **Social Jetlag** - Weekend sleep delays  
   - **Progressive Delay** - Gradually shifting sleep times
3. **Adjust parameters** using the input controls on the left side:
   - Date ranges for simulation period
   - Sleep and awake durations
   - Specific delay parameters for jetlag/progressive patterns
4. **Click "Generate Pattern"** to create the simulation
5. **View visualizations** in the main panel:
   - Interactive heat maps showing sleep (dark blue) vs wake (light blue) states
   - Hover over the plot for detailed information
6. **Download data** as CSV files with customizable time intervals (minutes or seconds)
7. **Examine summary tables** below the plots for quantitative analysis

## Understanding the Visualizations

### Interactive Heat Maps
- **X-axis**: Hour of day (0-23)
- **Y-axis**: Date (Regular/Social Jetlag) or Day Number (Progressive Delay)
- **Colors**: 
  - Light blue = Awake (0)
  - Dark blue = Sleep (1)
- **Interactivity**: Built with plotly for hover tooltips, zoom, and pan capabilities
- **Sample Display**: Shows representative periods for better performance and readability

### Summary Tables
- **Interactive tables** powered by DT package with sorting and filtering
- **Sleep Hours**: Total sleep time per day/period
- **Awake Hours**: Total awake time per day/period
- **Additional metrics**: 
  - First Sleep Start time (Progressive Delay)
  - Weekend vs Weekday comparisons (Social Jetlag)

### CSV Export Feature
- **Flexible time intervals**: Export data in minute or second intervals
- **Interpolated data**: Hourly simulation data is interpolated to chosen resolution
- **Direct download**: Click download buttons to save CSV files with timestamps and sleep states

## Example Use Cases

### Research Applications
1. **Circadian Rhythm Studies**: Use progressive delay to simulate controlled laboratory conditions
2. **Social Jetlag Research**: Compare weekday vs weekend sleep patterns
3. **Sleep Intervention Planning**: Model different sleep schedules and their effects

### Educational Applications
1. **Teaching circadian biology concepts**
2. **Demonstrating the effects of irregular sleep schedules**
3. **Visualizing sleep pattern interventions**

## Technical Implementation

### User Interface
- **Dashboard Layout**: Built with shinydashboard for professional appearance
- **Tabbed Interface**: Three distinct tabs for different simulation types
- **Color-coded Design**: Each tab has unique color themes (blue, orange, red)
- **Responsive Controls**: Date inputs, numeric inputs, radio buttons, and action buttons

### Data Processing
- **Hourly Resolution**: Optimized for performance with hourly time intervals
- **Pattern Generation**: Mathematical algorithms for each sleep pattern type
- **Real-time Computation**: Patterns generated on-demand when parameters change
- **Data Interpolation**: CSV exports use interpolation for higher resolution output

### Visualization Technology
- **ggplot2**: Base plotting with professional styling
- **plotly**: Interactive features including hover, zoom, and pan
- **Conditional Rendering**: Plots only render when data is available
- **Sample Period Display**: Shows representative weeks/periods for large datasets

### Performance Considerations
- **Hourly intervals**: Used for visualization to maintain responsiveness
- **Sample visualization**: Large date ranges show representative periods
- **Lazy evaluation**: Data generated only when requested
- **Memory efficient**: Reactive values prevent unnecessary data storage

## Technical Notes

### Data Structure
- **Time Resolution**: Hourly intervals (optimized for performance)
- **Date Range**: Flexible, user-defined periods
- **Sleep States**: Binary (0 = awake, 1 = sleep)

### Performance Considerations
- Large date ranges may take longer to process
- Visualizations show sample periods for better readability
- Summary tables are limited to prevent overwhelming display

### Customization
The app can be extended by modifying the source code:
- Add new sleep pattern algorithms
- Include additional visualization types
- Extend parameter ranges or add new parameters

## Troubleshooting

### Common Issues
1. **App won't start**: 
   - Check that all required packages are installed
   - Verify R version compatibility (4.0+ recommended)
2. **Plots not showing**: 
   - Ensure you clicked "Generate Pattern" after setting parameters
   - Check that date ranges are reasonable (not too large)
3. **Slow performance**: 
   - Try shorter date ranges or smaller delay increments
   - Close other R sessions to free memory
4. **CSV download issues**:
   - Large date ranges with second intervals may take time to process
   - Consider using minute intervals for very long periods

### Error Messages
- **"object not found"**: Ensure all required packages are loaded
- **"Invalid date range"**: Start date must be before end date
- **"No data generated"**: Click the "Generate Pattern" button after setting parameters
- **Download errors**: Check available disk space and file permissions

## File Structure
```
SleepSimulator_ShinyApp.R    # Main Shiny application (complete implementation)
SleepSimulator.Rproj         # R project file
README.md                    # This documentation file
```

## Application Architecture

### UI Components
- **Dashboard header**: Application title
- **Sidebar menu**: Navigation between three pattern types
- **Main content**: Parameter controls, visualizations, and data tables
- **Download buttons**: CSV export functionality

### Server Logic
- **Reactive data storage**: Separate reactive values for each pattern type
- **Pattern generation**: Mathematical algorithms for sleep pattern creation
- **Visualization rendering**: plotly-based interactive charts
- **Data export**: CSV generation with user-selected time intervals

## Customization and Extension

The application can be extended by modifying the source code:

### Adding New Sleep Patterns
1. Create new UI tab in the `dashboardBody`
2. Add corresponding server logic for pattern generation
3. Implement visualization and summary table rendering
4. Add CSV export functionality

### Modifying Parameters
- Extend numeric input ranges in UI components
- Add new parameter types (sliders, checkboxes, etc.)
- Update server logic to handle new parameters

### Enhanced Visualizations
- Add new plot types using ggplot2/plotly
- Implement additional summary statistics
- Create custom color schemes or themes

## Credits

**Developer**: Dr. Ali Rahjouei  
**Research Group**: Circadian Medicine Group, Charité - Universitätsmedizin Berlin  
**Group Leader**: Prof. Dr. Luisa Klaus Pilz, PhD

Built with modern R Shiny framework and the following key components:
- **Shiny & shinydashboard**: Interactive web application framework
- **ggplot2 & plotly**: Professional data visualization
- **dplyr**: Efficient data manipulation
- **DT**: Interactive data tables

Sleep pattern algorithms implement:
1. Regular sleep pattern generation with customizable durations
2. Social jetlag simulation with weekend sleep delays
3. Progressive delay patterns for circadian rhythm research

## Version Information

- **R Version**: 4.0+ recommended
- **Shiny Version**: Compatible with current CRAN releases
- **Last Updated**: August 2025

## License

This software is provided as-is for research and educational purposes.
