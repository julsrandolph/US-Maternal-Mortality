library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

# Define the UI
ui <- fluidPage(
  titlePanel("US Maternal Death"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year(s) of Death:", choices = NULL, multiple = TRUE),
      selectInput("group", "Select Group:", choices = NULL),
      selectInput("subgroup", "Select Subgroup:", choices = NULL, multiple = TRUE),
      checkboxInput("showData", "Show Data", value = FALSE)
    ),
    mainPanel(
      plotlyOutput("linePlot"),
      plotlyOutput("pieChart"),
      uiOutput("dataTableUI")
    )
  )
)

# Define the server
server <- function(input, output, session) {
  # Load the data from the URL
  data <- reactive({
    url <- "https://data.cdc.gov/api/views/e2d5-ggg7/rows.csv"
    read.csv(url)
  })
  
  # Populate Year dropdown with unique values from "Year.of.Death"
  observe({
    if (nrow(data()) > 0) {
      years <- unique(data()[["Year.of.Death"]])
      max_year <- max(years, na.rm = TRUE)  # Determine the most recent year
      updateSelectInput(
        session,
        "year",
        choices = sort(years, decreasing = TRUE),
        selected = max_year  # Default to the most recent year
      )
    }
  })
  
  # Populate Group dropdown with unique values from "Group" and default to "Total"
  observe({
    if (nrow(data()) > 0) {
      groups <- unique(data()[["Group"]])
      groups <- c("Total", setdiff(sort(groups), "Total"))  # Ensure "Total" is first
      updateSelectInput(session, "group", choices = groups, selected = "Total")
    }
  })
  
  # Initially set the Subgroup to "Total" on page load
  observe({
    if (nrow(data()) > 0) {
      updateSelectInput(session, "subgroup", choices = c("Total"))
    }
  })
  
  # Update Subgroup dropdown based on selected Group
  observeEvent(input$group, {
    if (!is.null(input$group) && nrow(data()) > 0) {
      # Get unique subgroups for the selected group
      subgroups <- unique(data()[data()[["Group"]] == input$group, "Subgroup"])
      
      # Update the Subgroup dropdown to the available options
      updateSelectInput(session, "subgroup", choices = subgroups, selected = "Total")
    }
  })
  
  # Filter data based on selected Year(s), Group, and Subgroup
  filteredData <- reactive({
    req(input$year, input$group, input$subgroup, nrow(data()) > 0)
    
    # Ensure that the selected subgroups are properly filtered if multiple are selected
    subset(data(),
           Year.of.Death %in% input$year &
             Group == input$group &
             Subgroup %in% input$subgroup)
  })
  
  # Render the line plot
  output$linePlot <- renderPlotly({
    req(filteredData())  # Ensure filtered data is available
    
    # Create the Timestamp column and tooltip content
    plot_data <- filteredData() %>%
      mutate(
        Month = factor(format(as.Date(paste(Year.of.Death, Month.of.Death, "01", sep = "-"), format = "%Y-%m-%d"), "%b"), levels = month.abb),
        YearMonth = as.Date(paste(Year.of.Death, Month.of.Death, "01", sep = "-"), format = "%Y-%m-%d"),
        Combination = paste(Subgroup, Year.of.Death, sep = " - "),  # Only Subgroup and Year in legend
        TooltipText = paste(
          "Date: ", Month, ",", Year.of.Death, "<br>",
          "Group: ", Group, "<br>",
          "Subgroup: ", Subgroup, "<br>",
          "Live Births: ", Live.Births, "<br>",
          "Maternal Deaths: ", Maternal.Deaths, "<br>",
          "Mortality Rate: ", round(Maternal.Mortality.Rate, 2)
        )  # Custom tooltip text
      )
    
    # Check if the data is empty after filtering
    if (nrow(plot_data) == 0) {
      return(NULL)
    }
    
    # Define a custom palette of darker, high-contrast colors
    custom_palette <- c(
      "#e7298a", "#1b9e77", "#d95f02", "#7570b3", "#66a61e", 
      "#e6ab02", "#a6761d", "#666666", "#444444", "#2c7bb6"
    )
    
    # Use only as many colors as needed
    palette_to_use <- custom_palette[1:length(unique(plot_data$Combination))]
    
    # Calculate dynamic offset for text labels
    y_offset <- (max(plot_data$Maternal.Deaths, na.rm = TRUE) - min(plot_data$Maternal.Deaths, na.rm = TRUE)) * 0.05
    
    # Create the line plot with ggplot2
    p <- ggplot(plot_data, aes(x = Month, y = Maternal.Deaths, group = Combination, color = Combination)) +
      geom_line(size = 0.5) +  # Thicker lines for better visibility
      geom_point(aes(text = TooltipText), size = 2) +  # Use TooltipText for hover
      geom_text(
        aes(label = round(Maternal.Mortality.Rate, 2)), 
        nudge_y = y_offset,  # Dynamically offset text above the points
        size = 3,
        check_overlap = TRUE  # Avoid overlapping text
      ) +
      labs(x = "Month", y = "Maternal Deaths", title = "Maternal Deaths (12 Month Ending Counts)") +
      scale_color_manual(values = palette_to_use) +  # Use the custom color palette
      theme_minimal() +
      theme(
        legend.title = element_blank(),  # Remove legend title
        axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
      )
    
    # Convert ggplot to plotly with custom tooltip
    ggplotly(p, tooltip = "text")
  })
  
  # Render the pie chart
  output$pieChart <- renderPlotly({
    req(filteredData())  # Ensure filtered data is available
    
    # Identify the most recent month in the dataset
    most_recent <- filteredData() %>%
      summarise(
        RecentYear = max(Year.of.Death, na.rm = TRUE),  # Most recent year
        RecentMonth = max(Month.of.Death[Year.of.Death == max(Year.of.Death, na.rm = TRUE)], na.rm = TRUE)  # Most recent month in the most recent year
      )
    
    # Extract the most recent year and month
    recent_month <- most_recent$RecentMonth
    
    # Filter data for the most recent month across all selected years
    filtered_pie_data <- filteredData() %>%
      filter(Month.of.Death == recent_month)  # Filter to the most recent month
    
    # Summarize data for the pie chart with the new calculation
    pie_data <- filtered_pie_data %>%
      group_by(Subgroup) %>%
      summarise(
        CalculatedValue = sum(Maternal.Deaths, na.rm = TRUE) / sum(Live.Births, na.rm = TRUE) * 100000
      ) %>%
      mutate(Percentage = CalculatedValue / sum(CalculatedValue) * 100)
    
    # Check if there is data for the selected month and years
    if (nrow(pie_data) == 0) {
      return(NULL)
    }
    
    # Create the pie chart
    plot_ly(
      pie_data,
      labels = ~Subgroup,
      values = ~CalculatedValue,
      type = "pie",
      textinfo = "label+percent",
      hoverinfo = "label+value+percent",
      marker = list(colors = RColorBrewer::brewer.pal(n = nrow(pie_data), name = "Set3"))
    ) %>%
      layout(title = paste("Maternal Mortality Ratio for", month.abb[recent_month], "(Across Selected Years)"))
  })
  
  output$dataTableUI <- renderUI({
    if (input$showData) {
      tableOutput("dataTable")
    }
  })
  
  # Render the filtered data table
  output$dataTable <- renderTable({
    filteredData()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
