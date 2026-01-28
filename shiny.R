#Call data cleaning script----
source("cleaning_data.R")

# PACKAGES ----
pacman::p_load(
  tidyverse,    # data management and visualization
  scales,       # quick percents
  shiny,        # dashboards
  shinyWidgets, # UI widgets
  DT            # interactive tables
)
###############################################################################
# DASHBOARDS WITH SHINY ----

# divide location groups into priority regions and others
group1 <- c(
  "Global",
  "Central Europe, Eastern Europe, and Central Asia",
  "High-income",
  "Latin America and Caribbean",
  "North Africa and Middle East",
  "South Asia",
  "Southeast Asia, East Asia, and Oceania",
  "Sub-Saharan Africa"
)

group2 <- setdiff(sort(unique(linelist$location_name)), group1)

# select year range 
year_min <- min(linelist$year_id, na.rm = TRUE)
year_max <- max(linelist$year_id, na.rm = TRUE)


###############################################################################
# UI----

ui <- fluidPage(
  
  tags$head(
    tags$script(HTML("
      $(function () {
        $('[data-toggle=\"popover\"]').popover({
          trigger: 'click',
          placement: 'bottom',
          container: 'body',
          html: true
        });

        $(document).on('click', '.popover-close', function () {
          $('[data-toggle=\"popover\"]').popover('hide');
        });
      });
    ")),
    
    tags$style(HTML("
      .tfr-title {
        display: flex;
        align-items: center;
        gap: 8px;
        margin-bottom: 5px;
      }
      .tfr-title i {
        color: #6c757d;
        cursor: pointer;
      }
      .popover {
        max-width: 420px;
      }
    "))
  ),
  
  # Title + info icon
  tags$div(
    class = "tfr-title",
    tags$h2("Total fertility rate with projections"),
    
    tags$span(
      icon("circle-info"),
      `data-toggle` = "popover",
      title = HTML("
        <div style='display:flex; justify-content:space-between; align-items:center;'>
          <strong>What is Total Fertility Rate?</strong>
        </div>
      "),
      `data-content` = HTML("
        <div style='font-size:13px; color:#555;'>
          The total fertility rate (per woman) is the average number of children
          a hypothetical cohort of women would have at the end of their reproductive period 
          if they were subject during their whole lives to the fertility rates of a given period 
          and if they were not subject to mortality.<br><br>
          Estimates are shown for 1950-2021 with projections for 2022-2100, 
          as provided by the Institute for Health Metrics and Evaluation (IHME). 
        </div>
      ")
    )
  ),

  sidebarLayout(
    sidebarPanel(
      
      # select countries and regions
      pickerInput(
        inputId = "location_name",
        label = "Select countries and regions",
        choices = list(
          "Regions" = group1,
          "Countries / Others" = group2
        ),
        selected = group1,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE,
          `selected-text-format` = "count > 3",
          `count-selected-text` = "{0} locations selected"
        )
      ),
      
      # select year range   
      sliderInput(
        inputId = "year_id",
        label = "Select year range:",
        min = year_min,
        max = year_max,
        value = c(year_min, year_max),
        step = 1,
        sep = ""
      ),
      
      # create Play time-lapse button           
      actionButton(
        inputId = "play",
        label = "▶ Play time-lapse"
      ),

      # create download button      
      downloadButton(
        outputId = "download_plot",
        label = "Download chart (PNG)"
      ),
      downloadButton(
        outputId = "download_data",
        label = "Download displayed data"
      )
    ),
  
    mainPanel(
      plotOutput(
        "tfr_line",
        height = "500px",
        # hover below the plot
        hover = shiny::hoverOpts("plot_hover", delay = 50, nullOutside = TRUE)),
      fluidRow(
        column(8, h4(textOutput("year_title"))),
        column(9, actionButton("reset_hover", "Reset", icon = icon("eraser")))),
      DTOutput("hover_values")
    )
  )
)
  
    
###############################################################################
# SERVER ----
server <- function(input, output, session) {
  
  # store last hovered year
  hover_year <- reactiveVal(NULL)
  observeEvent(input$reset_hover, {
    hover_year(NULL)
  })
  
  observeEvent(input$plot_hover, {
    h <- input$plot_hover
    
    if (is.null(h) || is.null(h$x) || is.na(h$x)) {
      hover_year(NULL)
    } else {
      hover_year(as.integer(round(h$x)))
    }
  })

  # abbreviate names if the number of location > 50  
display_names <- reactive({
  locs <- input$location_name
  if (length(locs) >= 50) {
    substr(locs, 1, 3)
  } else {
    locs
  }
})

    # filtered data of year range  
filteredData <- reactive({
  req(input$year_id)
  
  linelist %>% 
    filter(location_name %in% input$location_name,
           year_id >= input$year_id[1],
           year_id <= input$year_id[2]) %>%
    mutate(
      period = ifelse(year_id >= 2022, "Future projections", "Past estimates"),
      display_name = if (length(input$location_name) >= 50) substr(location_name, 1, 3) else location_name
    )
})

  # plot  
  make_plot <- reactive({
    df <- filteredData()
    yr <- hover_year()

    # no data selected
    if (nrow(df) == 0) {
      mid_x <- mean(input$year_id)
      mid_y <- 2.5
      
      return(
        ggplot() +
          coord_cartesian(xlim = input$year_id, expand = FALSE) +
          scale_x_continuous(
            breaks = c(input$year_id[1], input$year_id[2]),
            labels = function(x) formatC(x, format = "f", digits = 0)
          ) +
          scale_y_continuous(
            limits = c(0, 5),
            labels = function(y) formatC(y, format = "f", digits = 1)
          ) +
          annotate(
            "text",
            x = mid_x,
            y = mid_y,
            label = "No country or region selected.\nTry adding countries and regions to display data.",
            size = 10,
            color = "grey40",
            hjust = 0.5,
            vjust = 0.5
          ) +
          labs(
            title = "Total Fertility Rate (TFR)",
            x = "Year",
            y = "TFR"
          ) +
          theme_minimal()
      )
    }
    
    # normal plot
    p <- ggplot(df, aes(year_id, val, color = display_name,
                        linetype = period, group = interaction(display_name, period))) +
      geom_line(linewidth = 0.5) + geom_point(size = 0.5) +
      scale_linetype_manual(
        values = c("Past estimates" = "solid", "Future projections" = "dotted")
      ) +
      labs(
        title = "Total Fertility Rate (TFR)",
        x = "Year",
        y = "TFR",
        color = "Countries and regions",
        linetype = "Period"
      ) +
      guides(
        color = guide_legend(order = 1),
        linetype = guide_legend(order = 2)
      ) +      
      coord_cartesian(xlim = input$year_id, expand = FALSE) +
      
      # axis formatting
      scale_x_continuous(
        breaks = function(lims) unique(c(lims[1], lims[2], seq(year_min, year_max, by = 10))),
        labels = function(x) formatC(x, format = "f", digits = 0)
      ) +
    
      scale_y_continuous(
        limits = c(0, NA),
        breaks = scales::pretty_breaks(n = 5),
        labels = function(y) formatC(y, format = "f", digits = 1),
        expand = expansion(mult = c(0, 0.05))
      ) +
      theme_minimal() +
      
      # marks key year and replacement fertility level
      geom_vline(xintercept = 2021, linetype = "longdash", color = "black", linewidth = 0.5) + 
      geom_hline(yintercept = 2.1, linetype = "longdash", color = "black", linewidth = 0.5) +
      theme(
        legend.position = "right",
        legend.box = "vertical",
        plot.title = element_text(size = 16, face = "bold")
      )
    
    # hover vertical line
    if (!is.null(yr)) {
      p <- p + geom_vline(xintercept = yr, linetype = "solid", color = "red", linewidth = 0.8)
    }
    
    p
  })
  
  # render plot
  output$tfr_line <- renderPlot({
    input$plot_hover
    make_plot()
  })   

  # hover title    
  output$year_title <- renderText({
    if (is.null(hover_year())) "Hover over the chart to display data"
    else paste0("Data in year ", formatC(hover_year(), format = "f", digits = 0))
  })
  
  
  # hover table
  output$hover_values <- renderDT({
    if (is.null(hover_year())) return(NULL)
    df <- filteredData() %>%
      filter(year_id == hover_year()) %>%
      transmute(
        Location = location_name,
        TFR = paste0(
          formatC(val, format = "f", digits = 2), " (",
          formatC(lower, format = "f", digits = 2), "–",
          formatC(upper, format = "f", digits = 2), ")"
        )
      )
    
    datatable(
      df,
      rownames = FALSE,
      options = list(
        order = list(list(0, "asc")),   # default A → Z by Location
        pageLength = 10,
        autoWidth = TRUE
      )
    )
  })
  
  
  # downloads  
  output$download_plot <- downloadHandler(
    filename = function() paste0("tfr_plot_", Sys.Date(), ".png"),
    content = function(file) {
      ggsave(file, plot = make_plot(), device = "png", width = 10, height = 6, dpi = 300)
    }
  )
  
  output$download_data <- downloadHandler(
    filename = function() 
      paste0(
        "tfr_filtered_",
        Sys.Date(),
        ".csv"
      ),
    content = function(file) 
      write_csv(filteredData(), file)
  )
  
  # time-lapse animation  
  playing <- reactiveVal(FALSE)
  play_start <- reactiveVal(year_min)
  
  observeEvent(input$play, {
    current_range <- input$year_id
    
    if (!playing()) {
      if (current_range[2] >= year_max) {
        updateSliderInput(session, "year_id", value = c(year_min, year_min+1))
        play_start(year_min)
      } else {
        play_start(current_range[1])
      }
      playing(TRUE)
    } else {
      playing(FALSE)
    }
  })
  
  observe({
    req(playing())
    invalidateLater(1000, session)
    
    current_range <- input$year_id
    new_max <- current_range[2] + 1
    
    if (new_max > year_max) {
      playing(FALSE)
      return()
    }
    
    updateSliderInput(
      session,
      "year_id",
      value = c(play_start(), min(new_max, year_max))
    )
  })
  
  observe({
    updateActionButton(
      session,
      "play",
      label = ifelse(playing(), "⏸ Pause time-lapse", "▶ Play time-lapse")
    )
  })
}
###############################################################################
# RUN ----
shinyApp(ui = ui, server = server)
