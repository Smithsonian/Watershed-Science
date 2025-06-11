#libraries
library(shiny)
library(data.table)
library(plotly)
library(DT)
library(dplyr)
library(lubridate)
library(zoo)
library(shinythemes)
library(bslib)
library(RColorBrewer)

#arranging & laying out
ui <- fluidPage(
  theme =  bslib::bs_theme(version = 5, bootswatch = "morph") , #journal, sketchy, minty
  titlePanel( div(img(src = "https://serc.si.edu/sites/default/files/website-gen/si_logo-primary-white.webp", height = "50px"),
             br(),br(),
            h1("flag on the play"))),
  
  #upload
  wellPanel(
    actionButton("edit_defaults", "Edit File Defaults", class = "btn-primary"),
    br(), br(),
    fileInput("file", "upload data", accept = ".csv, .dat, .txt")),
  
  #2 tabs, one for set up second for flagging
  tabsetPanel(id = "tabs",
              #sliders
              tabPanel("set ranges", value = "ranges",
                       wellPanel(
                         h3("Parameter Ranges"),
                         p("adjust the reasonable ranges for your parameters,", em("these will determine your Range flags")),
                         actionButton("apply_ranges", "apply ranges", class = "btn-primary"),
                         div(id = "paramSelectors"),
                         br(),
                         )),
              
              #flagging
              tabPanel("flag data", value = "flagging",
                       fluidRow(
                         #plot side
                         column(8,
                                wellPanel(
                                  selectInput("plot_param", "select parameter:", choices = NULL),
                                  plotlyOutput("plot"),
                                  br(),
                                  radioButtons("flag_type", "flag type:", 
                                               inline = TRUE,
                                               choices = c("Range (r)" = "r", 
                                                           "Out-of-Water (oow)" = "oow", 
                                                           "Outlier (o)" = "o",
                                                           "Dry (d)" = "d",
                                                           "Questionable (q)" = "q",
                                                           "Clear Flag" = "NA")))),
                         #table side for selected points
                         column(4,
                                wellPanel(
                                  h4("selected points"),
                                  actionButton("flag_selected", "flag selected points", class = "btn-warning"),
                                  br(),
                                  actionButton("clear_selection", "clear selection", class = "btn-info"),
                                  br(), br(),
                                  DTOutput("selected_points_table")
                                ),
                         br(),
                         #download button
                         wellPanel(actionButton("done_flagging", "accept flags", class = "btn-primary"))
                         )
                       )
              )
  )
)

server <- function(input, output, session) {
  #hold values
  values <- reactiveValues(
    data = NULL,  
    flagged = NULL, 
    export = NULL,
    params = NULL,       
    selected_points = NULL,
    ranges_applied = FALSE,
    window = 30,
    sds = 3,
    timestamp_column = 1,
    sitename_column = 2,
    timeformat  = "%Y-%m-%d %H:%M:%S"
  )
  
  #add default editing into a modal, to clean up the UI
  observeEvent(input$edit_defaults, {
    showModal(modalDialog(
      title = "Edit File Defaults",
      h5("Provide some info before uploading your file!"),
      numericInput("timestamp_column", "Timestamp Column Index:", 1, min = 1),
      numericInput("sitename_column", "Site Name Column Index:", 2, min = 1),
      selectInput("timeformat", "Timestamp Format (strptime):", 
                  choices = c("%Y-%m-%d %H:%M:%S", "%m/%d/%Y  %H:%M", "%m/%d/%Y  %H:%M:%S %p")),
      h5("Enter values to determine outliers!"),
      p("If you want an aggressive smooth, increase the rolling average window & decrease the standard deviation cut-off."),
      numericInput("window", "Rolling average window:", 30, min = 10),
      numericInput("sds", "Standard deviations:", 3, min = 2),
      footer = tagList(
        actionButton("confirm_defaults", "Accept Changes")  
      ),
      easyClose = TRUE
    ))
  })
  
  #store inputs
  observeEvent(input$confirm_defaults, {
    req(input$timestamp_column, input$sitename_column, input$timeformat, input$window, input$sds) 
    values$timestamp_column <- input$timestamp_column
    values$sitename_column <- input$sitename_column
    values$timeformat <- input$timeformat
    values$window <- input$window
    values$sds <- input$sds
    removeModal() 
  })
  
  
#accept file for flagging. can take .csv/.dat/.txt
  observeEvent(input$file, {
    req(input$file)
    values$ranges_applied <- FALSE
    timestamp_column <- values$timestamp_column
    sitename_column <- values$sitename_column
    timeformat <- values$timeformat
    #set view to the range selectors
    updateTabsetPanel(session, "tabs", selected = "ranges")
    # read and format data (should probably add a modal/popup for if a file isn't formatted correctly or just edit so you can select your timestamp columns + add a site name)
    df <- unique(fread(input$file$datapath))
    colnames(df)[timestamp_column] <- "TIMESTAMP"
    colnames(df)[sitename_column] <- "SiteName"
    df <- df %>%
      relocate(SiteName)%>%
      relocate(TIMESTAMP)

    df$TIMESTAMP <- as.POSIXct(df$TIMESTAMP, format = timeformat) #add more try formats so this is less likely to tweak
    df$RowID <- seq.int(nrow(df))
    params <- setdiff(names(df), c("RowID","TIMESTAMP", "SiteName"))
    values$params <- params
    #cycle through parameters, make sure theyre numeric 
    rms=F
    rms_names = c()
    for(p in params) {
      df[[p]] <- as.numeric(as.character(df[[p]]))
       if (all(is.na(df[[p]]))){
         rms=T
         rms_names = c(rms_names,  p)
       }    
    }
  
    values$params <- params[!params %in% rms_names]
    params <- values$params
    df <- df %>% select(-all_of(rms_names))

    #hold the file data in reactive val
    values$data <- df
    
    #create flag columns + hold in flagged reactive val
    for(p in params) {
      df[[paste0(p, "_flag")]] <- NA
    }
    values$flagged <- df
    #range sliders
    for(p in params) {
      min_val <- min(df[[p]], na.rm = TRUE)
      max_val <- max(df[[p]], na.rm = TRUE)
      
      insertUI(
        selector = "#paramSelectors",
        ui = sliderInput(
            inputId = paste0("slider_", p),
            label = paste("Range for", p),
                        # min = ifelse(is.finite(min_val), min_val, 0), 
                        # max = ifelse(is.finite(max_val), max_val, 0), 
            min = min_val,
            max = max_val,
            value = c(min_val, max_val),
            step = 0.5,
            round = 0.5
        )
      )
    }})
  
  # go! (do the rolling clean and range flagging )
  observeEvent(input$apply_ranges, {
    req(values$data, values$params)
    window <- values$window
    sds <- values$sds
    df <- values$flagged
    for(p in values$params) {
      range_id <- paste0("slider_", p)
        range <- input[[range_id]]
        flag_col <- paste0(p, "_flag")
          roll_mean <- rollapply(df[[p]], window, mean, align = "center", fill = NA, na.rm = TRUE)
          roll_sd <- rollapply(df[[p]], window, sd, align = "center", fill = NA, na.rm = TRUE)
          z_score <- (df[[p]] - roll_mean) / roll_sd
          df[[flag_col]] <- ifelse((z_score <= -sds | z_score >= sds), "o", df[[flag_col]])
          df[[flag_col]] <- ifelse(df[[p]] < range[1] | df[[p]] > range[2], "r", df[[flag_col]])
    }
    #update flagged val 
    values$flagged <- df
    values$ranges_applied <- TRUE
    #update tab view 
    updateSelectInput(session, "plot_param", choices = values$params)
    updateTabsetPanel(session, "tabs", selected = "flagging")
  })
  
  #plotly
  output$plot <- renderPlotly({
    req(values$flagged, input$plot_param, values$ranges_applied)
    req(input$tabs == "flagging")
    param <- input$plot_param
    flag_col <- paste0(param, "_flag")
    
    #flag colors
    flag_colors <- list(
      "unflagged" = "#A9a9a9",
      "r" = "#ee5f5b",
      "o" = "#5bc0de",
      "oow" = "#5cb85c",
      "d" = "#f89406",
      "q" = "purple3"
    )
   
     add_flag_trace <- function(p, data, flag, flagtag, flag_col, param, color) {
      flagged_data <- if (flagtag == "unflagged") {
        data[is.na(data[[flag_col]]), ]
      } else {
        data[data[[flag_col]] == flagtag, ]
      }
      p <- add_trace(p, data = flagged_data, 
                     key = ~RowID,
                     x = ~TIMESTAMP, 
                     y = as.formula(paste0("~`", param, "`")),
                     type = "scatter", 
                     mode = "markers",
                     marker = list(color = color, size = 5),
                     name = flag)
      return(p)
    }
    
    p <- plot_ly(values$flagged, source = "plot")
    p <- add_flag_trace(p, values$flagged, "Unflagged","unflagged", flag_col, param, flag_colors$unflagged)
    p <- add_flag_trace(p, values$flagged, "Range", "r", flag_col, param, flag_colors$r)
    p <- add_flag_trace(p, values$flagged, "Outlier","o", flag_col, param, flag_colors$o)
    p <- add_flag_trace(p, values$flagged, "Out-of-water","oow", flag_col, param, flag_colors$oow)
    p <- add_flag_trace(p, values$flagged, "Dry","d", flag_col, param, flag_colors$d)
    p <- add_flag_trace(p, values$flagged, "Questionable","q", flag_col, param, flag_colors$q)
    
    p <- layout(p, 
                xaxis = list(title = "Time"),
                yaxis = list(title = param),
                dragmode = "select")
    
    event_register(p, 'plotly_selected')
    
    return(p)
  })

  #selected data from plot
  observe({
    selected_data <- event_data("plotly_selected", source = "plot")
    if(!is.null(selected_data)) {
     # values$selected_points <- selected_data$pointNumber +1
      values$selected_points <- as.integer(selected_data$key)
    }
  })

  #data table of selected plot points
  output$selected_points_table <- renderDT({
    req(values$flagged, input$plot_param,values$selected_points)
    param <- input$plot_param
    flag_col <- paste0(param, "_flag")
    
    if(is.null(values$selected_points) | length(values$selected_points) < 1) {
      table_data<- values$flagged[0]
    }else{
      table_data <- values$flagged %>% slice(values$selected_points)
    }
    table <- data.frame(
      Timestamp = table_data$TIMESTAMP,
      Value = table_data[[param]],
      Flag = table_data[[flag_col]]
    )
    datatable(table,rownames = FALSE)
  })
  
  #flag the selected points
  observeEvent(input$flag_selected, {
    req(values$flagged, input$plot_param, values$selected_points)
    param <- input$plot_param
    flag_col <- paste0(param, "_flag")
    if(!is.null(values$selected_points) && length(values$selected_points) > 0) {
      flag_value <- ifelse(input$flag_type == "NA", NA, input$flag_type)
      point_indices <- values$selected_points
      values$flagged[point_indices, flag_col] <- flag_value
      #if flag is "dry" apply to all columns
      if(input$flag_type == "d"){
      params <- values$params
      for(p in params){
        flag_col <- paste0(p, "_flag")
        values$flagged[point_indices, flag_col] <- input$flag_type
        }
      }
      
    }
  })
  
  #clear selection variable
  observeEvent(input$clear_selection, {
    values$selected_points <- NULL
  })
  
  #reset when parameter switches
  observeEvent(input$plot_param, {
    values$selected_points <- NULL
  })
  
  #download flagged data
  observeEvent(input$done_flagging, {
    req(values$flagged, input$file)
    df <- values$flagged
     params <- values$params
     for(p in params){
    ## commentted out to not remove any values, just keep flags
    #   flag_col <- paste0(p, "_flag")
    #   df[[p]] <- ifelse(is.na(df[[flag_col]]) | df[[flag_col]] == "o", df[[p]], "NA")
    df[[p]] <- ifelse(df[[p]] == "NaN" | is.na(df[[p]]), "NA", df[[p]])
     }
   values$export <- df[,-c("RowID")]
     #popup
    showModal(modalDialog(
      title = "Download Flagged Data",
      "Your flagged data is ready to download.",
      textInput("user", "Initials:"),
      textInput("notes", "Flagging Session notes:"),
      footer = tagList(
        downloadButton("download_flagged", "Download CSV"),
        downloadButton("download_notes", "Download Notes"),
        modalButton("Cancel")
      ),
      easyClose = TRUE
    ))
  })
  
  output$download_flagged <- downloadHandler(
    filename = function() {
      #original filename
      base <- tools::file_path_sans_ext(input$file$name)
      paste0(base, "_flagged_", toupper(input$user), ".csv")
    },
    content = function(file) {
    fwrite(values$export, file)
      }
    ) 

  output$download_notes <- downloadHandler(
    filename = function() {
      base <- tools::file_path_sans_ext(input$file$name)
      paste0(base, "_flagged_notes_", toupper(input$user), ".txt")
    },
    content = function(file) {
      base <- tools::file_path_sans_ext(input$file$name)
      notes <- paste0(" File:", base, "\n Flagged On:", today(), 
                      "\n Flagged By:", input$user,
                      "\n Notes:", input$notes)
      writeLines(notes, file)  
    }
  )
  
}

shinyApp(ui, server)

