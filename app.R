library(shiny)
library(readxl)
library(ggplot2)
library(tidyverse)
library(scales)

#-----helper function - for building scatter plots with line---------------
plot_pointswithline <- function(data,x,y,groupby,colorby,shapeby,panelby){
  if (is.null(data)==FALSE)
  p <- ggplot(data,aes_string(as.name(x),as.name(y),group=groupby,color=as.name(colorby),shape=as.name(shapeby))) +
    geom_point(size = 3) +
    theme(text = element_text(size = 20),plot.title = element_text(hjust = 0.5))+
    ggtitle(paste0(as.name(y)," vs. ",as.name(x)))+
    facet_grid(~get(panelby))+
    guides(colour = guide_legend(order = 1), 
           shape = guide_legend(order = 2))
  if(groupby=='combined_line') p <- p + geom_line()
  return(p)
}

# User interface ----
ui <- function(request) {
  fluidPage(
      sidebarLayout(
        sidebarPanel(
          fileInput(
            inputId = "sourcefile",
            label = "Choose data (Excel only for now)"
          ),
          uiOutput(outputId = "select_sheet"),
          uiOutput(outputId = "x"),
          splitLayout(
            textInput(
              inputId = "x_min", 
              label = NULL, 
              value = "", 
              #width = "600px", 
              placeholder = "X min"
            ),
            textInput(
              inputId = "x_max", 
              label = NULL, 
              value = "", 
              #width = "600px", 
              placeholder = "X max"
            ),
            actionButton(
              inputId = "reset_x",
              label = "Default"
            )
          ),
          uiOutput(outputId = "y"),
          splitLayout(
            textInput(
              inputId = "y_min", 
              label = NULL, 
              value = "", 
              #width = "200px", 
              placeholder = "Y min"
            ),
            textInput(
              inputId = "y_max", 
              label = NULL, 
              value = "", 
              #width = "200px", 
              placeholder = "Y max"
            ),
            actionButton(
              inputId = "reset_y",
              label = "Default")
            )
          ),  
        mainPanel(
          br(),
          tabsetPanel(
            tabPanel("Plot", br(), plotOutput(outputId = "scatter")),
            tabPanel("Table", tableOutput(outputId = "table"))
          )
        )
      ),

  sidebarLayout(
    sidebarPanel(
      uiOutput(outputId = "color"),
      uiOutput(outputId = "shape"),
      uiOutput(outputId = "panel"),
      uiOutput(outputId = "line1"),
      uiOutput(outputId = "line2"),
      uiOutput(outputId = "line3")
      ),
    mainPanel(
      actionButton("add_btn", "Add Filter"),
      actionButton("rm_btn", "Remove Filter"),
      uiOutput(outputId = "filters"),
      uiOutput(outputId = "select_all"),
      uiOutput(outputId = "deselect_all"),
      uiOutput(outputId = "values")
    )
  ),
  tableOutput('show_inputs'),
  tableOutput('lastselect'),
  bookmarkButton()
  )
}
# Server logic ----
server <- function(input, output, session) {

#---------DATA SHEET IMPORT----------------------------------------------------------------------

#First returns a list of dataframes corresponding to each sheet in the excel file
  data <- reactive({
    if (is.null(input$sourcefile)) {
      return(NULL)
    }
    file <- input$sourcefile
    sheetnames <- excel_sheets(file$datapath)
    dfs <- lapply(sheetnames,function(x) read_excel(file$datapath,sheet = x))
    names(dfs) <- sheetnames
    return(dfs)
  })

#Then waits for user to select a sheet
  output$select_sheet <- renderUI({
    selectInput(
      inputId = "select_sheet",
      label = "Select sheet",
      choices = names(data())
    )
  })

#Assigns user-selected sheet as df and replaces empty values
  df <- reactive({
    if (is.null(data()) == FALSE)
    data()[[input$select_sheet]] %>%
      mutate(across(where(is.character),function(x) ifelse(is.na(x),"N/A",x)))
  })

#---Input boxes for selecting axes--------
  output$x <- renderUI({
    selectInput(
      inputId = "x",
      label = "Select x-axis:",
      choices = colnames(df())
    )
  })
  
  output$y <- renderUI({
    selectInput(
      inputId = "y",
      label = "Select y-axis:",
      choices = colnames(df())
    )
  })
  
  output$color <- renderUI({
    selectInput(
      inputId = "color",
      label = "Color by:",
      choices = c(colnames(df()))
    )
  })
  
  output$shape <- renderUI({
    selectInput(
      inputId = "shape",
      label = "Shape by:",
      choices = c(colnames(df()))
    )
  })
  
  output$panel <- renderUI({
    selectInput(
      inputId = "panel",
      label = "Panel by:",
      choices = c(colnames(df()))
    )
  })
  
  output$line1 <- renderUI({
    selectInput(
      inputId = "line1",
      label = "Line by:",
      choices = c("None",colnames(df()))
    )
  })
  output$line2 <- renderUI({
    selectInput(
      inputId = "line2",
      label = NULL,
      choices = c("None",colnames(df()))
    )
  })
  output$line3 <- renderUI({
    selectInput(
      inputId = "line3",
      label = NULL,
      choices = c("None",colnames(df()))
    )
  })
  
  new_df <- reactive({
    if (input$line1 == "None" & input$line2 == "None" & input$line3 == "None") {
      return(df())
    }
    else{
      combined_line <- paste(df()[[input$line1]],df()[[input$line2]],df()[[input$line3]])
      cbind(df(),combined_line)
    }
  })

#-------DATAFRAME ALLINPUTS FOR REMEMBERING USER SELECTIONS WHEN ADDING NEW FILTERS---------
  counter <- reactiveValues(n = 0)
  # Track all user inputs
  AllInputs <- reactive({
    myvalues <- NULL
    newvalues <- NULL
    for(i in 1:length(names(input))){
          newvalues <- as.data.frame(cbind(names(input)[i], input[[(names(input)[i])]]))
      myvalues <- as.data.frame(bind_rows(myvalues, newvalues))
    }
    myvalues
  })
  
#---------ADD/REMOVE FILTER BUTTONS----------------------------------------------------------------------
  
  observeEvent(input$add_btn, {
    counter$n <- counter$n + 1
  })
  observeEvent(input$rm_btn, {
    if (counter$n > 0) counter$n <- counter$n - 1
  })
  
#---------FILTER OPTIONS----------------------------------------------------------------------
  
  
  filters <- reactive({
    n <- counter$n
    if (n > 0) {
      lapply(seq_len(n), function(i) {
        selectInput(
          inputId = paste0("filter",i),
          label = "Filter by:",
          choices = c("",colnames(df())),
          #keep using df instead of new_df, the combined line column doesn't need to be selectable
          selected = (AllInputs() %>% filter(AllInputs()[["V1"]] == paste0("filter",i)))[["V2"]]
        )
      })
    }
  })
  
#---------VALUES FOR FILTER CHECKBOX---------------------------------------------------------------------------
  
#   df_for_values <- reactive({
#     n <- counter$n
#     test <- new_df()
#     if (n > 1) {
#      for (i in c(2:n)) {
#      test <- test %>% filter(test[[input[[paste0("filter",i-1)]]]] %in% input[[paste0("options",i-1)]])
#      }
#     }
#     #test[[input[[paste0("filter",1)]]]] <- df()[[input[[paste0("filter",1)]]]]
#     return(test)
# })
  
  #Helper function - loops through all filters and filters new_df by them all-----
  filter_values <- function(n) {
    test <- new_df()
    if (n > 1) {
      for (i in c(2:n)) {
        test <- test %>% filter(test[[input[[paste0("filter",i-1)]]]] %in% input[[paste0("options",i-1)]])
      }
    }
    #test[[input[[paste0("filter",1)]]]] <- df()[[input[[paste0("filter",1)]]]]
    return(test)
  }
  
  values <- reactive({
    n <- counter$n
    if (n > 0) {
      lapply(seq_len(n), function(i) {
        checkboxGroupInput(
          inputId = paste0("options",i),
          label = "Values",
          choices = {
              # if (i==n) {
              #   na.omit(unique(df_for_values()[[input[[paste0("filter",i)]]]]))
              # }
              # else if (i > 1) {
              #if (i > 1) {
                na.omit(unique(filter_values(i)[[input[[paste0("filter",i)]]]]))
                #na.omit(unique(filter(new_df(),new_df()[[input[[paste0("filter",i-1)]]]] %in% input[[paste0("options",i-1)]])[[input[[paste0("filter",i)]]]]))
              #}
              #else {
                #na.omit(unique(new_df()[[input[[paste0("filter",i)]]]]))              
              #}
            },
          selected = {
            if (is.null(input[[paste0("options",i)]]) == TRUE & input[[paste0("deselect_all",i)]]==0)
              na.omit(unique(filter_values(i)[[input[[paste0("filter",i)]]]]))
            else
              (AllInputs() %>% filter(AllInputs()[["V1"]] == paste0("options",i)))[["V2"]]
          }
        )
      })
    }
    
  })

#---------SELECT ALL AND DESELECT ALL BUTTONS---------------------------------------------------------------------------

  select_all <- reactive({
    n <- counter$n
    if (n > 0) {
      lapply(seq_len(n), function(i) {
        actionButton(
          inputId = paste0("select_all",i),
          label = "Select all",
        )
      })
    }
  })
  
  deselect_all <- reactive({
    n <- counter$n
    if (n > 0) {
      lapply(seq_len(n), function(i) {
        actionButton(
          inputId = paste0("deselect_all",i),
          label = "Deselect all",
        )
      })
    }
  })

  #observe is useful here as it's passively updating the list of select_all buttons; it requires an additional observeEvent to actually trigger updating the checkboxGroupInput
  #reactive wasn't suitable as you're not trying to return anything
  observe({
    n <- counter$n
    if (n > 0) {
      select_all_list <- paste0("select_all",n)
      lapply(select_all_list, function(x) {
        observeEvent(input[[x]],updateCheckboxGroupInput(
          session,
          inputId = paste0("options",n),
          #choices = na.omit(unique(df_for_values()[[input[[paste0("filter",n)]]]])),
          choices = na.omit(unique(filter_values(n)[[input[[paste0("filter",n)]]]])),
          #that gives you a vector with the selected values of the preceding filter
          selected = na.omit(unique(filter_values(n)[[input[[paste0("filter",n)]]]]))
        ))
      })
    }

  })

  observe({
    n <- counter$n
    if (n > 0) {
      deselect_all_list <- paste0("deselect_all",n)
      lapply(deselect_all_list, function(x) {
        observeEvent(input[[x]],updateCheckboxGroupInput(
          session, 
          inputId = paste0("options",n),
          choices = na.omit(unique(filter_values(n)[[input[[paste0("filter",n)]]]])),
                #filter(df(), df()[[input[[paste0("filter",n-1)]]]] == input[[paste0("options",n-1)]])[[input[[paste0("filter",n)]]]]
          selected = NULL
        ))
      })
    }
  })
  
  filtered_data <- reactive({
    n <- counter$n
    holder <- new_df()
    if (n == 0) {
      return(holder)
    }

    else {
      for (i in seq_len(n)) {
        if (input[[paste0("filter",i)]] != "") {
          holder <- filter(holder,holder[[input[[paste0("filter",i)]]]] %in% input[[paste0("options",i)]])
        }
      }
      return(holder)
    }
  })

#-----Resetting x-axis and y-axis-------
  
  observeEvent(input$reset_x,{
    updateTextInput(session,
                    inputId = "x_min",
                    #label = "X-axis min:", 
                    value = "",
                    placeholder = "X min"
                    )
    updateTextInput(session,
                    inputId = "x_max",
                    #label = "X-axis max:", 
                    value = "",
                    placeholder = "X max"
    )
  })
  
  observeEvent(input$reset_y,{
    updateTextInput(session,
                    inputId = "y_min",
                    #label = "Y-axis min:", 
                    value = "",
                    placeholder = "Y min"
    )
    updateTextInput(session,
                    inputId = "y_max",
                    #label = "Y-axis max:", 
                    value = "",
                    placeholder = "Y max"
    )
  })

#-----Other fixed outputs-----------------------------
  
  #-----helper function - for getting default x and y ranges from ggplot-----------
  default_ranges <- function(plt) {
    x_min_def <- ggplot_build(plt)$layout$panel_params[[1]]$x.range[[1]]
    x_max_def <- ggplot_build(plt)$layout$panel_params[[1]]$x.range[[2]]
    y_min_def <- ggplot_build(plt)$layout$panel_params[[1]]$y.range[[1]]
    y_max_def <- ggplot_build(plt)$layout$panel_params[[1]]$y.range[[2]]
    list(x_min_def,x_max_def,y_min_def,y_max_def)
  }
  
  output$scatter <- renderPlot({
    if (is.null(new_df()[['combined_line']]) == FALSE) 
      p <- plot_pointswithline(data = filtered_data(), x = input$x, y = input$y,groupby="combined_line",colorby=input$color,shapeby=input$shape,panelby=input$panel)
    else
      p<- plot_pointswithline(data = filtered_data(), x = input$x, y = input$y,groupby="NULL",colorby=input$color,shapeby=input$shape,panelby=input$panel)
    
    if (input$x_min!="") x_min <- as.numeric(input$x_min)
    else x_min <- default_ranges(p)[[1]]
    if (input$x_max!="") x_max <- as.numeric(input$x_max)
    else x_max <- default_ranges(p)[[2]]
    if (input$y_min!="") y_min <- as.numeric(input$y_min)
    else y_min <- default_ranges(p)[[3]]
    if (input$y_max!="") y_max <- as.numeric(input$y_max)
    else y_max <- default_ranges(p)[[4]]
    
    p <- p+coord_cartesian(xlim = c(x_min,x_max),ylim=c(y_min,y_max))
    
    if (is.numeric(filtered_data()[[input$x]]) == TRUE)
          p <- p + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
    if (is.numeric(filtered_data()[[input$y]]) == TRUE)
          p <- p + scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
    
    return(p)
  })
  

  
  output$table <- renderTable({
    filtered_data()
  })

  # output$show_inputs <- renderTable({
  #   AllInputs()
  #   #default_ranges(plot_pointswithline(data = filtered_data(), x = input$x, y = input$y,groupby="combined_line",colorby=input$color))
  # })
  
  # output$lastselect <- renderTable({
  #   df_for_values()
  # })
  
  output$filters <- renderUI({
    if (is.null(filters())==FALSE)
    do.call(flowLayout,filters())
  })
  output$select_all <- renderUI({
    if (is.null(select_all())==FALSE)
    do.call(flowLayout,select_all())
  })
  output$deselect_all <- renderUI({
    if (is.null(deselect_all())==FALSE)
    do.call(flowLayout,deselect_all())
  })
  output$values <- renderUI({
    if (is.null(values())==FALSE)
    do.call(flowLayout,values())
  })
}

# Run app ----
shinyApp(ui, server, enableBookmarking = "server")
