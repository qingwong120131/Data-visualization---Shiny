library(shiny)
library(readxl)
library(ggplot2)
library(ggprism)
library(ggthemes)
library(tidyverse)
library(scales)
library(shinyjs)
library(ggpubr)



#----QW 08/16/21 SHOWING TONS OF DATA WITH TABS
#----QW 11/27/21 QOL changes
#----QW 12/1/21 removed multi-step data refresh
#----QW 12/28/21 fixed a bunch of bugs

#-----helper function - for building scatter plots with line---------------
plot_pointswithline <- function(data,x,y,groupby,colorby,shapeby,panelby){
  if (is.null(data)==FALSE)
    p <- ggplot(data,aes_string(as.name(x),as.name(y),group=groupby,color=if(colorby!="None") as.name(colorby),shape=if(shapeby!="None") as.name(shapeby))) +
      geom_point(size = 5) +
      #scale_shape_manual(values = c(16,1)) + 
      theme_prism(
        palette = "black_and_white",
        base_size = 16
      ) +
      theme(legend.position = "right") +
      ggtitle(paste0(as.name(y)," vs. ",as.name(x)))+
      facet_grid(if (panelby!="None") ~get(panelby))+
      guides(colour = guide_legend(order = 1), 
             shape = guide_legend(order = 2))
  if(groupby=='combined_line') p <- p + geom_line(data=data[!is.na(data[[as.name(y)]]),])
  return(p)
}

#-----helper function - for forcing integer x axis breaks on the dashboard---------------

integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

# User interface ----
ui <- fluidPage(
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      
      actionButton(
        inputId = "browse",
        label = "Browse data (.xlsx)",
        #width = "75%"
        
      ),
      br(),
      
      
      actionButton(
        inputId = "reload",
        icon = icon("refresh"),
        label = "Reload data",
        #width = "25%"
      ),
      br(),
      
      actionButton(inputId = "refresh",icon = icon("angry"),label = "Refresh Plot",class = "btn-success"),
      br(),
      br(),
      
      
      selectInput(inputId = "select_sheet",
                  label = "Select sheet",
                  choices = NULL),
      selectInput(
        inputId = "selectinput_color",
        label = "Color by:",
        choices = NULL),
      selectInput(
        inputId = "selectinput_shape",
        label = "Shape by:",
        choices = NULL),
      selectInput(
        inputId = "selectinput_panel",
        label = "Panel by:",
        choices = NULL),
      selectInput(
        inputId = "selectinput_line1",
        label = "Line by:",
        choices = NULL),
      selectInput(
        inputId = "selectinput_line2",
        label = "Line by:",
        choices = NULL),
      selectInput(
        inputId = "selectinput_line3",
        label = "Line by:",
        choices = NULL)
    ),  
    mainPanel(
      width = 9,
      br(),
      tabsetPanel(
        tabPanel("Free Plot",
                 fluidRow(
                   br(),
                   column(3,
                          selectInput(
                            inputId = "selectinput_x",
                            label = "Select x-axis:",
                            choices = NULL),
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
                          selectInput(
                            inputId = "selectinput_y",
                            label = "Select y-axis:",
                            choices = NULL),
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
                   column(9,plotOutput(outputId = "scatter"
                                       #height="800px"
                   ))
                 )
        ),
        tabPanel("BR Growth",

                 plotOutput('growth',height="600px", width = "100%")),
        tabPanel("BR Metabolites",

                 plotOutput('metabolites',height="600px", width = "100%")),
        tabPanel("BR Phenotype",

                 plotOutput('phenotype',height="600px", width = "100%")),
        tabPanel("Table", tableOutput(outputId = "table")),
        tabPanel("Back-end", )
      )
    )
  ),
  actionButton("add_btn", "Add Filter"),
  actionButton("rm_btn", "Remove Filter"),
  uiOutput(outputId = "filters"),
  uiOutput(outputId = "select_all"),
  uiOutput(outputId = "deselect_all"),
  uiOutput(outputId = "values"),
  fluidRow(
    column(width = 6, tableOutput(outputId = "show_inputs")),
    column(width = 6, tableOutput(outputId = "show_inputs2"))
  )
  
)

# Server logic ----
server <- function(input, output, session) {
  
  
  
  #---------Data sheet import----------------------------------------------------------------------
  
  # First returns a list of dataframes corresponding to each sheet in the excel file
  
  
  wb <- reactiveVal()    
  
  observeEvent(input$browse,{
    filepath <<- file.choose()
    sheetnames <<- excel_sheets(filepath)
    dfs <- lapply(sheetnames, function(x) read_excel(filepath,sheet=x))
    names(dfs) <- sheetnames
    wb(dfs)
  })
  # #Then waits for user to select a sheet
  
  observe({
    if (is.null(wb())==FALSE)
      updateSelectInput(
        inputId = "select_sheet",
        label = "Select sheet",
        choices = names(wb())
      )
  })
  
  #-------------DF() AND NEW_DF(): dataframes generated from selecting a sheet as well as multiple lineby (might be obsolete)  ----------------------------------------------------
  
  df <- reactive({
    if (is.null(wb()) == FALSE) {
      df <- wb()[[input$select_sheet]]
      if (any(df[1,] %in% c("Character","Numeric","Integer")) == TRUE){
        for (i in c(1:ncol(df))){
          if (df[[1,i]] == "Integer")
            df[[i]] =  as.integer(df[[i]])
          else if (df[[1,i]] == "Character")
            df[[i]] =  as.character(df[[i]])
          else if (df[[1,i]] == "Numeric")
            df[[i]] =  as.numeric(df[[i]])
        }
        df <- df[rowSums(is.na(df))!= ncol(df),]
        df <- df[-1,]
      }
      
    }
    return(df)
  })
  
  new_df <- reactive({
    if (input$selectinput_line1 == "None" & input$selectinput_line2 == "None" & input$selectinput_line3 == "None") {
      return(df())
    }
    else{
      combined_line <- paste(df()[[input$selectinput_line1]],df()[[input$selectinput_line2]],df()[[input$selectinput_line3]])
      #combined_line <- gsub("NA","",combined_line)
      cbind(df(),combined_line)
      #print(gsub("NA","",combined_line))
    }
  })
  #-------------Data refresh----------------------------------------------------
  
  #when the save button is hit, store input values as they currently are
  current_inputs <- eventReactive(input$reload,isolate(AllInputs()))
  
  observeEvent(input$reload,{
    dfs <- lapply(sheetnames, function(x) read_excel(filepath,sheet=x))
    names(dfs) <- sheetnames
    wb(dfs)
    delay(0,
          updateSelectInput(
            session,
            inputId = "select_sheet",
            selected = filter(current_inputs(),current_inputs()[["V1"]]=="select_sheet")[["V2"]]
          )
    )
  })
  
  observeEvent(input$reload, {
    delay(500,
          lapply(names(input), function(i) {
            if (grepl("selectinput",i,fixed=TRUE)) {
              updateSelectInput(
                session,
                inputId = i,
                selected = (current_inputs() %>% filter(current_inputs()[["V1"]] == i))[["V2"]]
              )
            }
            if (grepl("filter",i,fixed=TRUE)) {
              updateSelectInput(
                session,
                inputId = i,
                selected = (current_inputs() %>% filter(current_inputs()[["V1"]] == i))[["V2"]]
              )
            }
          }
          )
    )
  })
  
  # observeEvent(input$reload,{
  #   delay(1000,
  #         lapply(names(input), function(i) {
  #           if (grepl("options",i,fixed=TRUE)) {
  #             updateCheckboxGroupInput(
  #               session,
  #               inputId = i,
  #               #choices = na.omit(unique(filter_values(i,new_df())[[input[[paste0("filter",i)]]]])),
  #               selected = (current_inputs() %>% filter(current_inputs()[["V1"]] == i))[["V2"]],
  #             )
  #           }
  #         }
  #         )
  #   )
  # })
  
  
  #---Input boxes for selecting axes--------
  observe({
    updateSelectInput(
      inputId = "selectinput_x",
      label = "Select x-axis:",
      choices = colnames(df())
    )
    updateSelectInput(
      inputId = "selectinput_y",
      label = "Select y-axis:",
      choices = colnames(df())
    )
    updateSelectInput(
      inputId = "selectinput_color",
      label = "Color by:",
      choices = c("None",colnames(df()))
    )
    updateSelectInput(
      inputId = "selectinput_shape",
      label = "Shape by:",
      choices = c("None",colnames(df()))
    )
    updateSelectInput(
      inputId = "selectinput_panel",
      label = "Panel by:",
      choices = c("None",colnames(df()))
    )
    updateSelectInput(
      inputId = "selectinput_line1",
      label = "Line by:",
      choices = c("None",colnames(df()))
    )
    updateSelectInput(
      inputId = "selectinput_line2",
      label = NULL,
      choices = c("None",colnames(df()))
    )
    updateSelectInput(
      inputId = "selectinput_line3",
      label = NULL,
      choices = c("None",colnames(df()))
    )
  })
  
  #-------AllInputs table for storing inputs to restore during data refresh---------
  
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
  
  #---------Add/remove filter buttons----------------------------------------------------------------------
  counter <- reactiveValues(n = 0)
  observeEvent(input$add_btn, {
    counter$n <- counter$n + 1
    
  })
  observeEvent(input$rm_btn, {
    if (counter$n > 0) counter$n <- counter$n - 1
  })
  
  #---------Filter options----------------------------------------------------------------------
  
  
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
  
  #---------Filter checkbox values---------------------------------------------------------------------------
  
  #Helper function - loops through all filters and filters new_df by them all-----
  filter_values <- function(n,df) {
    # test <- new_df()
    if (n > 1) {
      for (i in c(2:n)) {
        df <- df %>% filter(df[[input[[paste0("filter",i-1)]]]] %in% input[[paste0("options",i-1)]])
      }
    }
    #test[[input[[paste0("filter",1)]]]] <- df()[[input[[paste0("filter",1)]]]]
    return(df)
  }
  
  values <- reactive({
    n <- counter$n
    if (n > 0) {
      lapply(seq_len(n), function(i) {
        checkboxGroupInput(
          inputId = paste0("options",i),
          label = "Values",
          choices = {
            na.omit(unique(filter_values(i,new_df())[[input[[paste0("filter",i)]]]]))
          },
          selected = {
            if (is.null(input[[paste0("options",i)]]) == TRUE & input[[paste0("deselect_all",i)]]==0)
              na.omit(unique(filter_values(i,new_df())[[input[[paste0("filter",i)]]]]))
            else
              (AllInputs() %>% filter(AllInputs()[["V1"]] == paste0("options",i)))[["V2"]]
          }
        )
      })
    }
  })
  
  #---------Select and deselect all filter buttons---------------------------------------------------------------------------
  
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
          choices = na.omit(unique(filter_values(n,new_df())[[input[[paste0("filter",n)]]]])),
          #that gives you a vector with the selected values of the preceding filter
          selected = na.omit(unique(filter_values(n,new_df())[[input[[paste0("filter",n)]]]]))
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
          choices = na.omit(unique(filter_values(n,new_df())[[input[[paste0("filter",n)]]]])),
          selected = NULL
        ))
      })
    }
  })
  
  #-----Filtered dataframe - this is what ends up getting plotted-------
  
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
    req(input$refresh)
    isolate({
      if (is.null(new_df()[['combined_line']]) == FALSE)
        p <- plot_pointswithline(data = isolate(filtered_data()), x = isolate(input$selectinput_x), y = isolate(input$selectinput_y),groupby="combined_line",colorby=isolate(input$selectinput_color),shapeby=isolate(input$selectinput_shape),panelby=isolate(input$selectinput_panel))
      else
        p<- plot_pointswithline(data = isolate(filtered_data()), x = isolate(input$selectinput_x), y = isolate(input$selectinput_y),groupby="NULL",colorby=isolate(input$selectinput_color),shapeby=isolate(input$selectinput_shape),panelby=isolate(input$selectinput_panel))
      
      if (input$x_min!="") x_min <- as.numeric(input$x_min)
      else x_min <- default_ranges(p)[[1]]
      if (input$x_max!="") x_max <- as.numeric(input$x_max)
      else x_max <- default_ranges(p)[[2]]
      if (input$y_min!="") y_min <- as.numeric(input$y_min)
      else y_min <- default_ranges(p)[[3]]
      if (input$y_max!="") y_max <- as.numeric(input$y_max)
      else y_max <- default_ranges(p)[[4]]
      
      p <- p+coord_cartesian(xlim = c(x_min,x_max),ylim=c(y_min,y_max))
      
      if (is.integer(filtered_data()[[input$selectinput_x]]) == TRUE){
        p <- p + scale_x_continuous(breaks = pretty_breaks(n = 10))
      }
      
      else if (is.numeric(filtered_data()[[input$selectinput_x]])){
        p <- p + scale_x_continuous(breaks = integer_breaks(n=10))
      }
      
      if (is.numeric(filtered_data()[[input$selectinput_y]]) == TRUE)
        p <- p + scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
      return(p)
    }
    )
  })
  
  
  growth_plots <- list(
    "Viable Count (e6/mL)",
    "% Viability",
    "NC-200 Fold Expansion"
    #"DAPIneg (%)",
    #"ACR+/CD56+ (%)"
  )
  
  met_plots <- list(
    "Metaflex pH",
    "Metaflex Lactate (mM/L)",
    "Metaflex Glucose (mg/dL)",
    "Metaflex pO2 (%)"
  )
  
  phenotype_plots <- list("DAPIneg (%)",
                          "ACR+/CD56+ (%)"
                          # "NKG2A+/CD16- (%)",
                          # "NKG2A+/CD16+ (%)",
                          # "NKG2A-/CD16+ (%)"
  )
  
  output$growth <- renderPlot({
    req(input$refresh)
    isolate({
      plots <- lapply(seq_len(length(growth_plots)),function(i){
        if (is.null(new_df()[['combined_line']]) == FALSE)
          p <- plot_pointswithline(data = isolate(filtered_data()), x = "Day", y = growth_plots[[i]], groupby="combined_line",colorby=isolate(input$selectinput_color),shapeby=isolate(input$selectinput_shape),panelby=isolate(input$selectinput_panel))
        else
          p<- plot_pointswithline(data = isolate(filtered_data()), x = "Day", y = growth_plots[[i]], groupby="NULL",colorby=isolate(input$selectinput_color),shapeby=isolate(input$selectinput_shape),panelby=isolate(input$selectinput_panel))
        
        p <- p + scale_x_continuous(breaks = integer_breaks())
        p <- p + scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
      })
      ggarrange(plotlist = plots, common.legend = TRUE, legend="right")
    })
  })
  
  output$metabolites <- renderPlot({
    req(input$refresh)
    isolate({
      plots <- lapply(seq_len(length(met_plots)),function(i){
        if (is.null(new_df()[['combined_line']]) == FALSE)
          p <- plot_pointswithline(data = isolate(filtered_data()), x = "Day", y = met_plots[[i]], groupby="combined_line",colorby=isolate(input$selectinput_color),shapeby=isolate(input$selectinput_shape),panelby=isolate(input$selectinput_panel))
        else
          p<- plot_pointswithline(data = isolate(filtered_data()), x = "Day", y = met_plots[[i]], groupby="NULL",colorby=isolate(input$selectinput_color),shapeby=isolate(input$selectinput_shape),panelby=isolate(input$selectinput_panel))
        
        p <- p + scale_x_continuous(breaks = integer_breaks())
        p <- p + scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
      })
      ggarrange(plotlist = plots, common.legend = TRUE, legend="right")
    })
  })
  
  output$phenotype <- renderPlot({
    req(input$refresh)
    isolate({
      plots <- lapply(seq_len(length(phenotype_plots)),function(i){
        if (is.null(new_df()[['combined_line']]) == FALSE)
          p <- plot_pointswithline(data = isolate(filtered_data()), x = "Day", y = phenotype_plots[[i]], groupby="combined_line",colorby=isolate(input$selectinput_color),shapeby=isolate(input$selectinput_shape),panelby=isolate(input$selectinput_panel))
        else
          p<- plot_pointswithline(data = isolate(filtered_data()), x = "Day", y = phenotype_plots[[i]], groupby="NULL",colorby=isolate(input$selectinput_color),shapeby=isolate(input$selectinput_shape),panelby=isolate(input$selectinput_panel))
        
        p <- p + scale_x_continuous(breaks = integer_breaks())
        p <- p + scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
      })
      ggarrange(plotlist = plots, common.legend = TRUE, legend="right")
    })
  })
  
  
  
  output$table <- renderTable({
    filtered_data()
  })
  # 
  # output$show_inputs <- renderTable({
  #   AllInputs()
  # })
  
  # output$show_inputs2 <- renderTable({
  #   current_inputs()
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
shinyApp(ui, server)
