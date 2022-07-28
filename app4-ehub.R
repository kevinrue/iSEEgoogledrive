library(ExperimentHub)

# lpfun ----

lpfun <- function() {
    ehub <- ExperimentHub()
    
    datasets_available_table <- as.data.frame(mcols(ehub))
    
    se_load <- function(x) {
        stop("TODO")
    }
    
    function (FUN, input, output, session) {
        .ui_dataset_table <- "datasets_table"
        .ui_launch_button <- "launch"
        .dataset_selected_index <- paste0(.ui_dataset_table, "_rows_selected")
        .ui_dataset_columns <- "datasets_columns"
        
        # nocov start
        output$allPanels <- renderUI({
            tagList(
                fluidRow(
                    column(width = 8L, shinydashboard::box(title = "ExperimentHub",
                      collapsible = FALSE, width = NULL,
                        selectInput(inputId = .ui_dataset_columns, label = "Show columns:", choices = colnames(datasets_available_table), selected = c("title", "dataprovider", "species"), multiple = TRUE),
                        DTOutput(.ui_dataset_table)
                    ))
                ),
                fluidRow(column(width = 12L, actionButton(.ui_launch_button, label="Launch", style="color: #ffffff; background-color: #0092AC; border-color: #2e6da4")))
            )
        })
        # nocov end
        
        pObjects <- new.env()
        rObjects <- reactiveValues(rerender_datasets=1L)
        
        observeEvent(input[[.dataset_selected_index]], {
            pObjects[[.dataset_selected_index]] <- input[[.dataset_selected_index]]
            
        }, ignoreInit = FALSE, ignoreNULL = FALSE)
        
        observeEvent(input[[.ui_dataset_columns]], {
            pObjects[[.ui_dataset_columns]] <- input[[.ui_dataset_columns]]
            rObjects$rerender_datasets <- iSEE:::.increment_counter(isolate(rObjects$rerender_datasets))
        })
        
        output[[.ui_dataset_table]] <- DT::renderDT({
            force(rObjects$rerender_datasets)
            datasets_table_visible <- datasets_available_table[, pObjects[[.ui_dataset_columns]]]
            DT::datatable(datasets_table_visible, filter="top", rownames=FALSE,
            options=list(
                search=list(search="", smart=FALSE, regex=TRUE, caseInsensitive=FALSE),
                searchCols=c(list(NULL), list(NULL)), # row names are the first column!
                scrollX=TRUE,
                columnDefs=NULL),
            selection=list(mode = 'single', selected=1L, target = 'row'))
        })

        # nocov start, ignoreNULL=TRUE, ignoreInit=TRUE
        observeEvent(input[[.ui_launch_button]], {
            se2 <- try(se_load(pObjects[[.dataset_selected_index]]))
            if (is(se2, "try-error")) {
                showNotification("invalid SummarizedExperiment supplied", type="error")
            } else {
                # init <- try(initLoad(input[[.initializeInitial]]))
                # if (is(init, "try-error")) {
                #     showNotification("invalid initial state supplied", type="warning")
                #     init <- NULL
                # }
                # init <- list(ReducedDimensionPlot())
                init <- NULL
                FUN(SE=se2, INITIAL=init)
            }
        }, ignoreNULL=TRUE, ignoreInit=TRUE)
        # nocov end

        invisible(NULL)
    }
}

app <- iSEE(landingPage=lpfun())
if (interactive()) {
  shiny::runApp(app, port=1234, launch.browser = TRUE)
}
