library(iSEE)
library(DT)
library(BiocFileCache)

# load_datasets ----

load_datasets <- function(dir = "datasets") {
    dataset_files <- list.files(path = dir, pattern = "*.yaml", full.names = TRUE)
    dataset_infos <- lapply(dataset_files, function(x){ yaml::read_yaml(x) })
    names(dataset_infos) <- as.character(lapply(dataset_infos, function(x) { x$id }))
    return(dataset_infos)
}

datasets_available <- load_datasets()
str(datasets_available)

tabulate_datasets <- function(list) {
    data.frame(
        id = names(datasets_available)
    )
}

datasets_available_table <- tabulate_datasets(datasets_available)
datasets_available_table

# lpfun ----

lpfun <- function() {
    bfc <- BiocFileCache()
    
    se_load <- function(x) {
        dataset_id <- datasets_available[x, "id"]
        bioc_rpath <- bfcquery(x = bfc, query = dataset_id, field = "rname")[1, "rpath", drop=TRUE]
        if (is.na(bioc_rpath)) {
            bioc_rpath <- bfcadd(x = bfc, rname = datasets_available[[x]][["id"]], fpath = datasets_available[[x]][["url"]])
        }
        readRDS(bioc_rpath)
    }
    
    function (FUN, input, output, session) {
        .ui_dataset_table <- "datasets_table"
        .ui_launch_button <- "launch"
        .dataset_selected_index <- paste0(.ui_dataset_table, "_rows_selected")
        
        # nocov start
        output$allPanels <- renderUI({
            tagList(
                fluidRow(
                    column(width = 6L, DTOutput(.ui_dataset_table)),
                    column(width = 6L, shinydashboard::box(title = "Overview",
                      collapsible = FALSE, width = NULL, uiOutput("TODO")
                    ))
                ),
                fluidRow(column(width = 12L, actionButton(.ui_launch_button, label="Launch", style="color: #ffffff; background-color: #0092AC; border-color: #2e6da4")))
            )
        })
        # nocov end
        
        launcherObjects <- new.env()
        
        output[["TODO"]] <- renderUI({
            if (is.null(input[[.dataset_selected_index]])) {
                contents <- markdown("")
            } else {
                contents <- markdown(datasets_available[[input[[.dataset_selected_index]]]][["summary"]])
            }
            contents
        })
        
        observeEvent(input[[.dataset_selected_index]], {
            launcherObjects[[.dataset_selected_index]] <- input[[.dataset_selected_index]]
            
        }, ignoreInit = FALSE, ignoreNULL = FALSE)
        
        output[[.ui_dataset_table]] <- DT::renderDT({
            DT::datatable(datasets_available_table, filter="top", rownames=TRUE,
            options=list(
                search=list(search="", smart=FALSE, regex=TRUE, caseInsensitive=FALSE),
                searchCols=c(list(NULL), list(NULL)), # row names are the first column!
                scrollX=TRUE,
                columnDefs=NULL),
            selection=list(mode = 'single', selected=1L, target = 'row'))
        }, server = FALSE)

        # nocov start, ignoreNULL=TRUE, ignoreInit=TRUE
        observeEvent(input[[.ui_launch_button]], {
            se2 <- try(se_load(launcherObjects[[.dataset_selected_index]]))
            if (is(se2, "try-error")) {
                showNotification("invalid SummarizedExperiment supplied", type="error")
            } else {
                # init <- try(initLoad(input[[.initializeInitial]]))
                # if (is(init, "try-error")) {
                #     showNotification("invalid initial state supplied", type="warning")
                #     init <- NULL
                # }
                init <- list(ReducedDimensionPlot())
                # init <- NULL
                FUN(SE=se2, INITIAL=init)
            }
        }, ignoreNULL=TRUE, ignoreInit=TRUE)
        # nocov end

        invisible(NULL)
    }
}

app <- iSEE(landingPage=lpfun())
if (interactive()) {
  shiny::runApp(app, port=1234) # , launch.browser = TRUE
}
