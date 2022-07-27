library(iSEE)
library(DT)
library(BiocFileCache)

# ?createLandingPage

# googledrive::drive_download(file = as_id("1RQRBKzggV5vzNes-LlJhwYwozaC0ZDen"), path = "data/tidyverse/iris.csv", verbose = TRUE)

datasets_available <- read.table("datasets.txt", header = TRUE)

# lpfun ----

lpfun <- function() {
    bfc <- BiocFileCache()
    
    se_load <- function(x) {
        dataset_id <- datasets_available[x, "id"]
        bioc_rpath <- bfcquery(x = bfc, query = dataset_id, field = "rname")[1, "rpath", drop=TRUE]
        if (is.na(bioc_rpath)) {
            bioc_rpath <- bfcadd(x = bfc, rname = datasets_available[x, "id"], fpath = datasets_available[x, "url"])
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
                fluidRow(column(width = 12L, DTOutput(.ui_dataset_table))),
                fluidRow(column(width = 12L, actionButton(.ui_launch_button, label="Launch", style="color: #ffffff; background-color: #0092AC; border-color: #2e6da4")))
            )
        })
        # nocov end
        
        launcherObjects <- new.env()
        
        observeEvent(input[[.dataset_selected_index]], {
            launcherObjects[[.dataset_selected_index]] <- input[[.dataset_selected_index]]
        }, ignoreInit = FALSE, ignoreNULL = FALSE)
        
        output[[.ui_dataset_table]] <- DT::renderDT({
            DT::datatable(datasets_available, filter="top", rownames=TRUE,
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
