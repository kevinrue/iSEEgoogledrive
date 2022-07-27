library(iSEE)
library(DT)
library(BiocFileCache)

datasets_available <- rbind(
    data.frame(
        id = "zenodo.org/record/5196144/files/sce_hnscc.rds",
        url = "https://zenodo.org/record/5196144/files/sce_hnscc.rds?download=1"
        ),
    data.frame(
        id = "zenodo.org/record/3594331/files/sce_hgsc_annotated_final.rds",
        url = "https://zenodo.org/record/3594331/files/sce_hgsc_annotated_final.rds?download=1"
        )
)

bfc <- BiocFileCache()

# lpfun ----

se_load <- function(x) {
        dataset_index <- which(datasets_available$id == x)
        bioc_rpath <- bfcquery(x = bfc, query = x, field = "rname")[1, "rpath", drop=TRUE]
        message("bioc_rpath: ", bioc_rpath)
        if (is.na(bioc_rpath)) {
            bioc_rpath <- bfcadd(x = bfc, rname = x, fpath = datasets_available$url[dataset_index])
        }
        message("bioc_rpath: ", bioc_rpath)
        readRDS(bioc_rpath)
    }

lpfun <- createLandingPage(
    seUI=function(id) selectInput(id, "Dataset:", choices=datasets_available$id),
    seLoad=se_load,
    initLoad=function(x) list(ReducedDimensionPlot())
)

app <- iSEE(landingPage=lpfun)
if (interactive()) {
  shiny::runApp(app, port=1234) # , launch.browser = TRUE
}
