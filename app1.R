library(iSEE)
library(BiocFileCache)

# ?createLandingPage

# googledrive::drive_download(file = as_id("1RQRBKzggV5vzNes-LlJhwYwozaC0ZDen"), path = "data/tidyverse/iris.csv", verbose = TRUE)

datasets_available <- read.table("datasets.txt", header = TRUE)

bfc <- BiocFileCache()

se_load <- function(x) {
    message("se_load")
    message("x:")
    print(x)
        bioc_rpath <- bfcquery(x = bfc, query = "zenodo.org/record/5196144", field = "rname")[1, "rpath", drop=TRUE]
        if (!length(bioc_rpath)) {
            bioc_rpath <- bfcadd(x = bfc, rname = datasets_available[1, "id"], fpath = datasets_available[1, "url"])
        }
        message("bioc_rpath:")
        print(bioc_rpath)
        readRDS(bioc_rpath)
    }

lpfun <- createLandingPage(
    seUI=function(id) DT::datatable(data = datasets_available),
    seLoad=se_load
)

app <- iSEE(landingPage=lpfun)
if (interactive()) {
  shiny::runApp(app, port=1234, launch.browser = TRUE)
}
