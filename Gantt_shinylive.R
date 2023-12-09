## Convert the shiny app into the assets for running the app in a browser

shinylive::export(
 appdir = "Gantt",
 destdir = "docs"
)

## Run the following in an R session to serve the app:
httpuv::runStaticServer("docs")
