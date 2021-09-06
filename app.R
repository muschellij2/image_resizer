#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(magick)
library(shinyjs)

options(shiny.maxRequestSize = 1024^4)
local_run = Sys.info()["user"] == "johnmuschelli"

# logo <- image_read("logo:")
# sacler =
ui = function(input)
    # Define UI for application that draws a histogram
    ui <- fluidPage(
        useShinyjs(),
        # Application title
        titlePanel("Process images"),

        # Sidebar with a slider input for number of bins
        sidebarLayout(
            sidebarPanel(
                shiny::fileInput("zipfile", "Upload Zip file:", multiple = FALSE,
                                 accept = mime::guess_type("blah.zip")
                ),
                downloadButton("download", label = "Download")
                # ,
                # actionButton('process_data', "Reprocess Files")
            ),

            # Show a plot of the generated distribution
            mainPanel(
                textOutput("progresser")
            )
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {

    disable("download")
    # disable("process_data")
    values = reactiveValues()
    values$output_directory = ifelse(local_run, tempfile(), ".")

    get_all_files = reactive({
        values$zipfile = input$zipfile$datapath
        zipfile = values$zipfile
        print(input)
        dir.create(values$output_directory, recursive = TRUE, showWarnings = FALSE)
        if (!is.null(zipfile) && file.exists(zipfile)) {
            files = unzip(zipfile = zipfile,
                          exdir = values$output_directory,
                          overwrite = TRUE,
                          list = TRUE)
            files = files$Name
            files = files[!grepl("__MACOSX", files)]
            files = files[grepl("(jpg|png|jpeg|bmp|tiff|tif)$",
                                files, ignore.case = TRUE)]
            # save some space
        } else {
            files = NULL
        }
        files
    })


    process_file = function(path, full_path, exdir, zipfile) {
        if (!file.exists(full_path)) {
            unzip(zipfile = zipfile,
                  files = path,
                  exdir = exdir,
                  overwrite = TRUE)
        }
        img = magick::image_read(full_path, density = 150)
        img = image_scale(img, geometry = geometry_size_pixels(height = 1029))
        image_write(image = img,
                    path = full_path,
                    quality = 100,
                    density =  150)
        rm(img); gc()
        return(NULL)
    }


    output$progresser <- renderText({
        print("in render text")
        print(input$zipfile)
        validate(need(input$zipfile$datapath, label = "Need to Upload a Zip file"))
        # enable("process_data")

        # input$process_data # Re-run when button is clicked
        files = get_all_files()
        values$files = files
        values$full_files = file.path(values$output_directory, files)

        withProgress(message = 'Processing file', value = 0, {
            # Number of times we'll go through the loop
            n <- length(values$files)

            for (i in seq_along(values$files)) {
                # Each time through the loop, add another row of data. This is
                # a stand-in for a long-running computation.

                # Increment the progress bar, and update the detail text.
                incProgress(1/n, detail = values$files[i])

                # Pause for 0.1 seconds to simulate a long computation.
                process_file(
                    path = values$files[i],
                    full_path = values$full_files[i],
                    zipfile = values$zipfile,
                    exdir = values$output_directory)
            }
        })
        enable("download")
        "Now you can Download your Data!"
    })



    output$download <- downloadHandler(
        filename = function() {
            "processed_files.zip"
        },
        content = function(fname) {
            print(fname)
            browser()
            stopifnot(all(file.exists(values$full_files)))
            print(head(values$files))
            print(head(values$full_files))
            print(head(values$output_directory))
            owd = getwd()
            setwd(values$output_directory)
            utils::zip(
                zipfile = fname,
                files = values$files
            )
            setwd(owd)
            fname
        },
        contentType = "application/zip"
    )
}

# Run the application
shinyApp(ui = ui, server = server)
