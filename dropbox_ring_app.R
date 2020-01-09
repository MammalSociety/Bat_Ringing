
#Current issues: 
# 1) csv it outputs has no column headers
# 2) It's saving three versions of the same csv to dropbox
# 3) On Shiny.io doesn't let you submit, assume can't access dropbox
# 4) Need to get Ecobat logo on the form, ui/www files?


library(shiny)
library(shinyjs)
library(DT)
library(dplyr)
library(rdrop2)

##################################################################

#Use the following hashed lines to get and saave the droptoken 
#from dropbox:

#token <- drop_auth()
#saveRDS(token, "droptoken.rds")

###################################################################

#Access to dropbox account where it will save the data
token <- readRDS("droptoken.rds")


# define mandatory fields
fieldsMandatory <- c("Name", "Location", "Species", "Ring Colour", "Ring Location", "Email", "permission")

# add asterisk to mandatory fields
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# make asterisk red
appCSS <- ".mandatory_star { color: red; }
#error { color: red; }"


## define what happens to save the user's response ##

fieldsAll <- c("Name", "Organisation", "Location", "Species", "Ring Colour", "Ring Location", "Email", "Notes")
responsesDir <- file.path("responses")
epochTime <- function() {
  as.integer(Sys.time())
}

# this isn't working...
# humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")


# define function to retrieve all previous submissions
loadData <- function() {
  files <- list.files(file.path(responsesDir), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  data <- dplyr::rbind_all(data)
  data
}


#### shiny app ####

shinyApp(
  ui = fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    titlePanel("Bat Colour Ringing"),
    DT::dataTableOutput("responsesTable"),
    div(
      id = "form",
      
      textInput("Name", labelMandatory("Name"), ""),
      textInput("Organisation", "Organisation", ""),
      textInput("Location", labelMandatory("Location"), ""),
      textInput("Species", labelMandatory("Species"), ""),
      textInput("Ring Colour", labelMandatory("Ring colour"), ""),
      textInput("Ring Location", labelMandatory("Ring location"), ""),
      textInput("Email", labelMandatory("Email"), ""),
      textInput("Notes", "Notes", ""),
      checkboxInput("permission",
                    "My submitting this form I agree to displaying 
                    my email address", FALSE),
      actionButton("submit", "Submit", class = "btn-primary"),
      shinyjs::hidden(
        span(id = "submit_msg", "Submitting..."),
        div(id = "error",
            div(br(), tags$b("Error: "), span(id = "error_msg"))
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = "thankyou_msg",
        h3("Thanks, your response was submitted successfully!"),
        actionLink("submit_another", "Submit another response")
      )
    ) 
  ),  
  
  
  server = function(input, output, session) {
    
    observe({
      # check if all mandatory fields have a value
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      
      # enable/disable the submit button
      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })
    formData <- reactive({
      data <- sapply(fieldsAll, function(x) input[[x]])
      data <- c(data, timestamp = epochTime())
      data <- t(data)
      data
    })
    
    # save the data
    
    outputDir <- "responses"
    
    saveData <- function(data) {
      data <- t(data)
      # Create a unique file name
      fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
      # Write the data to a temporary file locally
      filePath <- file.path(tempdir(), fileName)
      write.csv(data, filePath, row.names = FALSE, quote = TRUE)
      # Upload the file to Dropbox
      drop_upload(filePath, path = outputDir)
    }
    
    loadData <- function() {
      # Read all the files into a list
      filesInfo <- drop_dir(outputDir)
      filePaths <- filesInfo$path_display
      data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
      # Concatenate all data together into one data.frame
      data <- do.call(rbind, data)
      data
    }
    
    # action to take when submit button is pressed
    observeEvent(input$submit, {
      saveData(formData())
    })
    
    
    observeEvent(input$submit, {
      saveData(formData())
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    })
    
    observeEvent(input$submit_another, {
      shinyjs::show("form")
      shinyjs::hide("thankyou_msg")
    }) 
    
    observeEvent(input$submit, {
      shinyjs::disable("submit")
      shinyjs::show("submit_msg")
      shinyjs::hide("error")
      
      tryCatch({
        saveData(formData())
        shinyjs::reset("form")
        shinyjs::hide("form")
        shinyjs::show("thankyou_msg")
      },
      error = function(err) {
        shinyjs::html("error_msg", err$message)
        shinyjs::show(id = "error", anim = TRUE, animType = "fade")
      },
      finally = {
        shinyjs::enable("submit")
        shinyjs::hide("submit_msg")
      })
    })
    
    output$responsesTable <- DT::renderDataTable(
      loadData(),
      rownames = FALSE,
      options = list(searching = FALSE, lengthChange = FALSE)
    ) 
  }
)
