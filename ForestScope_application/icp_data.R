library(shiny)
library(shinyFiles)
library(shinyWidgets)

dirInput <- function(inputId, label) {
  tagList(
    tags$label(label, `for` = inputId),
    tags$input(id = inputId, type = "file", webkitdirectory = TRUE, directory = TRUE)
  )
}

ui <- shinyUI(
  fluidPage(
    navbarPage("ICP and CHELSA data upload",
               tabPanel("Description",
                        img(src = "https://www.unwater.org/sites/default/files/styles/d02/public/app/uploads/2020/01/220x120_PARTNERS_IIASA.webp?itok=MjAJX7xP", height = 100),
                        p("This application allows you to upload ICP Forest and CHELSA Climate data for further analysis. ICP Forest data provides valuable information on the effects of air pollution on forests, while CHELSA Climate data offers high-resolution climate data for environmental research."),
                        p("Once you have uploaded all the necessary files, click the 'Process Data' button to begin the analysis."),
                        p("For more information about the datasets, please visit:",
                          a("ICP Forest Data", href = "https://www.icp-forests.org"),
                          "and",
                          a("CHELSA Climate Data", href = "https://chelsa-climate.org")
                        ),
                        br(),
                        actionButton("process_data", "Process Data"),
                        br(),
                        actionLink("back_home", "Back to Home Page")
               ),
               tabPanel("ICP Data",
                        
                       dirInput("ICP Climate Data folder", "Select a folder")
               ),
               tabPanel("ICP Soil Data",
                        fileInput("so_pfh", "ICP so_pfh file", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                        fileInput("so_som", "ICP so_som file", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                        fileInput("ss_ssm", "ICP so_som file", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
               ),
               tabPanel("CHELSA Data",
                        checkboxInput("use_CHELSA", "Use CHELSA Climate Data as Alternative?", value = FALSE),
                        conditionalPanel(
                          condition = "input.use_CHELSA == true",
                          fileInput("chelsa_pr", "Select CHELSA PR file", multiple = FALSE),
                          fileInput("chelsa_tas", "Select CHELSA TAS file", multiple = FALSE)
                        )
               ),
               tabPanel("ICP Stand Data",
                        fileInput("si_sta", "ICP si_sta file", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                        fileInput("gr_ipm", "ICP gr_ipm file", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                        fileInput("gr_iev", "ICP gr_iev file", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                        fileInput("gr_inv", "ICP gr_inv file", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                        fileInput("lf_lfm", "ICP lf_lfm file", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
               ))
              
  )
)







server<-shinyServer(function(input, output) {
 
})

shinyApp(ui = ui, server = server)




