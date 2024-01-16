
model_ui <- fluidPage(
  navbarPage(
    tabPanel("",
             fluidRow(
               column(width = 12, align = "center",
                      img(src = "https://www.unwater.org/sites/default/files/styles/d02/public/app/uploads/2020/01/220x120_PARTNERS_IIASA.webp?itok=MjAJX7xP", height = 100)
               )
             ),
             br(),
             h4("IIASA-ICP r3PG Modified Model Simulation"),
             br(),
             p("This app utilizes a modified version of the ",
               a("r3PG vegetation model", href = "https://github.com/trotsiuk/r3PG"),
               " provided by ",
               a("IIASA", href = "https://www.iiasa.ac.at/"),
               ". The original r3PG model is a process-based model developed for simulating forest growth and stand dynamics. This modified version extends the original model to include the simulation of carbon, nitrogen, and phosphorus pools and fluxes in both aboveground and belowground components."),
             br(),
             p("To get started, make sure you selected the site and the site has all the required data (e.g. climate, soil, species and thinning)."),
             br(),
             fluidRow(
               column(width = 4, align = "center",
                      actionButton("run_model", icon = icon("cog"), "Run MODEL"),
                      br(),
                      actionLink("back_home", "Back to Home Page")
               ),
               column(width = 8, align = "center",
                      plotOutput("output_plot")
               )
             )
    )
  )
)

model_server <- function(input, output, session) {
  
  
  observeEvent(input$run_model, {
    out <- run_3PG(
      site = d_site,
      species = d_species,
      climate = d_climate,
      thinning = d_thinning,
      parameters = d_parameters,
      size_dist = d_sizeDist,
      settings = list(light_model = 2, transp_model = 2, phys_model = 2,
                      calculate_d13c = 0, soil_model=3, nut_lim=1),
      check_input = TRUE, df_out = FALSE)
    
    
  # Observe the 'run_model' action button
  output$output_plot <- renderPlot({
    isolate({
      # Run the r3PG function
      par(mfrow=c(2,4))
      plot(out[,,6,1]*0.1, main = 'GPP', xlab='Time', ylab = 'GPP - kg C m-2 mo-1',typ="l")
      plot(out[,,6,2]*0.1, main = 'NPP', xlab='Time', ylab = 'NPP - kg C m-2 mo-1',typ="l")
      plot(out[,1,11,10]*0.1,main='Total soil resp',xlab='Time', ylab='C - kg C m-2 mo-1',typ="l")
      plot(out[,1,11,11]*0.1,main='Total C',xlab='Time', ylab='C - kg C m-2',typ='l',lwd=2)
      plot(out[,1,11,12]*0.1,main='Total N',xlab='Time', ylab='N - kg N m-2',typ='l',lwd=2)
      plot(out[,1,11,13]*0.1,main='Total P',xlab='Time', ylab='P - kg P m-2',typ='l',lwd=2)
      plot(out[,1,11,14]*0.1,main='Inogranic N',xlab='Time', ylab='N - kg N m-2',typ='l',lwd=2)
      plot(out[,1,11,15]*0.1,main='Inogranic P',xlab='Time', ylab='P - kg P m-2',typ='l',lwd=2)
    })
  })
  })
}
shinyApp(ui = model_ui, server = model_server)

               