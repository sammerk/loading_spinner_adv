library(ggplot2)
library(shiny)
library(shinyjs)


mycss <- "
#plot-container {
position: relative;
}
#loading-spinner {
position: absolute;
left: 50%;
top: 50%;
z-index: -1;
margin-top: -33px;  /* half of the spinner's height */
margin-left: -33px; /* half of the spinner's width */
}
#plot.recalculating {
z-index: -2;
}
"



ui <- shinyUI(pageWithSidebar(
        
         headerPanel("renderSVG example"),
         sidebarPanel(
           sliderInput("obs", "Number of observations:",
                       min = 0, max = 1000,  value = 500),
           actionButton("plotbtn", "Plot", icon = icon("bar-chart-o", lib = "font-awesome"))
         ),
         mainPanel(
           useShinyjs(),
           tags$head(tags$style(HTML(mycss))),
           hidden(div(id = "plot-container",
               tags$img(src = "spinner.gif",
                        id = "loading-spinner"),
               imageOutput("plot_as_svg")))
           
         )
       ))



server <- function(input, output, session) {
  
            ## create .svg file of the plot and render it with renderImage()
            
             observeEvent(input$plotbtn,{
               show("plot-container")
             })
  
            list_of_plot <- eventReactive(input$plotbtn, {  
              width  <- session$clientData$output_plot_as_svg_width
              height <- session$clientData$output_plot_as_svg_height
              mysvgwidth <- width/96
              mysvgheight <- height/96
              
              # A temp file to save the output.
              # This file will be removed later by renderImage
              
              outfile <- tempfile(fileext='.svg')
              
              # Generate the svg
              #to see actually what will be plotted and compare 
              qplot(clarity, data=diamonds, fill=cut, geom="bar")
              #save the plot in a variable image to be able to export to svg
              image=qplot(clarity, data=diamonds[1:input$obs,], fill=cut, geom="bar", main = "ggplot as svg")
              #This actually save the plot in a image
              ggsave(file=outfile, plot=image, width=mysvgwidth, height=mysvgheight)
              
              # Return a list containing the filename
              list(src = normalizePath(outfile),
                   contentType = 'image/svg+xml',
                   width = width,
                   height = height,
                   alt = "My svg Histogram")
            })
            
            output$plot_as_svg <- renderImage({
              Sys.sleep(2)
              list_of_plot()
            })
            
          }

shinyApp(ui, server)