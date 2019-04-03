ui <- fluidPage(
	headerPanel('Plot'),
	sidebarPanel(
		sliderInput(inputId = 'x',label='X', value = 1,min=1,max=3),
		sliderInput(inputId = 'y',label='Y', value = 1,min=1,max=3)
	),
	mainPanel(plotOutput('plot') )
)

server<-function(input,output) {
	a <- reactive({input$x})
	c <- reactive({input$y})
	output$plot <- renderPlot({plot(x(), y())})
}

shinyApp(ui = ui, server = server)