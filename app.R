library("shiny")
library("igraph")
library("rsconnect")

ui <- fluidPage(
  titlePanel("RIPS Autograder"),
  sidebarLayout(
  sidebarPanel(
  textInput("numQuestions", "How many questions are in RIPS?")),
  mainPanel(
  uiOutput("dependBoxes"),
  imageOutput("graphPlot"),
  HTML("<br><br><br>"),
  textOutput("valid")),
  )
)

server <- function(input, output) {
  output$dependBoxes <- renderUI({
    numQuestions <- as.integer(input$numQuestions)
    lapply(1:numQuestions, function(i) {
      textInput(inputId = paste0("proofsUsed", i), paste("Write out the theorems/axioms used in problem", i, " as a space-separated list."))
    })
  })

  
  graph <- reactive({
    adjlist <- list(input$numQuestions)
    numAxioms <- 0
    for (i in 1:input$numQuestions) {
      # print("A")
      # print(input[[paste0("proofsUsed",i)]])
      # print("B")
      # print(strsplit(input[[paste0("proofsUsed",i)]], split = " "))
      # print("C")
      # print(unlist(strsplit(input[[paste0("proofsUsed",i)]], split = " ")))
      # print("D")
      # print(as.integer(unlist(strsplit(input[[paste0("proofsUsed",i)]], split = " "))))
      # print("E")
      # print(adjlist)
      adjlist[[i]] <- as.integer(unlist(strsplit(input[[paste0("proofsUsed",i)]], split = " ")))
      if (length(adjlist[[i]]) == 0){
        numAxioms <- numAxioms + 1
      }
    }
    #print(adjlist)
    graph <- graph_from_adj_list(adjlist, mode = "in")
    png("plot.png", 500, 500)
    plot.igraph(graph)
    dev.off()
    return(graph)
    })
  
  output$graphPlot <- renderImage({
    graph()
    list(src = "plot.png", contentType = 'image/png',width = 500, height = 500,
                                     alt = "This is alternate text", deleteFile = TRUE)})
  
  output$valid <- renderText({
    graph <- graph()
    if (!is_dag(graph)){
      print("This is not a valid system.")
    }else{
      adjlist <- get.adjlist(graph, mode = "in")
      numAxioms <- 0
      for (i in 1:length(adjlist)){
        if (length(adjlist[[i]]) == 0)
          numAxioms <- numAxioms + 1
      }
      print(paste("This is a valid system with ", numAxioms, " axioms."))
    }
  })
  
  
}

shinyApp(ui = ui, server = server)