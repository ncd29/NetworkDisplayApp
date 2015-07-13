#MatrixApp3.0 - first version of a functioning network analysis
# Example 3
library(shiny)
library(igraph)

# can use source instead to call Sociomatrix.R, but must save it in the app's folder
wom <- read.csv("WOMResearchers.csv", header = T, strip.white = T) # remove extra spaces in the middle with strip.white
ns <- paste(as.character(wom$First.Name),as.character(wom$Last.Name))
names <- list(ns)
Collaborators <- as.character(wom$Collaborators)

# for all researchers, create a vector of all zeros, and change to a 1
# if they have collaborated with a researcher in the corresponding column
l <- list()
for (n in 1:66) {
  if (!is.na(Collaborators[n])) (Collaborators[[n]] <- strsplit(as.character(Collaborators[n]),", "))
  v <- vector("numeric",length = length(ns))
  for (m in 1:66) {
    if (ns[m]%in%Collaborators[[n]][[1]]) { #if the column researcher is a collaborator of the row researcher
      v[m] = 1
    }
  }
  l[[n]] <- v
}

# turn the list into a matrix
sociomatrix <- matrix(unlist(l),nrow = length(ns), ncol = length(ns), dimnames = names)
colnames(sociomatrix) <- ns
# end Sociomatrix.R

#turn the matrix into a graph data frame
#add the first two columns
df <- as.data.frame(sociomatrix)
sociodf <- as.data.frame(sociomatrix)
sociodf[1] <- ns
colnames(sociodf)[1] <- "Name"
# some schools are NA
sociodf[2] <- as.numeric(unlist(wom[3])) 
colnames(sociodf)[2] <- "School"
sociodf[3:68] <- df[1:66]
sociodf[3] <- 0 #Thales Teixiera has no collaborators
for (i in 3:68) {
  colnames(sociodf)[i] <- ns[i-2]
} 
sociogdf <- graph.data.frame(sociodf) #turn the data frame into a graph object with i graph

ui <- shinyUI(fluidPage(
  titlePanel("WOM Researchers and Collaborators"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selection", 
                  label = "Choose a name to display",
                  choices = sort(rownames(sociomatrix)),
                  selected = "Alessandro Peluso")
    ),
    mainPanel(
      textOutput("text1"),
      textOutput("text2"),
      br(),
      plotOutput("network")
    )
  )
))

server <- shinyServer(
  function(input, output) {
output$text1 <- renderText({
  paste("The collaborators of", input$selection, "are:")
})

output$text2 <- renderText({
  sociodfReduced <- colnames(sociodf)[which(sociodf[input$selection,] == 1)]
  if (identical(sociodfReduced,character(0))) {
    paste(input$selection, " does not have any collaborators.")
  }
  else {paste(colnames(sociomatrix)[sociomatrix[input$selection,] == 1], ",")}
})

output$network <- renderPlot({
  sociodfFirst <- sociodf[input$selection,1]
  sociodfReduced <- colnames(sociodf)[which(sociodf[input$selection,] == 1)]
  if (identical(sociodfReduced,character(0))) {
  }
  else{
    combined <- merge(sociodfFirst,sociodfReduced)
    sociogdf <- graph.data.frame(combined)
    plot(sociogdf)
  }
  })
})
shinyApp(ui = ui, server = server) #run the app with shinyApp()
# not needed if ui and server are in their own .R files
