#sociomatrixapp #3 - a network display app using igraph
# need to add documentation, especially for the more involved output part
# code seems very brute-force, room for improvement for sure
# somewhat difficult to read
library(shiny)
library(igraph)
# How do we get the first two columns of graph.data.frame to not be treated as
# an "edge list"?

# can use source instead to call Sociomatrix.R, but must save it in the app's folder
wom <- read.csv("WOM3.csv", header = T, strip.white = T) # remove extra spaces in the middle with strip.white
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
sociodf[2] <- as.numeric(unlist(wom[3]))
colnames(sociodf)[2] <- "School"
sociodf[3:68] <- df[1:66]
sociodf[3] <- 0 #Thales Teixiera has no collaborators
for (i in 3:68) {
  colnames(sociodf)[i] <- ns[i-2]
} 
sociogdf <- graph.data.frame(sociodf) #turn the data frame into a graph object with i graph

n <- shinyUI(fluidPage(
  titlePanel("WOM Researchers and Collaborators"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selection", 
                  label = "Choose a name to display",
                  choices = sort(rownames(sociomatrix)),
                  selected = "Alessandro Peluso"),
      
      selectInput("selection2", 
                  label = "Choose a name to display",
                  choices = sort(rownames(sociomatrix)),
                  selected = "Alessandro Peluso"),
      
      selectInput("selection3", 
                  label = "Choose a name to display",
                  choices = sort(rownames(sociomatrix)),
                  selected = "Alessandro Peluso")
      
    ),
    mainPanel(
      textOutput("text1"),
      textOutput("text2"),
      br(),
      textOutput("text1.2"),
      textOutput("text2.2"),
      br(),
      textOutput("text1.3"),
      textOutput("text2.3"),
      br(),
      br(),
      plotOutput("network")
    )
  )
))

d <- shinyServer(
  function(input, output) {
    output$text1 <- renderText({
      paste("The collaborators of", input$selection, "are:")
    })
    
    output$text2 <- renderText({
      paste(colnames(sociomatrix)[sociomatrix[input$selection,] == 1], ",")
    })
    
    output$text1.2 <- renderText({
      paste("The collaborators of", input$selection2, "are:")
    })
    
    output$text2.2 <- renderText({
      paste(colnames(sociomatrix)[sociomatrix[input$selection2,] == 1], ",")
    })
    
    output$text1.3 <- renderText({
      paste("The collaborators of", input$selection3, "are:")
    })
    
    output$text2.3 <- renderText({
      paste(colnames(sociomatrix)[sociomatrix[input$selection3,] == 1], ",")
    })
    
    output$network <- renderPlot({
      sociodfFirst2 <- sociodf[input$selection2,1]
      sociodfReduced2 <- colnames(sociodf)[which(sociodf[input$selection2,] == 1)]
      combined2 <- (merge(sociodfFirst2,sociodfReduced2))
      #row.names(combined2) <- 1:length(row.names(combined2))
      combined2[,1] <- as.character(combined2[,1])
      combined2[,2] <- as.character(combined2[,2])
      print(combined2)
      sociodfFirst3 <- sociodf[input$selection3,1]
      sociodfReduced3 <- colnames(sociodf)[which(sociodf[input$selection3,] == 1)]
      combined3 <- (merge(sociodfFirst3,sociodfReduced3))
      #row.names(combined3) <- 1:length(row.names(combined3))
      combined3[,1] <- as.character(combined3[,1])
      combined3[,2] <- as.character(combined3[,2])
      print(combined3)
      sociodfFirst <- sociodf[input$selection,1]
      sociodfReduced <- colnames(sociodf)[which(sociodf[input$selection,] == 1)]
      print("1")
      combined <- (merge(sociodfFirst,sociodfReduced))
      print(combined)
      combined <- unique(combined)
      print("#1")
      print("2")
      #row.names(combined) <- 1:length(row.names(combined))
      combined[,1] <- as.character(combined[,1])
      combined[,2] <- as.character(combined[,2])
      # can't just be 5-8 needs to be the length of combined2 and combined3
      print("3")
      combined[(length(rownames(unique(combined)))+1):(length(rownames(unique(combined2)))+length(rownames(unique(combined)))),] <- unique(combined2)
      print(combined)
      print("#2")
      combined[(length(rownames(unique(combined)))+1):(length(rownames(unique(combined3)))+length(rownames(unique(combined)))),] <- unique(combined3)
      print("4")
      print(combined)
      print("#3")
      print("5")
      sociogdf <- graph.data.frame(unique(combined))  #turns the df into a data frame with edges that can be read by graph.data.frame
      # unique is important,it prevents errors and makes sure the arrows are only drawn once when one professor is selected in all three inputs
      print("6")
      plot(sociogdf)
    })
  })
shinyApp(ui = n, server = d)
