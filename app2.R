#sociomatrixapp #3 - a network display app using igraph

library(shiny)
library(igraph)

# can use source instead to call Sociomatrix.R, but must save it in the app's folder
wom <- read.csv("WOM3.csv", header = T, strip.white = T) # remove extra spaces in the middle with strip.white
ns <- paste(as.character(wom$First.Name),as.character(wom$Last.Name))
names <- list(ns)
Collaborators <- as.character(wom$Collaborators)
wom$School <- as.character(wom$School)

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
      sociodfReduced <- colnames(sociodf)[which(sociodf[input$selection,] == 1)]
      if (identical(sociodfReduced,character(0)) | identical(sociodfReduced,"School")) {
        paste(input$selection, " does not have any collaborators.")
      }
      else {paste(colnames(sociomatrix)[sociomatrix[input$selection,] == 1], ",")}
    })
    
    output$text1.2 <- renderText({
      paste("The collaborators of", input$selection2, "are:")
    })
    
    output$text2.2 <- renderText({
      sociodfReduced <- colnames(sociodf)[which(sociodf[input$selection2,] == 1)]
      if (identical(sociodfReduced,character(0)) | identical(sociodfReduced,"School")){
        paste(input$selection2, " does not have any collaborators.")
      }
      else {paste(colnames(sociomatrix)[sociomatrix[input$selection2,] == 1], ",")}
    })
    
    output$text1.3 <- renderText({
      paste("The collaborators of", input$selection3, "are:")
    })
    
    output$text2.3 <- renderText({
      sociodfReduced <- colnames(sociodf)[which(sociodf[input$selection3,] == 1)]
      if (identical(sociodfReduced,character(0)) | identical(sociodfReduced,"School")) {
        paste(input$selection3, " does not have any collaborators.")
      }
      else {paste(colnames(sociomatrix)[sociomatrix[input$selection3,] == 1], ",")}
    })
    
    output$network <- renderPlot({
      sociodfFirst2 <- sociodf[input$selection2,1]
      sociodfReduced2 <- colnames(sociodf)[which(sociodf[input$selection2,] == 1)]
      combined2 <- (merge(sociodfFirst2,sociodfReduced2))
      combined2[,1] <- as.character(combined2[,1])
      combined2[,2] <- as.character(combined2[,2])
      sociodfFirst3 <- sociodf[input$selection3,1]
      sociodfReduced3 <- colnames(sociodf)[which(sociodf[input$selection3,] == 1)]
      combined3 <- (merge(sociodfFirst3,sociodfReduced3))
      combined3[,1] <- as.character(combined3[,1])
      combined3[,2] <- as.character(combined3[,2])
      sociodfFirst <- sociodf[input$selection,1]
      sociodfReduced <- colnames(sociodf)[which(sociodf[input$selection,] == 1)]
      combined <- (merge(sociodfFirst,sociodfReduced))
      combined <- unique(combined)
      combined[,1] <- as.character(combined[,1])
      combined[,2] <- as.character(combined[,2])
      combined.all <- rbind(combined, combined2, combined3)
      if (is.null(rownames(combined.all))) {}
      else{
        sociogdf <- graph.data.frame(unique(combined.all))  #turns the df into a data frame with edges that can be read by graph.data.frame
        plot(sociogdf)
      }
    })
  })
shinyApp(ui = n, server = d)
