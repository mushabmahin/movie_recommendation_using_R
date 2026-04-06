# ==============================
# 🎬 FAST MOVIE RECOMMENDER + SIMILARITY
# ==============================

library(shiny)
library(recommenderlab)
library(data.table)
library(reshape2)

# ==============================
# LOAD DATA
# ==============================
movie_data <- read.csv("IMDB-Dataset/movies.csv", stringsAsFactors = FALSE)
rating_data <- read.csv("IMDB-Dataset/ratings.csv")

# ==============================
# CREATE RATING MATRIX
# ==============================
ratingMatrix <- dcast(rating_data, userId ~ movieId, value.var = "rating")
ratingMatrix <- as.matrix(ratingMatrix[,-1])
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")

# ==============================
# 🔥 REDUCE SIZE (IMPORTANT)
# ==============================

# Keep only active users and popular movies
ratingMatrix <- ratingMatrix[
  rowCounts(ratingMatrix) > 50,
  colCounts(ratingMatrix) > 50
]

# ==============================
# BUILD MODEL (FAST)
# ==============================
model <- Recommender(ratingMatrix, method = "IBCF", parameter = list(k = 20))

# ==============================
# 🔥 FAST SIMILARITY (LIMITED)
# ==============================
subset_movies <- 1:min(500, ncol(ratingMatrix))
movie_similarity <- similarity(ratingMatrix[, subset_movies],
                               method = "cosine",
                               which = "items")

# ==============================
# UI
# ==============================
ui <- fluidPage(
  
  titlePanel("⚡ Fast Movie Recommender"),
  
  tabsetPanel(
    
    tabPanel("User Recommendation",
             sidebarLayout(
               sidebarPanel(
                 numericInput("user_id", "Enter User ID:", value = 1, min = 1),
                 actionButton("user_btn", "Recommend")
               ),
               mainPanel(tableOutput("user_results"))
             )),
    
    tabPanel("Movie Similarity",
             sidebarLayout(
               sidebarPanel(
                 textInput("movie_name", "Enter Movie Name:", "Toy Story (1995)"),
                 actionButton("movie_btn", "Find Similar")
               ),
               mainPanel(tableOutput("movie_results"))
             ))
  )
)

# ==============================
# SERVER
# ==============================
server <- function(input, output){
  
  # ----------------------------
  # USER RECOMMENDATION
  # ----------------------------
  observeEvent(input$user_btn,{
    
    preds <- predict(model, ratingMatrix, n = 10)
    
    if(input$user_id > length(preds@items)){
      res <- c("Invalid user ID")
    } else {
      items <- preds@items[[input$user_id]]
      ids <- preds@itemLabels[items]
      
      res <- sapply(ids, function(id){
        title <- movie_data$title[movie_data$movieId == as.numeric(id)]
        if(length(title) == 0) return(NA)
        return(title)
      })
    }
    
    output$user_results <- renderTable({
      data.frame(Recommendations = res)
    })
    
  })
  
  # ----------------------------
  # MOVIE SIMILARITY (FAST)
  # ----------------------------
  observeEvent(input$movie_btn,{
    
    movie_index <- which(movie_data$title == input$movie_name)
    
    if(length(movie_index) == 0){
      res <- c("Movie not found")
    } else if(movie_index > ncol(movie_similarity)){
      res <- c("Movie not in fast subset")
    } else {
      sim_scores <- movie_similarity[movie_index, ]
      top_movies <- order(sim_scores, decreasing = TRUE)[2:6]
      res <- movie_data$title[top_movies]
    }
    
    output$movie_results <- renderTable({
      data.frame(Similar_Movies = res)
    })
    
  })
}

# ==============================
# RUN APP
# ==============================
shinyApp(ui, server)