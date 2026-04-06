# ==============================
# 🎬 FINAL MOVIE RECOMMENDER (UI UPGRADED)
# ==============================

library(shiny)
library(recommenderlab)
library(data.table)
library(ggplot2)
library(reshape2)
library(proxy)

# ==============================
# 📂 LOAD DATA
# ==============================
movie_data <- read.csv("IMDB-Dataset/movies.csv", stringsAsFactors = FALSE)
rating_data <- read.csv("IMDB-Dataset/ratings.csv")

# ==============================
# 🧩 CREATE RATING MATRIX
# ==============================
ratingMatrix <- dcast(rating_data, userId ~ movieId, value.var = "rating")
ratingMatrix <- as.matrix(ratingMatrix[,-1])
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")

# ==============================
# 🎭 GENRE ENCODING (FIXED)
# ==============================
movie_genre <- as.data.frame(movie_data$genres, stringsAsFactors = FALSE)
movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1], '[|]'), stringsAsFactors = FALSE)

list_genre <- c("Action","Adventure","Animation","Children","Comedy","Crime",
                "Documentary","Drama","Fantasy","Film-Noir","Horror","Musical",
                "Mystery","Romance","Sci-Fi","Thriller","War","Western")

genre_mat <- matrix(0, nrow(movie_data), length(list_genre))
colnames(genre_mat) <- list_genre

for(i in 1:nrow(movie_genre2)){
  for(j in 1:ncol(movie_genre2)){
    g <- trimws(movie_genre2[i,j])
    if(!is.na(g) && g %in% colnames(genre_mat)){
      col_index <- which(colnames(genre_mat) == g)
      if(length(col_index) > 0){
        genre_mat[i, col_index] <- 1
      }
    }
  }
}

genre_mat <- as.data.frame(genre_mat)

# ==============================
# 🤖 MODEL (IBCF)
# ==============================
model <- Recommender(ratingMatrix, method = "IBCF", parameter = list(k = 30))

# ==============================
# 🔥 HYBRID SIMILARITY
# ==============================
rating_subset <- ratingMatrix[, 1:min(500, ncol(ratingMatrix))]
movie_similarity <- similarity(rating_subset, method="cosine", which="items")

content_sim <- simil(as.matrix(genre_mat), method="cosine")

min_dim <- min(nrow(content_sim), nrow(movie_similarity))

hybrid_sim <- 0.7 * as.matrix(movie_similarity[1:min_dim,1:min_dim]) +
  0.3 * as.matrix(content_sim[1:min_dim,1:min_dim])

# ==============================
# 🎯 USER RECOMMENDATION
# ==============================
recommend_for_user <- function(user_index){
  
  preds <- predict(model, ratingMatrix, n = 10)
  
  if(user_index < 1 || user_index > length(preds@items)){
    return("Invalid user index")
  }
  
  items <- preds@items[[user_index]]
  ids <- preds@itemLabels[items]
  
  names <- sapply(ids, function(id){
    result <- subset(movie_data, movieId == as.numeric(id))$title
    if(length(result) == 0) return(NA)
    return(result)
  })
  
  return(names)
}

# ==============================
# 🎯 MOVIE-BASED RECOMMENDATION
# ==============================
recommend_similar_movies <- function(movie_name){
  
  index <- which(movie_data$title == movie_name)
  
  if(length(index) == 0){
    return("Movie not found")
  }
  
  if(index > nrow(hybrid_sim)){
    return("Movie not in similarity matrix")
  }
  
  sim_scores <- hybrid_sim[index, ]
  top <- order(sim_scores, decreasing=TRUE)[2:6]
  
  return(movie_data$title[top])
}

# ==============================
# 🎨 MODERN UI
# ==============================
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #0f172a;
        color: white;
      }
      .card {
        background-color: #1e293b;
        padding: 20px;
        border-radius: 15px;
        box-shadow: 0 4px 10px rgba(0,0,0,0.5);
      }
      .title {
        font-size: 32px;
        font-weight: bold;
        margin-bottom: 20px;
      }
      .btn-custom {
        background-color: #22c55e;
        color: white;
        border-radius: 10px;
        padding: 10px 20px;
        border: none;
      }
      .btn-custom:hover {
        background-color: #16a34a;
      }
    "))
  ),
  
  div(class = "title", "🎬 Hybrid Movie Recommendation System"),
  
  tabsetPanel(
    
    tabPanel("👤 User Recommendation",
             br(),
             fluidRow(
               
               column(4,
                      div(class = "card",
                          h4("Enter User ID"),
                          numericInput("user_id", "", value = 1, min = 1),
                          actionButton("user_btn", "Recommend", class = "btn-custom")
                      )
               ),
               
               column(8,
                      div(class = "card",
                          h4("Top Recommendations"),
                          tableOutput("user_results")
                      )
               )
             )
    ),
    
    tabPanel("🎥 Movie Similarity",
             br(),
             fluidRow(
               
               column(4,
                      div(class = "card",
                          h4("Search Movie"),
                          textInput("movie_name", "", "Toy Story (1995)"),
                          actionButton("movie_btn", "Find Similar", class = "btn-custom")
                      )
               ),
               
               column(8,
                      div(class = "card",
                          h4("Similar Movies"),
                          tableOutput("movie_results")
                      )
               )
             )
    )
  )
)

# ==============================
# ⚙️ SERVER
# ==============================
server <- function(input, output){
  
  observeEvent(input$user_btn,{
    
    output$user_results <- renderTable({
      data.frame(Status = "⏳ Loading recommendations...")
    })
    
    Sys.sleep(0.5)
    
    res <- recommend_for_user(input$user_id)
    
    output$user_results <- renderTable({
      data.frame(Rank = 1:length(res), Movie = res)
    })
  })
  
  observeEvent(input$movie_btn,{
    
    res <- recommend_similar_movies(input$movie_name)
    
    output$movie_results <- renderTable({
      data.frame(Rank = 1:length(res), Movie = res)
    })
  })
}

# ==============================
# 🚀 RUN APP
# ==============================
shinyApp(ui, server)