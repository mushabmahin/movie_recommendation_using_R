# Hybrid Movie Recommendation System (R + Shiny)

## 🔍 Overview
A hybrid movie recommendation system combining collaborative filtering and content-based filtering, with an interactive Shiny UI for real-time recommendations.

---

## Problem
Users face difficulty finding relevant movies due to large catalogs. Traditional recommendation approaches either rely only on user behavior or content, limiting accuracy and personalization.

---

## Approach
Built a hybrid recommendation system by combining:

### 1. Collaborative Filtering
- Implemented Item-Based Collaborative Filtering (IBCF)
- Used user-item rating matrix
- Applied cosine similarity for item relationships

### 2. Content-Based Filtering
- Encoded movie genres into feature vectors
- Computed similarity using cosine distance

### 3. Hybrid Model
- Combined both approaches:
  - 70% collaborative filtering  
  - 30% content-based filtering  
- Improved recommendation relevance and diversity

### 4. Interactive UI
- Built using Shiny for real-time interaction
- Supports:
  - User-based recommendations  
  - Movie similarity search  

---

## Results
- Generated personalized movie recommendations  
- Improved recommendation quality using hybrid approach  
- Enabled real-time interaction through UI  
- Successfully handled multiple recommendation scenarios  

---

## Tech Stack
- R  
- Shiny  
- recommenderlab  
- data.table  
- ggplot2  
- proxy  

---

## Demo
Run the app:

```r
shiny::runApp()
