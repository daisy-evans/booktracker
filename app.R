library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(DT)

# Define the data file path
DATA_FILE <- "books_data.csv"

# Function to load books from CSV
load_books <- function() {
  if (file.exists(DATA_FILE)) {
    read.csv(DATA_FILE, stringsAsFactors = FALSE)
  } else {
    data.frame(
      Title = character(),
      Author = character(),
      DateRead = date(),
      Genre = character(),
      Year = integer(),
      Rating = numeric(),
      stringsAsFactors = FALSE
    )
  }
}

# Function to save books to CSV
save_books <- function(books_df) {
  write.csv(books_df, DATA_FILE, row.names = FALSE)
}

ui <- page_sidebar(
  title = "Book Tracker",
  sidebar = sidebar(
    h4("Add a Book"),
    textInput("title", "Book Title", placeholder = "Enter title..."),
    textInput("author", "Author", placeholder = "Enter author..."),
    dateInput("dateread", "Date Read", format = "yyyy-mm-dd"),
    selectInput("genre", "Genre", 
                choices = c("Fiction", "Non-Fiction", "Science Fiction", 
                           "Fantasy", "Mystery", "Romance", "Biography", 
                           "History", "Other")),
    numericInput("year", "Year First Published", 
                value = 2024, min = 1000, max = 2024, step = 1),
    sliderInput("rating", "Rating (out of 5 stars)", 
               min = 1, max = 5, value = 3, step = 0.5),
    actionButton("add_book", "Add Book", class = "btn-primary"),
    hr(),
    actionButton("delete_selected", "Delete Selected", class = "btn-warning"),
    hr(),
    downloadButton("download_csv", "Download CSV", class = "btn-success"),
    width = 400
  ),
  
  card(
    card_header("Book Collection"),
    DTOutput("book_table")
  ),
  
  layout_columns(
    card(
      card_header("Ratings Distribution"),
      plotOutput("rating_plot")
    ),
    card(
      card_header("Books by Genre"),
      plotOutput("genre_plot")
    ),
    col_widths = c(6, 6)
  ),
  
  layout_columns(
    card(
      card_header("Year Published Distribution"),
      plotOutput("published_plot")
    ),
    card(
      card_header("Month Read Distribution"),
      plotOutput("read_plot")
    ),
    col_widths = c(6, 6)
  )
  # ,
  # 
  # card(
  #   card_header("Books Over Time"),
  #   plotOutput("timeline_plot")
  # )
)

server <- function(input, output, session) {
  # Load existing books on startup
  books <- reactiveVal(load_books())
  
  # Add book when button is clicked
  observeEvent(input$add_book, {
    # Validate inputs
    req(input$title, input$author)
    
    if(input$title == "" || input$author == "") {
      showNotification("Please enter both title and author", type = "error")
      return()
    }
    
    # Create new book entry
    new_book <- data.frame(
      Title = input$title,
      Author = input$author,
      DateRead = input$dateread,
      Genre = input$genre,
      Year = input$year,
      Rating = input$rating,
      stringsAsFactors = FALSE
    )
    
    # Add to existing books
    updated_books <- rbind(books(), new_book)
    books(updated_books)
    
    # Save to file
    save_books(updated_books)
    
    # Clear inputs
    updateTextInput(session, "title", value = "")
    updateTextInput(session, "author", value = "")
    
    showNotification("Book added and saved successfully!", type = "message")
  })
  
  # Delete selected rows
  observeEvent(input$delete_selected, {
    selected_rows <- input$book_table_rows_selected
    
    if(is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("Please select rows to delete", type = "warning")
      return()
    }
    
    # Remove selected rows
    updated_books <- books()[-selected_rows, , drop = FALSE]
    books(updated_books)
    
    # Save to file
    save_books(updated_books)
    
    showNotification(paste(length(selected_rows), "book(s) deleted"), type = "message")
  })
  
  # Display book table
  output$book_table <- renderDT({
    req(nrow(books()) > 0)
    datatable(books(), 
              selection = 'multiple',
              options = list(pageLength = 10, 
                           scrollX = TRUE),
              rownames = FALSE)
  })
  
  # Rating distribution plot
  output$rating_plot <- renderPlot({
    req(nrow(books()) > 0)
    
    ggplot(books(), aes(x = Rating)) +
      geom_histogram(binwidth = 0.5, fill = "#3498db", color = "white") +
      scale_x_continuous(breaks = seq(1, 5, 0.5), limits = c(0.5,5.5)) +
      labs(title = "Distribution of Ratings", 
           x = "Rating", 
           y = "Number of Books") +
      theme_minimal()
  })
  
  # Genre distribution plot
  output$genre_plot <- renderPlot({
    req(nrow(books()) > 0)
    
    genre_counts <- books() %>%
      count(Genre) %>%
      arrange(desc(n))
    
    ggplot(genre_counts, aes(x = reorder(Genre, n), y = n)) +
      geom_col(fill = "#2ecc71") +
      coord_flip() +
      labs(title = "Books by Genre", 
           x = "Genre", 
           y = "Number of Books") +
      theme_minimal()
  })
  
  # Year Published plot
  output$published_plot <- renderPlot({
    req(nrow(books()) > 0)
    
    year_pub_counts <- books() %>%
      count(Year) %>%
      arrange(desc(n))
    
    ggplot(year_pub_counts, aes(x = reorder(Year, n), y = n)) +
      geom_col(fill = "#2ecc71") +
      #coord_flip() +
      labs(title = "Year Published", 
           x = "Year", 
           y = "Number of Books") +
      theme_minimal()
  })
  
  # Year Read plot
  output$read_plot <- renderPlot({
    req(nrow(books()) > 0)
    
    year_read_counts <- books() %>%
      mutate(YearRead = lubridate::floor_date(lubridate::dmy(DateRead),unit = "month")) %>%
      group_by(YearRead, Genre) %>%
      summarise(
        n = n()
      )
    
    ggplot(year_read_counts, aes(x = YearRead, y = n, colour=Genre, fill=Genre)) +
      geom_bar(position = "stack", stat = "identity") +
      #coord_flip() +
      labs(title = "Year Read", 
           x = "Year", 
           y = "Number of Books") +
      theme_minimal()
  })
  
  # Timeline plot
  output$timeline_plot <- renderPlot({
    req(nrow(books()) > 0)
    
    year_summary <- books() %>%
      group_by(DateRead) %>%
      summarise(
        Count = n(),
        AvgRating = mean(Rating),
        .groups = 'drop'
      )
    
    ggplot(year_summary, aes(x = DateRead, y = Count)) +
      geom_line(color = "#e74c3c", size = 1, group=1) +
      geom_point(color = "#e74c3c", alpha = 0.7) +
      labs(title = "Books Read Over Time", 
           x = "Date Read", 
           y = "Number of Books") +
      theme_minimal() +
      theme(legend.position = "right")
  })
  
  # Download CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("book_collection_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(books(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
