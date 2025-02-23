#' Retrieve References from a Zotero Collection
#'
#' Fetches metadata (title, DOI) from a specified Zotero collection using the API.
#'
#' @param Cha Chapter number (1–5) to process, with 5 being overarching refs.
#'
#' @details 
#' - Loads API credentials from environment variables.
#' - Retrieves references in batches of 100 until all items are fetched.
#' - Extracts and returns titles and DOIs as a list.
#' 
#' @return A list containing the retrieved metadata.

load_refs_from_zoteroCollection <- function(Cha) {
  
  # Load Zotero API credentials
  user_id <- Sys.getenv("ZOTERO_USER_ID")
  api_key <- Sys.getenv("ZOTERO_API_KEY")
  
  # Validate that credentials are set
  if (user_id == "" || api_key == "") {
    stop("Zotero API credentials are missing. Please set them in ~/.Renviron")
  }
  
  # Current collection identifiers
  # the diffrent collections refer to Chapter 1, Chapter 2, Chapter 3, Chapter 4, and overarching Collection 
  chapter <- c("QAU44694", "EMT6Y9WJ", "LCZNYYU7", "627JJPR6","QVGRU43K")  
  
  # set path to collections
  base_url <- paste0("https://api.zotero.org/users/", user_id, 
                     "/collections/", chapter[Cha], "/items")
  
  #init list of references
  all_data <- list() 
  
  # helper variables for reading in batches, 100 each 
  start <- 0          
  limit <- 100        
  total_items <- 0    
  
  repeat {
    
    # Execute API request 
    response <- tryCatch({
      GET(
        base_url,
        add_headers("Zotero-API-Key" = api_key),
        query = list(format = "json", limit = limit, start = start),
        timeout(10)
      )
    }, error = function(e) {
      message("Error fetching Zotero data: ", e$message)
      return(NULL)
    })
    
    # Parse JSON response
    items <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
    # Exit when no more items are received
    if (is.null(items) || length(items) == 0) {
      break
    }
    
    fetched_count <- length(items$key)
    total_items <- total_items + fetched_count
    
    # Merge fetched items into the aggregated list
      for (key in c("title", "DOI")) {
        if (!is.null(items$data[[key]])) {
          if (!key %in% names(all_data)) {
            all_data[[key]] <- list()
          }
          all_data[[key]] <- append(all_data[[key]], items$data[[key]])
        }
      }
    
    # Update pagination index and pause to avoid rate-limiting
    start <- start + limit
    Sys.sleep(1)
  }
  
  return(all_data)
}

