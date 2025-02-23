#' Match OpenAlex IDs by Title or DOI
#'
#' Matches references from a Zotero collection to OpenAlex IDs using DOI or title search.
#' Results are stored as an `.rds` file for reuse.
#'
#' @param Cha Chapter number (1–5) to process, with 5 being overarching refs.
#'
#' @details 
#' - Loads existing OpenAlex IDs if the `.rds` file already exists.
#' - Otherwise, retrieves metadata from Zotero and searches OpenAlex.
#' - First tries DOI; if missing or unmatched, falls back to a title search.
#' - Saves the matched results with a timestamp.



match_oaID_by_title_or_doi <- function(Cha) {
  
  literature_list<-NULL
  
  fn<-file.path(".","input",paste0(Cha,"_chapter"),paste0(Cha,"_chapter_oa_machted.rds"))
  if (file.exists(fn)){
    cat("OpenAlex IDs for chapter", Cha,"are loaded from", fn, "\n")
    literature_list<-readRDS(fn)
  }else{
    cat("Open Alex IDs are newly matched. This might take a moment\n")
  #load  title and DOI from references in zotero collection
  literature_list<-load_refs_from_zoteroCollection(Cha) 
  
  #Add list element thatreferes to the OpenalexID
  literature_list$OpenAlexID <- vector("list", length(literature_list$title)) # Initialize as list

  #function to remove special characters, as they are not allowed in oa_fetch
  clean_title <- function(title) {
    return(
      gsub("[^a-zA-Z0-9 :()'\\-]", "", title)
      ) 
  }
  
  # Iterate over all entries
  for (i in seq_along(literature_list$title)) {
    
    search_type <- ""
    openalex_result <- NULL
    
    # First try with DOI if given
    if (!is.null(literature_list$DOI[[i]]) && literature_list$DOI[[i]] != "" && !is.na(literature_list$DOI[[i]])) {
      search_type <- "DOI"
      openalex_result <- oa_fetch(entity = "works", doi = literature_list$DOI[[i]])
    }
    

    # If DOI search failed or missing, try title search
    if (is.null(openalex_result) || length(openalex_result) == 0 ){
      # Proceed if title is given
      if(!is.null(literature_list$title[[i]]) &&  literature_list$title[[i]] != ""  && !is.na(literature_list$title[[i]])){
        search_type <- "Title"
        # Remove characters that are not allowed in the oa_fetch() function
        cleaned_title <- clean_title(literature_list$title[[i]])
        openalex_result <- oa_fetch(entity = "works", title.search = cleaned_title)
      }
    }
      # If there they are matches with openAlex
    if (!is.null(openalex_result) && length(openalex_result) > 0) {
        # Take the first entry in the list (typically best matching)
        literature_list$OpenAlexID[[i]] <- as.character(openalex_result[["id"]][[1]]) 
        cat("Found OpenAlex ID", literature_list$OpenAlexID[[i]],"with",search_type, "\n")
      }else{
      cat("No OpenAlex ID found for: ")
      if(!is.null(literature_list$title[[i]]) &&  literature_list$title[[i]] != ""  && !is.na(literature_list$title[[i]])){
        cat(as.character(literature_list$title[[i]]), "\n")
      }else{
        cat("unknown title, position", i, "\n")
      }
    }
  }
  
  literature_list$time_of_generating <- Sys.Date()
  
  saveRDS(literature_list,fn)
  cat("Creating and matching literature list was successful!")
  }
  cat(length(unique(literature_list$OpenAlexID)), "key references linked\n.")
}

