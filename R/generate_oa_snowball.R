#' Generate a Snowball Literature Network
#'
#' Constructs a citation-based literature network using OpenAlex data for a given chapter.
#'
#' @param Cha Chapter number (1–5) to process, with 5 being overarching refs.
#'
#' @details 
#' - Loads existing snowball data if available; otherwise, generates new one.
#' - Matches OpenAlex IDs using `match_oaID_by_title_or_doi()`.
#' - Filters unique OpenAlex IDs and retrieves citation connections via `oa_snowball()`.
#' - Saves the generated network as an `.rds` file with a timestamp.


generate_oa_snowball<-function(Cha){
 
   snowball=NULL
  
  fn_snowball<- file.path(".","data",paste0(Cha,"_chapter"),paste0(Cha,"_chapter_oa_snowball.rds"))
  
  if(file.exists(fn_snowball)){
    cat("Snowball literature network for chapter", Cha,"is loaded from", fn_snowball, "\n")
    snowball<-readRDS(fn_snowball)
    }else{
    cat("Snowball literature network for chapter", Cha,"are newly created. This might take a moment.\n")
    match_oaID_by_title_or_doi(Cha)
    
    fn_oa_matched <- file.path(".","input",paste0(Cha,"_chapter"),paste0(Cha,"_chapter_oa_machted.rds"))
    
    #load matched OpenAlex IDs
    oa_matched<-readRDS(fn_oa_matched)
    
    #Filter out doubled and empty IDs
    unique_OpenAlexID<-unique(unlist(oa_matched$OpenAlexID))
    #unique_OpenAlexID<-unique_OpenAlexID[!is.na(unique_OpenAlexID) && !is.null(!is.na(unique_OpenAlexID)) && !""]
    
    #generate snowball
    snowball<-oa_snowball(
      identifier = unique_OpenAlexID, 
      verbose=TRUE
    )
    
    snowball$time_of_generating <- Sys.Date()
    #save to data folder
    saveRDS(snowball, fn_snowball)
    cat("Snowball successfull!")
    }
    
  cat("In total", length(unique(snowball$nodes[["id"]])), "references in the expanded citation network.\n.")
  
}
