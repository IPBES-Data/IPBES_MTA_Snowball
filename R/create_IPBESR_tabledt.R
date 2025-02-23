
#' Create an Interactive Literature Table per Chapter
#'
#' Generates an interactive table from a snowball literature network for a given chapter.
#'
#' @param Cha Chapter number (1–5) to process.
#'
#' @details 
#' - Loads snowball literature network, if it exists. If not gernerate new snowball
#' - Converts the data into a tibble format.
#' - Extracts key metadata (DOI, author, title, journal, and year).
#' - Creates an interactive data table using `IPBES.R::table_dt()`.


create_IPBESR_tabledt<-function(Cha){
  
  snowball<-list()
  fn_snowball<- file.path(".","data",paste0(Cha,"_chapter"),paste0(Cha,"_chapter_oa_snowball.rds"))
  if(file.exists(fn_snowball)){
    snowball<-readRDS(fn_snowball)
  }else{
      generate_oa_snowball(Cha)
      snowball<-readRDS(fn_snowball)
    }
  
  flat_snow<-snowball2df(snowball)|>tibble::as_tibble()
  flat_snow$author_short<-IPBES.R::abbreviate_authors(flat_snow)
  
  flat_snow <- flat_snow |>
    mutate(
      id = sprintf('<a href="https://openalex.org/%s" target="_blank">%s</a>', id, id),
      doi = sprintf('<a href="%s" target="_blank">%s</a>', doi, gsub("https://doi.org/", "", doi))
    ) 
  
  flat_snow |> rename(
    year = publication_year,
    journal = so
  ) |>
    select(
      id, doi, author_short, title, journal
    )|>
    IPBES.R::table_dt(fixedColumns = NULL, fn="L2")
  
}