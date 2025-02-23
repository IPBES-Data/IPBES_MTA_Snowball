#' Create an Interactive Literature Table - Overarching chapters
#'
#' Merges snowball citation data from multiple chapters into a single dataset, removing duplicates.
#'
#' @details
#' - Iterates through all 5 folders (4 chapters + 1 overarching), loading or iif needed generating snowball data.
#' - Converts the data to a tibble format.
#' - Removes duplicate entries based on OpenAlex ID (`id`).
#' - Creates an interactive table visualization using `IPBES.R::table_dt()`.


combine_and_remove_duplicates<-function(){


  # Initialize an empty tibble to store the combined results
  Combined_table <- tibble()

  for (Chapt in 1:5) {
    
    # Correct file path reference using Chapt instead of Cha
    fn_snowball <- file.path(".", "data", paste0(Chapt, "_chapter"), paste0(Chapt, "_chapter_oa_snowball.rds"))
    
    # Initialize an empty list for snowball in case the file doesn't exist
    snowball <- list()
    
    # Load or generate snowball data
    if (file.exists(fn_snowball)) {
      snowball <- readRDS(fn_snowball)
    } else {
      generate_oa_snowball(Chapt)
      snowball <- readRDS(fn_snowball)
    }
    
    # Convert snowball data to tibble
    flat_snow <- snowball2df(snowball) |> as_tibble()
    
    # Add an abbreviated author column
    flat_snow$author_short <- IPBES.R::abbreviate_authors(flat_snow)
    
    # Combine tables while removing duplicates based on OpenAlexID
    Combined_table <- bind_rows(Combined_table, flat_snow) |> distinct(id, .keep_all = TRUE)
    gc()
  }
  
  
  
  Combined_table|> rename(
    year = publication_year,
    journal = so
  ) |>
    select(
      id, doi, author_short, title, journal
    )|>
    IPBES.R::table_dt(fixedColumns = NULL, fn="L2")

}