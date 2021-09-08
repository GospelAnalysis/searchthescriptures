
#' Ask
#'
#' "Ask, and ye shall receive." Specify some search
#' terms and get an output of all scriptures that match
#' those terms.
#'
#' Assumes you pass in a dataframe of scriptures, which allows
#' you to edit/manipulate that dataframe and only parse the fields
#' you want.
#'
#' @param df_text A dataframe of text.
#' @param terms An element or a vector of strings
#'
#' @return
#' @export
#'
#' examples

library(scriptuRs)
library(dplyr)
dfs <- lds_scriptures() %>%
  mutate(text2 = tolower(text))

ask <- function(df_text, col, terms, output='dt', n = 200){
  # varname <- deparse(substitute(col))

  x <- df_text
  color = "#56B4E9"
  x$Text <- dplyr::pull(x, {{col}})
  for(t in terms){
    # filter to just search terms
    x <- dplyr::filter(x, stringr::str_detect({{ col }}, t))
    # highlight the terms
    x <- dplyr::mutate(
      x,
      Text = stringr::str_replace_all(
        Text,
        t,
        paste0('<span style="color:', color, '; font-weight:bold">', t, '</span>')
        ))
  }
  x <- dplyr::select(x, -{{col}})
  msg <- paste0('There are ', nrow(x), ' results. Showing ', n, '.')
  message(msg)
  x <- head(x, n)

  if (output == 'view'){
    View(x)
  } else {
    DT::datatable(
      x,
      escape=F
    )
  }
}
dfs %>%
  select(verse_short_title, text2) %>%
  ask(text2, 'ye')
