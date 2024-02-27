# app/logic/fmtDate

box::use(
  magrittr[`%>%`],
  stringr[str_split_1,str_sub],
  purrr[map],
)

#' @export
fmtDate = function(date){

  date_formatted = tryCatch({
    map(date, function(d){
      d = d %>% as.character %>% str_split_1(pattern = "\\.")
      year = d[1]
      month = d[2] %>% str_sub(start = 1, end = 2)
      day = d[2] %>% str_sub(start = 3, end = 4)
      paste(c(year,month,day),collapse = "-")
    }) %>%
      unlist() %>%
      as.Date()
  }, error = function(e){
    warning(e)
    return(date)
  })

  return(date_formatted)
}
