# app/logic/idno2storage

box::use(
  dplyr[mutate, rename],
  tidyr[separate],
  stringr[str_remove_all],
  magrittr[`%<>%`,`%>%`]
)

#' @export
idno2storage = function(df, idno = 'idno') {
  if (!idno %in% names(df))
    stop("No idno in storage locations")
  seps = c("_s", "_u", "_rw", "_rm")
  ids = c("shelf", "unit", "row", "room")
  df = df %>%
    mutate(idnoTmp = !!as.name(idno))
  for (i in 1:length(seps)) {
    df = df %>%
      separate(
        idnoTmp,
        into = c('idnoTmp', ids[i]),
        sep = seps[i],
        fill = "right"
      )
  }
  df = df %>%
    mutate(idnoTmp = idnoTmp %>% str_remove_all("^b")) %>%
    rename(building = idnoTmp)
  return(df)
}
