# app/logic/getBoxes

box::use(
  dplyr[tbl,filter,collect,inner_join,left_join,mutate,select],
  magrittr[`%>%`],
  stringr[str_detect],
)

box::use(
  app/logic/idno2storage[idno2storage],
  app/logic/fmtDate[fmtDate],
)

#' @export
getBoxes = function(location_idno,current = T,con){

  result = suppressWarnings({tbl(con,"ca_storage_locations") %>%
    select(location = idno, location_id) %>%
    filter(str_detect(location,location_idno)) %>%
    inner_join(
      tbl(con,"ca_collections_x_storage_locations") %>%
        select(collection_id,location_id, date = edatetime),
      by = "location_id"
    ) %>%
    inner_join(
      tbl(con,"ca_collections") %>%
        select(boxno = idno, collection_id),
      by = "collection_id"
    ) %>%
      left_join(
        tbl(con,"ca_history_tracking_current_values") %>%
          filter(table_num == 13) %>%
          select(collection_id = row_id, location_id = current_row_id) %>%
          mutate(current = TRUE)
      ) %>%
    collect() %>%
    mutate(date = fmtDate(date)) %>%
    idno2storage(idno = "location")
  })

  if(current){
    result = result %>%
      filter(current == 1)
  }

  return(result)
}
