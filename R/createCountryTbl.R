createCountryTbl = function(ss, create = FALSE){
  allSheet = gs_read(ss, ws = "All")
  countryTbl = data_frame(country = sort(unique(allSheet$Country)))
  countryTbl = countryTbl %>% add_column(countryID = 1:nrow(countryTbl), .before = 1)

  ss = if(create){
    ss %>% gs_ws_new(ws_title = "countryTbl", input = countryTbl, trim = TRUE, row_extent = 7)
  }else{
    ss %>% gs_edit_cells(ws = "countryTbl", input = countryTbl, anchor = "A1", trim = TRUE)
  }

  db = dbConnect(RSQLite::SQLite(), "ibsar")
  dbWriteTable(db, "countryTbl", countryTbl)
  dbDisconnect(db)

  return(ss)
}
