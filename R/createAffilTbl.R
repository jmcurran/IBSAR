createAffilTbl = function(ss, create = FALSE){
  allSheet = gs_read(ss, ws = "All")
  affilTbl = data_frame(affiliation = sort(unique(c(allSheet$Unified.Affiliation, allSheet$Unified.Affiliation2))))
  affilTbl = affilTbl %>% add_column(affilID = 1:nrow(affilTbl), .before = 1)

  ss = if(create){
    ss %>% gs_ws_new(ws_title = "affilTbl", input = affilTbl, trim = TRUE, row_extent = 2)
  }else{
    ss %>% gs_edit_cells(ws = "affilTbl", input = affilTbl, anchor = "A1", trim = TRUE)
  }

  # db = dbConnect(RSQLite::SQLite(), "ibsar")
  # dbWriteTable(db, "affilTbl", affilTbl, overwrite = TRUE)
  # dbDisconnect(db)

  return(ss)
}
