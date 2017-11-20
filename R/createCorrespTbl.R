createCorrespondingTbl = function(ss, create = FALSE){
  correspSheet = ss %>% gs_read(ws = "AcceptedCorresponding")
  authorTbl = ss %>% gs_read(ws = "authorTbl")

  correspTbl = authorTbl %>%
    left_join(correspSheet, by = c("author" = "Author")) %>%
    select(authorID,`#`) %>%
    rename(subID = `#`) %>%
    filter(!is.na(subID))

  ss = if(create){
    ss %>% gs_ws_new(ws_title = "correspTbl", input = correspTbl, row_extent = 2)
  }else{
    ss %>% gs_edit_cells(ws = "correspTbl", input = correspTbl, anchor = "A1", trim = TRUE)
  }

  db = dbConnect(RSQLite::SQLite(), "ibsar")
  dbWriteTable(db, "correspTbl", correspTbl)
  dbDisconnect(db)

  return(ss)
}
