createAuthorTbl = function(ss, create = FALSE){
  ## Create the Author Table
  allSheet = gs_read(ss, ws = "All")

  authorTbl = allSheet %>%
    select(-Submission) %>%
    distinct(Author, .keep_all  = TRUE)

  authorTbl = authorTbl %>%
    add_column(authorID = 1:nrow(authorTbl), .before = 1) %>%
    rename(author = Author, email = Email, country = Country, affiliation = Affiliation)

  allTbl = allSheet %>% select(Affiliation,Unified.Affiliation,Unified.Affiliation2) %>%
    rename(raw.affilitation = Affiliation, affiliation = Unified.Affiliation)

  affilTbl = gs_read(ss, ws = "affilTbl")
  allTbl = allTbl %>%
    left_join(affilTbl, by = "affiliation") %>%
    select(raw.affilitation,affilID) %>%
    rename(affiliation = raw.affilitation)

  authorTbl = authorTbl %>%
    left_join(allTbl, by = "affiliation") %>%
    select(-affiliation) %>%
    distinct(authorID, .keep_all = TRUE) %>%
    full_join(affilTbl, by = c("Unified.Affiliation2" = "affiliation")) %>%
    filter(!is.na(authorID))

  authorTbl = authorTbl %>%
    rename(affilID1 = affilID.x, affilID2 = affilID.y) %>%
    select(-Unified.Affiliation,-Unified.Affiliation2)

  if(create){
    ss = ss %>% gs_ws_new(ws_title = "authorTbl", input = authorTbl, trim = TRUE, row_extent = 5)
  }else{
    ss = ss %>% gs_edit_cells(ws = "authorTbl", input = authorTbl, anchor = "A1", trim = TRUE)
  }

  # db = dbConnect(RSQLite::SQLite(), "ibsar")
  # dbWriteTable(db, "authorTbl", authorTbl)
  # dbDisconnect(db)

  return(ss)
}
