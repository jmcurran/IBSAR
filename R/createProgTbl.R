createProgTbl = function(ss, create = FALSE, delete = FALSE){

  ### Try and match the current programme to talks
  buildTbl = function(){
    mainProg = gs_read(ss, ws = "Programme", range = "A1:M24", col_names = LETTERS[1:13])

    Days = list(mainProg$A, mainProg$D, mainProg$G, mainProg$K)

    i = 1
    Sessions = list()

    for(col in c("B", "C", "E", "F", "H", "I", "J", "L", "M")){
      Sessions[[i]] = mainProg[[col]]# sub("^([^\n]*)\n.*$", "\\1", mainProg[[col]])
      i = i + 1
    }

    Daysession = c(1,1,2,2,3,3,3,4,4)
    Stream = c(1,2,1,2,1,2,3,1,2)

    i = 1
    programmeID = 1
    progTbl = data_frame(programmeID = integer(), day = integer(), stream = integer(),
                         time = integer(), rawEntry = character(), type = character())

    for(session in Sessions){
      day = Daysession[i]
      stream = Stream[i]
      times = Days[[day]]

      for(j in 1:length(session)){
        if(!is.na(session[j])){

          progTbl = progTbl %>% add_row(programmeID = as.integer(programmeID),
                                        day = as.integer(day),
                                        stream = as.integer(stream),
                                        time = as.integer(times[j]),
                                        rawEntry = session[j])
          programmeID = programmeID + 1
        }
        j = j + 1
      }
      i = i + 1
    }
    return(progTbl)
  }

  proper = function(x){
    gsub("(?<=\\b)([a-z])", "\\U\\1", x, perl = TRUE)
  }

  progTbl = buildTbl()
  progTbl = progTbl %>% mutate(title = proper(sub("^([^\n]*)\n.*$", "\\1", rawEntry)))
  progTbl = progTbl %>%
    mutate(presenter = sub("^([^\n]*)\n([^\n]*).*$", "\\2", rawEntry)) %>%
    mutate(presenter = gsub("(^[^,]*),.*$", "\\1", presenter))

  authorTbl = gs_read(ss, ws = "authorTbl")
  progTbl = progTbl %>% left_join(select(authorTbl, authorID, author), by = c("presenter" = "author"))

  catherine = slice(authorTbl, match("Catherine M. McKenzie", author))
  irene = slice(authorTbl, match("Irene Suilan Zeng", author))
  progTbl$authorID[which(progTbl$presenter == "Catherine McKenzie")] = catherine$authorID
  progTbl$authorID[which(progTbl$presenter == "Irene Zeng")] = irene$authorID


  titleTbl = gs_read(ss, ws = "titleTbl")
  progTbl = progTbl %>% left_join(titleTbl, by = "title")

  getType = function(col){
    keynotes = "(Elisabetta|Jean|Rachel|Dianne|Sonja|Louise Ryan)"
    meals =  "(Morning|Lunch|Afternoon)"
    posters = "Lightning"
    welcome = "Welcome"
    confClose = "^.*Conference Close.*$"
    housekeeping = "Housekeeping"
    conferenceDay = "(Mon|Tues|Wednes|Thurs)day"

    type = rep("", length(col))

    type[grep(housekeeping, col)] = "housekeeping"
    type[grep(conferenceDay, col)] = "day"
    type[grep(meals, col)] = "mealbreak"
    type[grep(posters, col)] = "poster"
    type[grep(welcome, col)] = "welcome"
    type[grepl(keynotes, col) & !grepl(confClose, col)] = "keynote"
    type[grepl(confClose, col)] = "close"
    type[grepl("^$", type)] = "contributed"

    return(type)
  }

  progTbl = progTbl %>%
    mutate(type = getType(rawEntry)) %>%
    mutate(type = replace(type, is.na(time) & type != "day", "sessionheader"))


  if(delete){
    ss = ss %>% gs_ws_delete(ws = "progTbl") %>% gs_ws_new(ws_title = "progTbl", input = progTbl, trim = TRUE, row_extent = 10)
  }else{
    ss = if(create){
      ss %>% gs_ws_new(ws_title = "progTbl", input = progTbl, trim = TRUE, row_extent = 10)
    }else{
      ss %>% gs_edit_cells(ws = "progTbl", input = progTbl, anchor = "A1", trim = TRUE)
    }
  }

  db = dbConnect(RSQLite::SQLite(), "ibsar")
  dbWriteTable(db, "progTbl", progTbl)
  dbDisconnect(db)

  return(ss)
}
