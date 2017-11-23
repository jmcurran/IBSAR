writeTT = function(path = "~/Dropbox/Work/2017/Conferences/IBS-AR/IBSAR-Prog/"){
  programme = getProgrammeSheet()
  progTbl = programme %>% gs_read(ws = "progTbl")
  # %>% arrange(day, time)

  daysOfWeek = paste0(c("Mon", "Tues", "Wednes", "Thurs"), "day")

  fileName = paste0(path, "00-Programme.Rmd")
  f1 = file(fileName, "w")

  buildHTMLTable = function(dayProgTbl){
    numStreams = max(dayProgTbl$stream, na.rm = TRUE)

    tbl = "<table>"
    tbl = c(tbl, "<thead>")

    dayOfWeek = dayProgTbl %>%
      filter(is.na(time)) %>%
      select(title)

    dayProgTbl = dayProgTbl %>%
      filter(!is.na(time))

    tbl = c(tbl, "<tr>")
    tbl = c(tbl, "<th class = \"time\">Time</th>")
    tbl = c(tbl, sprintf("<th class = \"day\" colspan = \"%d\">%s</th>", numStreams, dayOfWeek$title))
    tbl = c(tbl, "</tr>")
    tbl = c(tbl, "</thead>")

    tbl = c(tbl, "<tbody>")

    times = dayProgTbl %>%
      distinct(time) %>%
      select(time) %>%
      arrange(time)

    createHTMlrow = function(rowData){
      nRows = nrow(rowData)
      # browser()

      if(nRows == 1){ ## usually housekeeping, keynote, mealbreak, poster, close
        tblRow = "<tr>"

        if(rowData$type %in% c("close","housekeeping", "keynote", "mealbreak", "poster", "welcome")){
          timeStr = sprintf("<td class = \"time\">%d</td>", rowData$time)
          tblRow = c(tblRow, timeStr)

          rowData = rowData %>%
            mutate(rawEntry = gsub("\n", "<br/>", rawEntry))

          rowStr = sprintf("<td class = \"%s\" colspan = \"%d\">%s</td>",
                           rowData$type,
                           numStreams,
                           rowData$rawEntry)
          tblRow = c(tblRow, rowStr)
          tblRow = c(tblRow, "</tr>")
        }else{
          ## This only happens in one place day 1, 12 pm stream 1 is emptpy
          timeStr = sprintf("<td class = \"time\">%d</td>", rowData$time)
          tblRow = c(tblRow, timeStr)

          rowData = rowData %>%
            mutate(rawEntry = gsub("\n", "<br/>", rawEntry))

          rowStr = sprintf("<td class = \"contributed\"></td>\n<td class = \"%s\">%s</td>",
                           rowData$type,
                           rowData$rawEntry)
          tblRow = c(tblRow, rowStr)
          tblRow = c(tblRow, "</tr>")
        }
      }else if(nRows >=2){ ##  2-3 talks or 2-3 session headers and 2-3 talks
        tblRow = NULL

        sessionHeaders = rowData %>%
          filter(type == "sessionheader") %>%
          arrange(stream)

        if(nrow(sessionHeaders) > 0){
          tblRow = "<tr>"
          tblRow = c(tblRow, "<td class = \"time\"></td>")

          sessionStr = sprintf("<td class = \"sessionheader\">%s</td>", sessionHeaders$title)
          tblRow = c(tblRow, sessionStr)
          tblRow = c(tblRow, "</tr>")
        }

        if(is.null(tblRow)){
          tblRow = "<tr>"
        }else{
          tblRow = c(tblRow, "<tr>")
        }

        timeStr = sprintf("<td class = \"time\">%d</td>", rowData$time[1])
        tblRow = c(tblRow, timeStr)

        talks = rowData %>%
          filter(type == "contributed") %>%
          arrange(stream) %>%
          mutate(rawEntry = gsub("\n", "<br/>", rawEntry))
        talkStr = sprintf("<td class = \"contributed\">%s</td>", talks$rawEntry)
        tblRow = c(tblRow, talkStr)
        tblRow = c(tblRow, "</tr>")
      }else{
        browser()
      }

      return(tblRow)
    }

    for(tm in times$time){
      rowData = dayProgTbl %>% filter(time == tm)
      tbl = c(tbl, createHTMlrow(rowData))
    }

    # browser()

    tbl = c(tbl, "</tbody>")
    tbl = c(tbl, "</table>")

  }

  writeLines("# Programme At A Glance {-}", f1)

  for(d in 1:4){
    dayProgTbl = progTbl %>%
      filter(day == d)

    sessions = dayProgTbl %>%
      filter(type == "sessionheader") %>% select(programmeID)

    sessionTimes = dayProgTbl %>%
      filter(programmeID %in% (sessions$programmeID + 1)) %>%
      select(time)

    dayProgTbl$time[dayProgTbl$programmeID %in% sessions$programmeID] = sessionTimes$time

    dayProgTbl = dayProgTbl %>%  arrange(time)

    writeLines(sprintf("## %s {-}", daysOfWeek[d]), f1)

    htmlTbl = buildHTMLTable(dayProgTbl)
    writeLines(htmlTbl, f1)
  }

  close(f1)
}
