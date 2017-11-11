checkSpelling = function(path = "~/Dropbox/Work/2017/Conferences/IBS-AR/",
                         checkTitles = FALSE){
  Files = list.files(path = path, pattern = "(poster|sub)[0-9]{1,2}\\.tex")
  results = data_frame(file = character(), titlewords = character(), words = character())
  library(hunspell)
  for(f1 in Files){
    Lines = readLines(paste0(path, f1))

    cat(sprintf("\nFile: %s\n", f1))

    if(checkTitles){
      titleLine = gsub("^\\\\subsection[*]\\{([^}]+)\\}$", "\\1", Lines[grep("\\\\subsection", Lines)])
      bad = unlist(hunspell(titleLine, format = "latex", dict = "EN_GB"))
      if(length(bad) > 0){
        results = results %>% add_row(file = f1, titlewords = paste(bad), words = "")
      }
    }

    i = grep("\\\\end\\{center\\}", Lines)
    Lines = Lines[-(1:i)]

    bad = unlist(hunspell(Lines, format = "latex", dict = "EN_GB"))
    if(length(bad) > 0){
      results = results %>% add_row(file = f1, titlewords = "", words = paste(bad))
    }
  }
  return(results)
}
