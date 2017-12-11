#' @import dplyr

#' Getting history files.
#' 
#' Factor levels trimming
#' @param folder="." Folder containing history files
#' @param extension=".Rhistory" History files' extension.
#' @keywords history, .Rhistory
#' @export
#' @examples
#' fetch_history(folder=".")
fetch_history <- function(folder=".", extension=".Rhistory"){
    list.files(folder, full.names = T)[list.files(folder, full.names = T) %>% grepl(pattern=extension)]
    }

in_file_count <- function(name, what){
  readLines(name) %>% grep(what, .) %>% length
}

#' History searching.
#' 
#' Searching for string in a multitude of horrible horrible history files. A fancy grep.
#' @param search_string="ccle_cn_endo<-" String to be searched for.
#' @keywords history, search
#' @export
#' @examples
#' hist_search(search_string="horrible_var_name")
hist_search<-function(search_string="ccle_cn_endo<-", location="."){
file.info(names(
        which(sapply(fetch_history(folder=location), function(name) in_file_count(name, search_string)) > 0)
    ))[,c(4,5)]
}    

 
#putting variable names atop their .Rhistory files
adding_varnames<-function(the_name){
  the_lines <- readLines(the_name)

  the_lines[grepl("<-",the_lines)] %>%
    sapply(function(x) sub("(.*)<-.*","\\1",x)) %>%
    as.vector %>% unique ->
    variable_names
  
  write(c("#--#\n#Created variables:\n",variable_names,"#--#\n",the_lines), file=the_name)
}
#sapply(history_files, adding_varnames)

  

