library(magrittr)

# text parser
csie_train_xml_parser <- function(csie_xml_path){
  xml_file <- readLines(csie_xml_path)
  cleaned_xml_file <-  gsub(pattern = "\\s", xml_file, replacement = "")
  xml_data_logical <- grepl(pattern = "<Cell><Datass:Type=\"String\">.+</Data></Cell>", cleaned_xml_file)
  student_info <- cleaned_xml_file[xml_data_logical] %>%
    gsub(pattern = "<Cell><Datass:Type=\"String\">", ., replacement = "") %>%
    gsub(pattern = "</Data></Cell>", replacement = "")
  student_info <- student_info[-1:-11]
  list_len <- length(student_info) / 10
  student_list <- list()
  for (i in 1:list_len) {
    student_list[[i]] <- student_info[(1 + 10*(i-1)):(i * 10)]
  }
  return(student_list)
}

# list_to_df
list_to_df <- function(x){
  enroll_ids <- c()
  names <- c()
  for (i in 1:length(x)) {
    enroll_ids[i] <- x[[i]][1]
    names[i] <- x[[i]][2]
  }
  df <- data.frame(enroll_id = enroll_ids, name = names)
  return(df)
}

file_path <- "/Users/kuoyaojen/Downloads/287.xls" # Revise here
students_info <- csie_train_xml_parser(file_path)
students_id_name <- list_to_df(students_info)
students_id_name <- students_id_name[order(students_id_name$enroll_id),]
write.csv(students_id_name, file = "student.csv", row.names = FALSE)