library(magrittr)

csietrain_data_parser_latest <- function(file_path) {
  tryCatch({
    xml_txt <- readLines(file_path)
  }, warning = function(w) {
    print("There is a warning about incomplete final line.")
  })
  cleaned_xml_txt <- xml_txt %>%
    gsub(pattern = "\\s", ., replacement = "")
  student_data_pattern <- "<Cell><Datass:Type=\"String\">.*</Data></Cell>"
  is_student_data <- grepl(pattern = student_data_pattern, cleaned_xml_txt)
  student_data <- cleaned_xml_txt[is_student_data]
  student_data <- student_data[12:length(student_data)]
  student_data <- student_data %>%
    gsub(pattern = "<Cell><Datass:Type=\"String\">", ., replacement = "") %>%
    gsub(pattern = "</Data></Cell>", ., replacement = "")
  student_id <- vector()
  student_name <- vector()
  payment_status <- vector()
  for (i in 1:length(student_data)) {
    if (i %% 11 == 1) {
      student_id <- c(student_id, student_data[i])
    } else if (i %% 11 == 2) {
      student_name <- c(student_name, student_data[i])
    } else if (i %% 11 == 9) {
      payment_status <- c(payment_status, student_data[i])
    }
  }
  df <- data.frame(student_id, student_name, payment_status, stringsAsFactors = FALSE)
  write.csv(df, file = "~/student.csv", row.names = FALSE)
}