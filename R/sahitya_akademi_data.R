#' get_sahitya_akademi_winners
#' @title Get Sahitya Akademi Winners for any Language
#' @description
#' Get Sahitya Akademi award winners from the Wikipedia page -
#' https://en.wikipedia.org/wiki/Lists_of_Sahitya_Akademi_Award_winners. For
#' each language there is a separate page, for example, for English, the details
#' are available on https://en.wikipedia.org/wiki/List_of_Sahitya_Akademi_Award_winners_for_English.
#' @author Pushker Ravindra
#' @param language a character vector of length 1, for example, 'English'.
#' @return A data.frame containing list of Sahitya Akademi award winners with
#' the following information: Year, Author, Work, Category.
#' @examples
#' get_sahitya_akademi_winners("Hindi")
#' @import rvest
#' @import dplyr
#' @import stringr
#' @export
get_sahitya_akademi_winners <- function(language) {
  language <- stringr::str_to_sentence(language)
  
  language_url <- paste0("https://en.wikipedia.org/wiki/List_of_Sahitya_Akademi_Award_winners_for_", language)
  
  language_html <- tryCatch({
    language_url %>%
      rvest::read_html()
  }, error = function(e) {
    stop(e)
  })
  
  language_table <- language_html %>% rvest::html_nodes("table")
  language_nodes <- language_table[grep("wikitable", language_table)]
  
  language_info <- language_nodes[1] %>% rvest::html_table(fill = TRUE)
  language_info <- as.data.frame(language_info[[1]])
  
  if (language %in% c("Kannada")) {
    year <- language_info[, 1]
    book <- language_info[, 4]
    author <- language_info[, 3]
    category <- language_info[, 5]
  } else if (language %in% c("Konkani")) {
    year <- language_info[, 1]
    book <- language_info[, 3]
    author <- language_info[, 2]
    category <- NA
  } else if (language %in% c("Maithili")) {
    year <- language_info[, 1]
    book <- language_info[, 2]
    author <- language_info[, 3]
    category <- NA
  } else if (language %in% c("Assamese", "Bengali", "English",
                             "Rajasthani", "Santali")) {
    year <- language_info[, 1]
    book <- language_info[, 2]
    author <- language_info[, 3]
    category <- language_info[, 4]
  } else {
    # "Bodo", "Dogri", "Gujarati", "Hindi", "Kashmiri", "Malayalam", "Meitei",
    # "Marathi", "Nepali" ,"Odia", "Punjabi", "Sanskrit", "Sindhi", "Tamil",
    # "Telugu", "Urdu"
    
    year <- language_info[, 1]
    book <- language_info[, 3]
    author <- language_info[, 2]
    category <- language_info[, 4]
  }
  
  data.frame(Year = year, Author = author, Book = book, Category = category)
}

#' get_author_html
#' @title Get Author HTML Content from WikiPedia
#' @description
#' Get Author HTML Content from WikiPedia.
#' @author Pushker Ravindra
#' @param author a character vector of length 1. For example, 'Ruskin Bond'.
#' @return An xml_document containing the html text with author's details
#' like date of birth, etc.
#' @examples
#' get_author_html("Ruskin Bond")
#' @import rvest
#' @import dplyr
#' @export
get_author_html <- function(author) {
  author <- gsub("\\s+", "_", author)
  author_url <- paste0("https://en.wikipedia.org/wiki/", author)
  
  author_html <- tryCatch({
    author_url %>%
      rvest::read_html()
  }, error = function(e) {
    stop(e)
  })
  
  author_html
}

#' get_dob_from_description
#' @title Get the Date of Birth of an Author from the Description
#' @description
#' Get the date of birth of an author from the description when the info box is
#' not present.
#' @author Pushker Ravindra
#' @param author_html a xml_document containing the html text with author's
#' details like date of birth, etc.
#' @return
#' A list of data of birth and date of death with remarks in case there were
#' some challenges in getting date of birth / death.
#' @examples
#' author_html <- get_author_html("Ruskin Bond")
#' get_dob_from_description(author_html)
#' @import stringr
#' @import rvest
#' @import dplyr
#' @export
get_dob_from_description <- function(author_html) {
  author_para <- author_html %>%
    rvest::html_nodes("p") %>%
    rvest::html_text()
  author_para <- paste(author_para, collapse = ";")
  
  dob_died <- unlist(stringr::str_extract_all(author_para, "\\d{4}-\\d{2}-\\d{2}|\\d+ \\w+ \\d{4}|\\d{2} \\w+,\\d{4}|\\w+ \\d{2}, \\d{4}"))
  
  dob <- dob_died[1]
  died <- dob_died[2]
  
  if (grepl(",", dob)) {
    dob <- gsub(",", " ", dob)
    dob <- as.character(as.Date(dob, "%B %d %Y"))
  } else {
    dob <- as.character(as.Date(dob, "%d %B %Y"))
  }
  
  if (grepl(",", died)) {
    died <- gsub(",", " ", died)
    died <- as.character(as.Date(died, "%B %d %Y"))
  } else {
    died <- as.character(as.Date(died, "%d %B %Y"))
  }
  
  return(data.frame(Born = dob, Died = died, Remarks = ""))
}

#' get_dob_from_infobox
#' @title Get Date of Birth from the Author's InfoBox
#' @description Get Date of Birth from the Author's InfoBox which is on the top
#' right side of author's page below their image.
#' @author Pushker Ravindra
#' @param author_info_df a data.frame containing 2 colums Field and Value. Field
#' contains information like Born, Died, which is required to get date of birth.
#' @return
#' A list of data of birth and date of death with remarks in case there were
#' some challenges in getting date of birth / death.
#' @examples
#' author_info_df <- data.frame(Field=c("Born", "Died"), Value=c("Harivansh Rai
#' Srivastava(1907-11-27)27 November 1907Babupatti, United Provinces of Agra and
#' Oudh, British India", "18 January 2003(2003-01-18) (aged 95)"))
#' get_dob_from_infobox(author_info_df)
#' @import stringr
#' @export
get_dob_from_infobox <- function(author_info_df) {
  dob <- author_info_df[author_info_df$Field == "Born", ]$Value
  died <- author_info_df[author_info_df$Field == "Died", ]$Value
  
  if (length(dob) != 0) {
    
    dob <- stringr::str_extract(dob, "\\d{4}-\\d{2}-\\d{2}|\\d+ \\w+ \\d{4}|\\d{2} \\w+,\\d{4}|\\d{4}")
    dob <- gsub(",", " ", dob)
    
    if (grepl(" ", dob)) {
      dob <- as.character(as.Date(dob, format = "%d %B %Y"))
    } else if (!grepl("-", dob)) {
      dob <- paste0(dob, "-01-01")
    }
  }
  
  if (length(died) != 0) {
    died <- stringr::str_extract(died, "\\d{4}-\\d{2}-\\d{2}|\\d+ \\w+ \\d{4}|\\d{2} \\w+,\\d{4}|\\d{4}")
    died <- gsub(",", " ", died)
    
    if (grepl(" ", died)) {
      died <- as.character(as.Date(died, format = "%d %B %Y"))
    } else if (!grepl("-", died)) {
      died <- paste0(died, "-01-01")
    }
  }
  
  if (length(dob) == 0) {
    dob <- NA
  }
  
  if (length(died) == 0) {
    died <- NA
  }
  
  return(data.frame(Born = dob, Died = died, Remarks = ""))
}

#' get_authors_dob_dod
#' @title Get Authors Date of Birth and Death
#' @description Get authors date of birth and death from the authors' Wikipedia
#' page.
#' @author Pushker Ravindra
#' @param language_data a data.frame containing list of Sahitya Akademi award
#' winners with the following information: Year, Author, Work, Category.
#' @return A data.frame containing Year, Author, Work, Category, Date of Birth,
#' Date of Death, Remarks.
#' @examples
#' language_data <- get_sahitya_akademi_winners("Hindi")[1:5, ]
#' get_authors_dob_dod(language_data)
#' @import rvest
#' @import dplyr
#' @export
get_authors_dob_dod <- function(language_data) {
  authors <- language_data$Author
  
  authors_dob_died <- do.call(rbind, lapply(authors, FUN = function(author) {
    if (length(author) == 0) {
      return(data.frame(Born = NA, Died = NA, Remarks = ""))
    }
    
    author_html <- get_author_html(author)
    
    if (is.na(author_html)) {
      return(data.frame(Born = NA, Died = NA, Remarks = ""))
    }
    
    author_tables <- author_html %>% rvest::html_nodes("table")
    author_nodes <- author_tables[grep("infobox", author_tables)]
    
    author_info <- author_nodes[1] %>% rvest::html_table(fill = TRUE)
    
    if (length(author_info) == 0) {
      # Page doesn't exist - For example, Cho.Dharman, or in some cases
      # infobox block doesn't exist
      get_dob_from_description(author_html)
    } else {
      author_info_df <- author_info[[1]]
      
      if (ncol(author_info_df) != 2) {
        return(data.frame(Born = NA, Died = NA))
      }
      
      names(author_info_df) <- c("Field", "Value")
      
      get_dob_from_infobox(author_info_df)
    }
  }))
  
  authors_data <- cbind(language_data, authors_dob_died)
  
  return(authors_data)
}

#' get_sahitya_akademi_languages
#' @title Get Sahitya Akademi Languages
#' @description
#' Sahitya Akademi Award is given in 24 languages. Return those 24 languages. 
#' @author Pushker Ravindra
#' @return A character vector of length 24.
#' @examples
#' get_sahitya_akademi_languages
#' @export
get_sahitya_akademi_languages <- function() {
  default_languages <- c("Kannada", "Konkani", "Maithili", "Assamese",
                         "Bengali", "English", "Rajasthani", "Santali", "Bodo",
                         "Dogri", "Gujarati", "Hindi", "Kashmiri", "Malayalam",
                         "Meitei", "Marathi", "Nepali", "Odia", "Punjabi",
                         "Sanskrit", "Sindhi", "Tamil", "Telugu", "Urdu")
  default_languages
}

#' is_sahitya_akademi_language
#' @title Is Sahitya Akademi Language
#' @description
#' Check whether Sahitya Akademi Award is given in the provided language(s). Sahitya Akademi Award is given in 24 languages. 
#' @author Pushker Ravindra
#' @param languages a character vector, for example, 'English'.
#' @return TRUE or FALSE
#' @examples
#' is_sahitya_akademi_language(c("English", "Hindi", "Kannada"))
#' is_sahitya_akademi_language("Spanish")
#' @export
is_sahitya_akademi_language <- function(languages) {
  default_languages <- get_sahitya_akademi_languages()
  
  for (language in languages) {
    if (!(language %in% default_languages)) {
      return(FALSE)
    }
  }
  
  TRUE
}

#' get_sahitya_akademi_languages_data
#' @title Get Sahitya Akademi Languages Data
#' @description
#' Get Sahitya Akademi data for the given languages. For each language, the data is stored in a CSV file and for given language(s), the data is fetched and aggregated to provide data for the given languages. 
#' @author Pushker Ravindra
#' @param languages a character vector, for example, 'English'.
#' @return A data.frame
#' @examples
#' get_sahitya_akademi_languages_data(c("English", "Hindi", "Kannada"))
#' @export
get_sahitya_akademi_languages_data <- function(languages) {
  if (!is_sahitya_akademi_language(languages)) {
    stop(paste0("There is no Sahitya Akademi Award for one of the languages in ",
         languages, " yet."))
  }
  authors_data_all <- do.call(rbind, lapply(languages, FUN = function(lang) {
    file <- paste0("List_of_Sahitya_Akademi_Award_winners_for_", lang,
                   "_dob_age.csv")
    authors_data <- read.csv(file, stringsAsFactors = FALSE)
    
    if (!lang %in% names(authors_data)) {
      authors_data$Language <- lang
    }
    
    return(authors_data)
  }))
  
}