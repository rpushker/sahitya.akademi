#' plot_sahitya_akademi_age
#' @title Get the Date of Birth of an Author from the Description
#' @description
#' Get the date of birth of an author from the description when the info box is
#' not present.
#' @author Ravindra Pushker
#' @param languages a character vector containing any of the 24 languages.
#' @param type a character vector of length 1 for the type of plot whether box or violin.
#' @param ztheme a logical vector of length 1 for applying ztheme.
#' @return A ggplot object containing box/violin plot. 
#' @examples
#' plot_sahitya_akademi_age(c("Hindi", "English", "Kannada"))
#' @import ggplot2
#' @export
plot_sahitya_akademi_age <- function(languages, type = "box", ztheme = TRUE) {


  p <- ggplot2::ggplot(aes(x = Language, y = Age.at.Sahitya.Academy,
                           fill = Language), data = authors_data_all)

  if (type == "box") {
    p <- p + ggplot2::geom_boxplot(aes(fill = Language), alpha = .3)
  } else if (type == "violin") {
    p <- p + ggplot2::geom_violin(aes(fill = Language), alpha = .3)
  } else {
    stop(paste0("The plot type - ", type, " not implemented yet. Provide only ",
                "box or violin as type."))
  }

  p <- p + ggplot2::geom_jitter(aes(color = Language), size = 3, alpha = .4) +
    scale_y_continuous(breaks = seq(0, 100, 10)) +
    guides(fill = FALSE, color = FALSE) +
    labs(title = "Sahitya Akademi Awards - Age Distribution",
         x = "Language",
         y = "Age")

  if (ztheme == TRUE) {
    p <- p + apply_z_theme()
  }

  ggplot2::ggsave(paste0("sahitya_akademi_age_", type, ".png"), height = 5,
                  width = 8, dpi = 240, type = "cairo-png")

  p
}

plot_sahitya_akademi_type <- function(authors_data_all, ztheme = TRUE) {
  authors_data_all_type <- aggregate(Work ~ Language + Type.of.Work,
                                     FUN = length, data = authors_data_all)
  names(authors_data_all_type) <- c("Language", "Type", "Count")

  p <- ggplot2::ggplot(data = authors_data_all_type, aes(fill = Type, y = Count,
                                                         x = Language)) +
    geom_bar(position = "stack", stat = "identity")

  if (ztheme == TRUE) {
    p <- p + apply_z_theme()
  }

  p
}
