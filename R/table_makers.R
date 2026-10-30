

#' #' @title Word highlighting
#' #' @description Finds most common words in string
#' #' @param string the string to evaluate
#' #' @param n the number of words to find
#' #' @param print whether or not to print the highlighted words
#' #' @return a vector of strings
#' #'
#' #' @export
#' find_highlight <- function(string, n = 3, print = F) {
#'   stop_words <- tlShiny::stop_words |>
#'     dplyr::bind_rows(data.frame(word = tlShiny::na_df,
#'                                     lexicon = "TL_NA"))
#'
#'   txt_df <- data.frame(column_name = "txt") |>
#'
#'   highlight <- string |>
#'     na.omit() |>
#'     as.data.frame() |>
#'     dplyr::rename() |>
#'     tidytext::unnest_tokens(word, txt) |>
#'     # This makes sure to get rid of numbers in consideration for highlighting
#'     # By making sure as.numeric returns NA on words
#'     dplyr::filter(is.na(as.numeric(word))) |>
#'     # Get a count of words and sort
#'     dplyr::count(word, sort = T) |>
#'     # Get rid of generic (stop) words
#'     dplyr::anti_join(stop_words) |>
#'     # Get a user-specified number of words, or the default 3
#'     utils::head(n) |>
#'     # Make this a vector
#'     dplyr::pull(word) |>
#'     # Suppress warnings from the as.numeric call
#'     suppressWarnings() |>
#'     suppressMessages()
#'
#'   if (print == T) {
#'     print(highlight)
#'   }
#'
#'   return(highlight)
#' }

#' @title Word highlighting
#' @description Provides html formatted highlighting
#' @param data the data to highlight
#' @param highlight the words to highlight
#' @return a vector of strings
#'
#' @export
highlight_fun <- function(data, highlight = tlShiny::find_highlight(data)) {

  # If the word is not plural then add highlighting for the plural version of the same word,
  plural_highlights <- unlist(
    purrr::map(highlight, ~ if (stringr::str_sub(.x, -1, -1)[1] != "s") {
      paste0(.x, "s")
    })
  )

  # Also do the inverse
  not_plural_highlights <- unlist(
    purrr::map(highlight, ~ if (stringr::str_sub(.x, -1, -1)[1] == "s") {
      stringr::str_remove(.x, "s$")
    })
  )
  # If the word is not capitalized then add highlighting for the capitalized version of the same word,
  capital_highlights <- unlist(
    purrr::map(highlight, ~ if (stringr::str_detect(stringr::str_sub(.x, 1, 1)[1], "[:upper:]")) {
      stringr::str_to_lower(.x)
    })
  )
  # Also do the inverse
  not_capital_highlights <- unlist(
    purrr::map(highlight, ~ if (stringr::str_detect(stringr::str_sub(.x, 1, 1)[1], "[:lower:]")) {
      stringr::str_to_title(.x)
    })
  )
  # Add plurals, capitalization to list and ensure uniqueness
  highlight <- append(highlight,
                      c(plural_highlights,
                        not_plural_highlights,
                        capital_highlights,
                        not_capital_highlights)) |>
    unique()

  # Create a vector for replacement with format <html>new_name</html> = old_name
  replacement_vector <- stats::setNames(
    paste0(
      "<span style='color:#04abeb; font-weight:bold;'>",
      highlight,
      "</span>"
    ),
    highlight
  )

  # Use replace_all on the original `string with the replacement vector created above
  highlighted_string <- stringr::str_replace_all(
    data,
    replacement_vector
  )

  return(highlighted_string)
}



#' @title Quote Visualization
#' @description takes a dataframe and makes a gt table or ggplot that shows a quote
#' @param data the dataframe
#' @param text_col columns to create table for
#' @param viz_type ggplot or gt visualization
#' @param title the title of the ggplot or gt
#' @param custom_highlight a vector, optional custom highlighting
#' @param n integer, number of words to auto-highlight
#' @param print T, whether or not to print the highlighted words to console
#' @param width The width of the table generated
#' @param suppress_warnings T/F suppression of warnings
#' @param align the table alignment: "left", "center", "right"
#' @param ... Arguments passed onto the gt table
#' @return a ggplot/gt that visualizes text
#'
#' @examples
#' \dontrun{
#' df <- tlShiny::survey_monkey
#' colnames(df)[1] <- "What learning are you excited to try?"
#' quote_viz(
#'   data = df,
#'   text_col = "What learning are you excited to try?",
#'   viz_type = "gt",
#'   title = "Responses from Survey Monkey"
#' )
#' }
#' @export

quote_viz <- function(data,
                      text_col = colnames(data)[1],
                      viz_type = "gt",
                      custom_highlight = NULL,
                      n = 3,
                      print = T,
                      width = 60,
                      title = NULL,
                      suppress_warnings = T,
                      align = "center",
                      save = T,
                      ...) {
  selecting_cols <- text_col
  text_col <- rlang::enquo(text_col)

  if (viz_type == "ggplot") {
    data |>
      dplyr::mutate(text = stringr::str_replace_all(stringr::str_wrap(.data[[text_col]], width = 60), "\n", "<br>")) |>
      dplyr::mutate(text = paste0("\"<i>", text, "\"")) |>
      dplyr::mutate(
        x = 0,
        y = dplyr::row_number()
      ) |>
      ggplot2::ggplot() +
      ggtext::geom_richtext(
        fill = NA, label.color = NA, family = "Lora",
        ggplot2::aes(label = text, x = x, y = y)
      ) +
      ggplot2::scale_y_discrete(expand = c(0, 0.3)) +
      ggplot2::theme_void() +
      ggplot2::theme(text = ggplot2::element_text(family = "Lora"))
  } else if (viz_type == "gt") {
    if (is.data.frame(data)) {
      data <- as.data.frame(data)
      selecting_cols <- "value"
    }

    # If not custom highlighting, find a specified number of words to highlight, n = 3 by default
    if (is.null(custom_highlight)) {
      highlight <- purrr::map_dfc(selecting_cols, ~ tlShiny::find_highlight(string = data |>
                                                                                  dplyr::pull(.x), n = n)) |>
        suppressMessages()
      highlight <- purrr::map_chr(1:length(highlight), ~ paste0("highlight", .x)) |>
        stats::setNames(highlight, nm = .)
    } else if (is.character(custom_highlight)) {
      highlight <- custom_highlight # Custom highlighting
    }

    # Print out highlights for reference
    if (print == T) {
      print(highlight)
    }

    # Select just the relevant columns for highlighting
    data_text <- data |>
      dplyr::select(!!text_col)

    # Get all highlights as just one vector
    all_highlights <- highlight |>
      tidyr::pivot_longer(dplyr::everything(), values_to = "highlight") |>
      dplyr::pull(highlight)

    # First map function applies the highlighting column by column
    # Second map function sorts the data so that the highlighted words are on top
    # Issue: this allows for overlay of html tags since it doesn't all occur at once,
    # NEED TO PREVENT <br> and <span> from getting highlighted
    data_final <- purrr::map2_dfc(data_text[selecting_cols], 1:length(colnames(highlight)), ~
                                    tlShiny::highlight_fun(
                                      tlShiny::html_wrap(.x, n = width),
                                      highlight |> dplyr::pull(.y)
                                    )) %>% purrr::map_df(., ~ append(
                                      sort(as.character(factor(.x, levels = unique(.x[stringr::str_detect(.x, "<span")])))),
                                      .x[!stringr::str_detect(.x, "<span")]
                                    ))

    if (save == F) {
      data_final
    } else {
      # Make gt table with all HTML Formatting
      data_final |>
        tlShiny::remove_empty_tl() |>
        dplyr::filter(dplyr::if_any(dplyr::everything(), ~ .x %!in% tlShiny::na_df)) |>
        gt::gt() %>%
        {
          if (!is.null(title)) gt::tab_header(data = ., title = gt::md(paste0("**", title, "**"))) else .
        } %>%
        gt::fmt_markdown(columns = gt::everything()) |>
        gt::sub_missing(columns = everything(),
                        missing_text = " ") |>
        gt::cols_align(align = align) |>
        gt::tab_style(
          style = list(
            gt::cell_text(
              size = "medium"
            )
          ),
          locations = gt::cells_body(
            columns = gt::everything(),
            rows = gt::everything()
          )
        ) |>
        gt::opt_row_striping(row_striping = TRUE) |>
        tlShiny::gt_theme_tl(align = align, ...)
    }

  }
}

#' gtable_remove_grob
#'
#' Helper function to remove grobs by name, from gtables
#'
#' @param g, gtable with the grob removed
#' @param pattern grob name or pattern to match
#'
#' @return g, with pattern removed.
gtable_remove_grob <- function(g, pattern = "guide-box") {
  matches <- c(grepl(pattern = pattern, g$layout$name))

  g$layout <- g$layout[!matches, , drop = FALSE]

  g$grobs <- g$grobs[!matches]
  return(g)
}

#' gtable_extract_grob
#'
#' Helper function to extract a grob from gtables by name.
#'
#' @param g, the gtable to extract the grob
#' @param pattern, grob name or pattern to match
#'
#' @return g, a grob matching the specified pattern
gtable_extract_grob <- function(g, pattern = "guide-box") {
  matches <- grepl(pattern = pattern, g$layout$name)

  g$layout <- g$layout[matches, , drop = FALSE]

  g$grobs <- g$grobs[matches]
  return(g)
}












