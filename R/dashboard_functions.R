#' @title Remove Empty Rows
#' @description Filters out all rows that contain entirely NA
#' @param data the dataframe to apply filter
#' @return a dataframe with no empty rows
#' @export
remove_empty_tl <- function(data) {
  data |>
    dplyr::select(dplyr::where(~ !all(is.na(.x))))
}


#' @title Negative Conditional Filter
#' @description Conditionally filters value given that it is not the first parameter,
#' for use in shiny apps
#' @param data the dataframe to apply filter
#' @param filter_this Filter for this, only filters if length >= 1
#' @param dat_filter Data column object to filter
#' @return filtered data.frame
#' @export
neg_cond_filter <- function(data, filter_this, dat_filter) {

  # Check if any of the filters are not the "All x" pattern and filter for the inputs if that is TRUE
  if (length(filter_this) > 1) {
    collapse::fsubset(data, data[[dat_filter]] %in% filter_this)
  } else if (length(filter_this) == 1 && filter_this != "") {
    collapse::fsubset(data, data[[dat_filter]] == filter_this)
  } else {
    data
  }

}

#' @title Date filter
#' @description Efficiently filters data frame by two dates (inclusive),
#' for use in shiny apps
#' @param data the dataframe to apply filter to
#' @param date1 the min date
#' @param date2 the max date
#' @param dat_filter Data column object to filter
#' @return filtered data.frame
#' @export
date_filter <- function(data, date1, date2, dat_filter) {

  collapse::fsubset(data, data[[dat_filter]] >= date1 & data[[dat_filter]] <= date2)

}

#' @title Verify email domain
#' @description Verifies that an email is of a specified domain
#' @param email the email provided
#' @param domain the domain to check that it is from
#' @return TRUE or FALSE
#' @export
check_email_domain <- function(email, domain) {
  grepl(paste0("@", domain, "$"), email, ignore.case = TRUE)
}


#' @title Verify email from vector
#' @description Verifies that an email is in a list
#' @param email the email provided
#' @param approved_emails_list the email to check the list for
#' @return TRUE or FALSE
#' @export
check_email_approved <- function(email, approved_emails_list) {
  email %in% approved_emails_list
}


#' @title HTML/CSS Button Content Expander
#' @description Creates a button that will expand or hide content
#' @param before button default, collapsed/not collapsed
#' @param options unclear
#' @param envir also unclear
#' @param name chunk name
#' @return html wrapper
#' @export
drop1 <- function(before = T, options, envir, name) {
  if (before) {
    paste(
      "<p>",
      glue::glue('<button class="btn btn-primary collapsed" data-toggle="collapse" data-target="{name}">'),
      "</button>",
      "</p>",
      glue::glue('<div class="collapse" id="{name}">'),
      '<div class="card card-body">',
      sep = "\n"
    )
  } else {
    paste("</div>", "</div>", sep = "\n")
  }
}

#' @title HTML Text Wrapping
#' @description Takes a string and inserts <br> at the requested intervals
#' @param string the string
#' @param n the width of the string before a <br> tag
#' @return the same string with <br> inserted at the requested interval
#'
#' @examples
#' html_wrap("a random string that has about 40 characters in it")
#' @export

html_wrap <- function(string, n = 40) {
  stringr::str_replace_all(
    stringr::str_wrap(string = string, width = n),
    "\n", "<br>"
  )
}


#' @title Find elements x not in a vector y
#'
#' @name notin
#' @aliases notin
#' @param x A vector of what shouldn't exist
#' @param y A vector to check against
#' @return Returns elements not in vector
#' @export

"%!in%" <- function(x, y) {
  !("%in%"(x, y))
}


#' @title Calculate nps score
#'
#' @param x A vector of nps scores
#' @return Returns the nps score
#' With formula `%` promoters - `%` detractors where promoters are 9 or 10 ratings and detractors are 0 to 6
#' @export

calc_nps <- function(x) {
  nps <- round((
    (sum(x == 10 | x == 9, na.rm = T) / sum(!is.na(x))) -
      (sum(x == 6 | x == 5 | x == 4 | x == 3 | x == 2 | x == 1 | x == 0, na.rm = T) / sum(!is.na(x)))
  ) * 100, 2)
  return(nps)
}



#' @title Agree/Strongly agree
#' @description Gets the percent that agree and strongly agree
#' @param data the data
#' @param question a string - the question to get the percentage for
#' @examples
#' \dontrun{
#' plot_agree |>
#'   agree_strongly_agree(question = "x")
#' }
#' @return a string
#' @export
agree_strongly_agree <- function(data, question) {
  data |>
    dplyr::filter(Question == question & Response %in% c("(4) Agree", "(5) Strongly agree")) |>
    dplyr::summarise(Percent = sum(Percent)) |>
    dplyr::pull(Percent) |>
    round() |>
    paste0("%")
}

#' @title File path
#' @description Gives the file path without double slash bug
#' @param ... The file path
#' @param fsep the file separation
#' @return fp a file path
#' @export
file.path2 <- function(..., fsep = .Platform$file.sep) {
  fp <- gsub("//", "/", file.path(..., fsep = fsep))
  return(fp)
}

#' @title No Data Plot
#' @description A plot that says no data available, and to check your filters
#' @export
no_data_plot_filters <- ggplot2::ggplot(data.frame(text = "No data available for this set of filters,\ncheck what you have set!", x = 0, y = 0)) +
  ggplot2::geom_text(ggplot2::aes(label = text, x, y), fontface = "bold", family = "Calibri Bold", size = 10, color = "black") +
  ggplot2::theme_void()

#' @title No Data Plot
#' @description A plot that says no data available yet this year
#' @export
no_data_plot_currently <- ggplot2::ggplot(data.frame(text = "No data available yet this year!", x = 0, y = 0)) +
  ggplot2::geom_text(ggplot2::aes(label = text, x, y), fontface = "bold", family = "Calibri Bold", size = 10, color = "black") +
  ggplot2::theme_void()

#' @title No Data Plot Adjustable Title
#' @description A plot that says no data available by default, but can be adjusted to say whatever you want
#' @export
no_data_plot_custom <- function(title = "No data available!") {
  ggplot2::ggplot(data.frame(text = title, x = 0, y = 0)) +
  ggplot2::geom_text(ggplot2::aes(label = text, x, y), fontface = "bold", family = "Calibri Bold", size = 10, color = "black") +
  ggplot2::theme_void()
}

#' @title GT or ggplot maker
#' @description makes a gt table with percent and n colored
#' @param df the data frame
#' @param column the column to get count and percent from
#' @param custom_title the title for the table
#' @param no_title make the table have no title
#' @param base_font overall table font size
#' @param heading_font title font size
#' @param custom_column_name a custom name for the column
#' @param viz_type gt by default, also has ggplot options like pie chart, waffle, or treemap
#' @return a gt table
#' @export
gt_percent_n <- function(df, column, custom_title, no_title = T, base_font = 10,
                         heading_font = 14, custom_column_name = "", viz_type = "gt") {
  if (nrow(df) >= 1) {
    column <- rlang::sym(column)

    if (viz_type == "gt") {
      df |>
        dplyr::group_by(!!column) |>
        dplyr::count(sort = T) |>
        tidyr::drop_na(!!column) |>
        dplyr::ungroup() |>
        dplyr::mutate(Percent = round(100 * n / sum(n), 2)) |>
        dplyr::rename({{ custom_column_name }} := {{ column }}) |>
        gt::gt() |>
        gt::cols_label({{ custom_column_name }} := gt::html(custom_column_name)) %>%
        {
          if (no_title == F) gt::tab_header(title = gt::md(glue::glue("*{custom_title}*"))) else .
        } %>%
        gt::data_color(
          columns = n,
          colors = scales::col_numeric(
            palette = tlShiny::tl_palette(color = "blue", n = 10),
            domain = NULL
          )
        ) |>
        gt::fmt_percent(
          columns = Percent,
          decimals = 2,
          scale_values = F
        ) |>
        gt::grand_summary_rows(
          columns = c(n),
          fns = list(
            Total = ~ sum(.)
          ),
          formatter = gt::fmt_number,
          decimals = 0
        ) |>
        gt::grand_summary_rows(
          columns = c(Percent),
          fns = list(
            Total = ~ sum(.)
          ),
          formatter = gt::fmt_percent,
          scale_values = F,
          decimals = 0
        ) %>%
        tlShiny::gt_theme_tl(base_font = base_font, heading_font = heading_font)
    } else if (viz_type == "pie") {
      ggplot_data <- df |>
        dplyr::group_by(!!column) |>
        dplyr::count(sort = T) |>
        tidyr::drop_na(!!column) |>
        dplyr::ungroup() |>
        dplyr::mutate(!!(column) := stringr::str_wrap(!!rlang::sym(column), width = 10),
          Percent = round(100 * n / sum(n), 2)
        ) |>
        dplyr::rename({{ custom_column_name }} := {{ column }}) |>
        dplyr::mutate(
          prop = 100 * (Percent / sum(Percent)),
          ypos = cumsum(prop) - 0.5 * prop,
          {{ custom_column_name }} := forcats::fct_reorder(!!rlang::ensym(custom_column_name), Percent)
        )
      ggplot_data |>
        ggplot2::ggplot(ggplot2::aes(
          x = "", y = Percent,
          fill = !!rlang::ensym(custom_column_name)
        )) +
        ggplot2::geom_col(key_glyph = ggplot2::draw_key_point) +
        ggplot2::geom_text(
          ggplot2::aes(
            label = ifelse(Percent > 5, paste0(Percent, "%"), ""),
            y = ypos
          ),
          family = "Calibri Bold",
          fontface = "bold",
          color = ifelse(min(ggplot_data$Percent) == ggplot_data$Percent | ggplot_data$Percent < 10,
            "white",
            "black"
          ),
          size = ifelse(min(ggplot_data$Percent) == ggplot_data$Percent | ggplot_data$Percent < 10,
            8,
            11
          ),
          vjust = ifelse(min(ggplot_data$Percent) == ggplot_data$Percent, -1.5, 0.5),
          show.legend = FALSE
        ) +
        ggplot2::coord_polar("y", start = 0) +
        ggplot2::labs(title = paste0(custom_column_name, " (n = ", format(sum(ggplot_data$n, na.rm = T), big.mark = ","), ")")) +
        ggplot2::scale_fill_manual(values = tlShiny::tl_palette(
          color = "blue",
          n = length(unique(ggplot_data[[custom_column_name]]))
        )) +
        ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(shape = 21, size = 10), reverse = T)) +
        ggplot2::theme_void(base_family = "Calibri") +
        ggplot2::theme(
          legend.position = "bottom",
          legend.text = ggplot2::element_text(size = 20),
          legend.title = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 30, family = "Calibri Bold"),
          legend.spacing.x = ggplot2::unit(1.0, 'cm')
        )
    } else if (viz_type == "waffle") {
      ggplot_data <- df |>
        dplyr::group_by(!!column) |>
        dplyr::count(sort = T) |>
        tidyr::drop_na(!!column) |>
        dplyr::ungroup() |>
        dplyr::mutate(Percent = round(100 * n / sum(n), 2)) |>
        dplyr::rename({{ custom_column_name }} := {{ column }}) |>
        dplyr::mutate(
          prop = 100 * (Percent / sum(Percent)),
          ypos = cumsum(prop) - 0.75 * prop,
          {{ custom_column_name }} := forcats::fct_reorder(!!rlang::ensym(custom_column_name), Percent)
        )

      subtitle <- ggplot_data |>
        dplyr::select({{ custom_column_name }}, Percent, n) |>
        dplyr::arrange(dplyr::desc(Percent)) |>
        dplyr::mutate(color = rev(tlShiny::tl_palette(n = length(n), color = "blue"))) |>
        dplyr::summarise(text = stringr::str_c("<b style='color:", color, ";white-space:pre;'>", !!rlang::ensym(custom_column_name), ": ", Percent, "%</b>", collapse = "    ")) |>
        dplyr::pull(text)

      ggplot_data |>
        ggplot2::ggplot(ggplot2::aes(
          fill = !!rlang::ensym(custom_column_name),
          values = n
        )) +
        ### ISSUE: some kind of weird error here internal to the library itself I think ###
        waffle::geom_waffle(
          n_rows = 10,
          size = 1, colour = "white",
          make_proportional = TRUE,
          na.rm = TRUE,
          radius = grid::unit(2, "pt"),
          height = 0.9,
          width = 0.9
        ) +
        ggplot2::labs(
          title = paste0(custom_column_name, " (n = ", format(sum(ggplot_data$n, na.rm = TRUE), big.mark = ","), ")"),
          subtitle = subtitle
        ) +
        ggplot2::scale_fill_manual(values = tlShiny::tl_palette(
          color = "blue",
          n = length(unique(ggplot_data[[custom_column_name]]))
        )) +
        ggplot2::theme_void(base_family = "Calibri") +
        ggplot2::theme(
          legend.position = "none",
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 30, family = "Calibri Bold"),
          plot.subtitle = ggtext::element_markdown(
            hjust = 0.5, face = "italic",
            lineheight = 1.15, size = 22,
            family = "Calibri"
          )
        )
    } else if (viz_type == "treemap") {
      ggplot_data <- df |>
        dplyr::group_by(!!column) |>
        dplyr::count(sort = T) |>
        tidyr::drop_na(!!column) |>
        dplyr::ungroup() |>
        dplyr::mutate(Percent = round(100 * n / sum(n), 2)) |>
        dplyr::rename({{ custom_column_name }} := {{ column }}) |>
        dplyr::mutate(
          prop = 100 * (Percent / sum(Percent)),
          ypos = cumsum(prop) - 0.5 * prop,
          {{ custom_column_name }} := forcats::fct_reorder(!!rlang::ensym(custom_column_name), Percent)
        )
      ggplot_data |>
        ggplot2::ggplot(ggplot2::aes(
          area = Percent,
          fill = !!rlang::ensym(custom_column_name)
        )) +
        treemapify::geom_treemap(key_glyph = ggplot2::draw_key_point) +
        treemapify::geom_treemap_text(ggplot2::aes(label = paste0(!!rlang::ensym(custom_column_name), ": ", Percent, "%")),
          family = "Calibri Bold",
          fontface = "bold",
          # grow = T,
          reflow = T,
          color = ifelse(min(ggplot_data$Percent) == ggplot_data$Percent | ggplot_data$Percent < 10,
            "white",
            "black"
          ),
          place = "center"
        ) +
        ggplot2::labs(title = paste0(custom_column_name, " (n = ", format(sum(ggplot_data$n, na.rm = T), big.mark = ","), ")\n")) +
        ggplot2::scale_fill_manual(values = tlShiny::tl_palette(
          color = "blue",
          n = length(unique(ggplot_data[[custom_column_name]]))
        )) +
        ggplot2::theme_void(base_family = "Calibri") +
        ggplot2::theme(
          legend.position = "none",
          legend.title = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 30, family = "Calibri Bold")
        )
    }
  } else {
    tlShiny:::no_data_plot_filters
  }
}

#' @title Grade IPG Data
#' @param x the data
#' @param type character or numeric
#' @description function for grading different parts of the ipg forms
#' @return a percentage of correct either by checking 3 or 4 or yes
#' @export

grade_ipg <- function(x, type = "character") {
  x <- x[!is.na(x)]
  x <- x[!is.null(x)]
  x <- x[!grepl(x = x, pattern = "Not Observed|Not observed|NULL|NA|N/A|Not applicable|Did not observe", fixed = TRUE)]
  # purrr::keep( ~ !is.null(.x)) %>%
  # purrr::keep( ~ !str_detect(.x, "Not Observed"))

  if (type == "character") {
    yes_count <- sum(grepl("Yes", x), na.rm = TRUE)
    no_count <- sum(grepl("No", x), na.rm = TRUE)
    x <- 100 * yes_count / (no_count + yes_count)
  } else if (type == "numeric") {
    match_count <- sum(grepl("3|4", x), na.rm = TRUE)
    non_match_count <- length(x) - match_count
    x <- 100 * match_count / (non_match_count + match_count)
  } else if (type == "numeric_low") {
    match_count <- sum(grepl("2|3", x), na.rm = TRUE)
    non_match_count <- length(x) - match_count
    x <- 100 * match_count / (non_match_count + match_count)
  }

  x
}

#' @title Get the percent of a column that equals specific values
#' @description Automatically scaled stacked bar chart with TL theming
#' @param data the data for the plotter to use, should include all columns of interest
#' @param percent_equal string inputs to find the percent of the column that equals those values
#' @return a percentage as a string
#' @export

tl_select_percent <- function(data, percent_equal) {
  sum_correct <- data |>
    table() |>
    magrittr::extract(percent_equal) |>
    sum(na.rm = T)

  sum_table <- sum(!is.na(data))

  sum_correct / sum_table
}


#' @title Student percent agree/strongly agree
#' @description Automatically dodged bar chart for student data
#' @param data the data for the plotter to use, should include all columns of interest
#' @param title the title for the plot
#' @param string_remove NULL by default, provides an optional string removal
#' @param col_select the columns to select with `tidyselect::contains`
#' @param agree_select the type of agree/strongly agree to select, for example also often/always
#' @param legend_position c(0.8, 0.25) by default, adjustable
#' @return a ggplot object
#' @export

student_bar_chart <- function(data,
                              col_select,
                              agree_select,
                              string_remove,
                              title,
                              legend_position = c(0.8, 0.25)) {
  n_size_1 <- format(sum(!is.na(data |> dplyr::filter(prepost == "Pre") |> dplyr::select(tidyselect::contains(col_select)) |> dplyr::pull(1))), big.mark = ",")
  n_size_2 <- format(sum(!is.na(data |> dplyr::filter(prepost == "Post") |> dplyr::select(tidyselect::contains(col_select)) |> dplyr::pull(1))), big.mark = ",")

  ### Makes race column, selects relevant columns and gets percent
  ### that selected relevant levels of agreeness
  negative_data <- data |>
    (\(.) if (col_select == "mses_a6") dplyr::select(., mses_a6_3, mses_a6_9, mses_a6_1, mses_a6_8, prepost) else .)() |>
    (\(.) if (col_select == "happiness_belonging") dplyr::select(., happiness_belonging_3, prepost) else .)() |>
    dplyr::group_by(prepost) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ tlShiny::tl_select_percent(.x, c("1 - Not at all like me 👎👎", "2 - Not much like me 👎", "1 - Disagree", "2 - Somewhat disagree")))) |>
    dplyr::ungroup() |>
    tidyr::drop_na(prepost)

  student_data_summarised <- data |>
    dplyr::select(tidyselect::contains(col_select), prepost) |>
    dplyr::group_by(prepost) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ tlShiny::tl_select_percent(.x, agree_select))) |>
    tidyr::drop_na(prepost) |>
    (\(.) if (ncol(.) > 2) dplyr::mutate(., Overall = rowMeans(dplyr::select(., -prepost))) else .)()

  if (col_select == "happiness_belonging") {
    student_data_summarised$Overall[student_data_summarised$prepost == "Pre"] <- mean(c(negative_data$happiness_belonging_3[negative_data$prepost == "Pre"], subset(student_data_summarised, select = -c(happiness_belonging_3, prepost))[1, ] |> as.vector() |> as.numeric()))
    if ("Post" %in% student_data_summarised$prepost) {
      student_data_summarised$Overall[student_data_summarised$prepost == "Post"] <- mean(c(negative_data$happiness_belonging_3[negative_data$prepost == "Post"], subset(student_data_summarised, select = -c(happiness_belonging_3, prepost))[2, ] |> as.vector() |> as.numeric()))
    }
  }

  if (col_select == "mses_a6") {
    student_data_summarised$Overall[student_data_summarised$prepost == "Pre"] <- mean(c(as.numeric(negative_data[1, ][c("mses_a6_3", "mses_a6_9", "mses_a6_1", "mses_a6_8")]), subset(student_data_summarised, select = -c(mses_a6_3, mses_a6_9, mses_a6_1, mses_a6_8, prepost))[1, ] |> as.vector() |> as.numeric()), na.rm = TRUE)
    if ("Post" %in% student_data_summarised$prepost) {
      student_data_summarised$Overall[student_data_summarised$prepost == "Post"] <- mean(c(as.numeric(negative_data[2, ][c("mses_a6_3", "mses_a6_9", "mses_a6_1", "mses_a6_8")]), subset(student_data_summarised, select = -c(mses_a6_3, mses_a6_9, mses_a6_1, mses_a6_8, prepost))[2, ] |> as.vector() |> as.numeric()), na.rm = TRUE)
    }
  }

  ### Reformat dataframe and prep for ggplot2
  student_data_percent <- student_data_summarised |>
    tidyr::pivot_longer(!prepost, names_to = "question", values_to = "percent") |>
    tidyr::drop_na(percent) |>
    dplyr::mutate(
      prepost = factor(prepost, levels = c("Pre", "Post")),
      negative = ifelse(question %in% c("mses_a6_3", "mses_a6_9", "mses_a6_1", "mses_a6_8", "happiness_belonging_3"), TRUE, FALSE),
      question = stringr::str_replace_all(question, c(
        "crse_1" = "My teacher explains what we are learning in different ways",
        "crse_2" = "My teacher wants students from different cultures to respect one another",
        "crse_3" = "My teacher uses what I already know to help me understand new ideas",
        "crse_4" = "My teacher uses examples from my culture when teaching",
        "crse_5" = "My teacher asks about ways that students’ cultures may be different from others",
        "crse_6" = "My teacher helps students learn about other students and their cultures",
        "crse_7" = "My teacher asks about students’ home life",
        "crse_8" = "My teacher treats all students like they are important members of the classroom",
        "teacher_student_rel_1" = "My teacher makes me feel that he/she really cares about me",
        "teacher_student_rel_2" = "I like the way my teacher treats me when I need help",
        "teacher_student_rel_3" = "My teacher seems to know if something is bothering me",
        "self_efficacy_1" = "I can do almost all the work in this class if I don’t give up",
        "self_efficacy_2" = "Even when work is hard, I know I can learn it",
        "self_efficacy_3" = "I'm certain I can master the skills taught in this class",
        "self_efficacy_4" = "When doing work for this class, I focus on learning, not the time work takes",
        "self_efficacy_5" = "I have been able to figure out the most difficult work in this class",
        "happiness_belonging_1" = "This class is a happy place for me to be",
        "happiness_belonging_2" = "In this class, I feel like I belong",
        "happiness_belonging_3" = "Being in this class makes me feel sad or angry",
        "happiness_belonging_4" = "The things we have done in class this year are interesting",
        "happiness_belonging_5" = "Because of this teacher, I am learning to love this subject",
        "happiness_belonging_6" = "I enjoy this subject this year",
        "being_challenged_1" = "My teacher makes sure that I try to do my best",
        "being_challenged_2" = "In this class, we learn a lot almost every day",
        "being_challenged_3" = "In this class, we learn to correct our mistakes",
        "being_challenged_4" = "In this class, my teacher accepts nothing less than our full effort",
        "being_challenged_5" = "My teacher wants us to use our thinking skills, not just memorize things",
        "growth_mindsets_a1_1" = "If you want to succeed in math, hard work alone just won’t cut it; you need to have a natural gift or talent",
        "growth_mindsets_a1_2" = "You have a certain amount of intelligence, and you really can’t do much to change it",
        "growth_mindsets_a1_3" = "When you have to try really hard in a subject in school, it means you can’t be good at that subject",
        "growth_mindsets_a1_4" = "Being a “math person” or not is something that you really can’t change. Some people are good at math and other people aren’t",
        "growth_mindsets_a2" = "My math teacher thinks failure helps us learn and grow",
        "growth_mindsets_a3" = "My math teacher believes that everybody in my class can be very good at math",
        "self_efficacy_a4_1" = "I usually do well in math",
        "self_efficacy_a4_2" = "I am good at working out difficult math problems",
        "self_efficacy_a4_3" = "I believe that I can be successful in my math class",
        "self_efficacy_a4_4" = "I am confident that I can understand the material in my math class",
        "self_efficacy_a4_5" = "I know I can learn the materials in my math class",
        "math_enjoyment_a5_1" = "I enjoy learning math",
        "math_enjoyment_a5_2" = "I learn many interesting things in math",
        "math_enjoyment_a5_3" = "I like to solve math problems",
        "math_enjoyment_a5_4" = "I like math",
        "mses_a6_11" = "I stay focused in math class",
        "mses_a6_12" = "I feel good when I am in math class",
        "mses_a6_13" = "I try to understand my mistakes when I get something wrong",
        "mses_a6_4" = "I think about different ways to solve a problem",
        "mses_a6_5" = "I try to connect what I am learning to things I have learned before",
        "mses_a6_6" = "I want to understand what is learned in math",
        "mses_a6_7" = "I talk about math outside of class",
        "mses_a6_8" = "I do other things when I am supposed to be paying attention",
        "mses_a6_9" = "I would rather be told the answer than have to do the work",
        "mses_a6_10" = "I keep trying even if something is hard",
        "mses_a6_1" = "I don't think that hard when I am doing work for class",
        "mses_a6_2" = "I go through the work for math class and make sure that it's right",
        "mses_a6_3" = "When work is hard, I only study the easy parts",
        "math_a7_1" = "My math teacher uses examples of students’ different cultures/backgrounds/families in their lessons",
        "math_a7_2" = "My math teacher respects my culture and background",
        "high_exp_one_1" = "In this class, people don't give up when the work gets hard.",
        "high_exp_one_2" = "I feel like I have access to all of the opportunities this class offers.",
        "high_exp_one_3" = "In this class, it feels like I’m expected -- and supported -- to learn a ton.",
        "high_exp_two" = "When you feel like giving up on a difficult task, how likely is it that your teacher will help you keep trying?",
        "rig_learn_one_1" = "In this class we use our thinking skills, in addition to memorizing things.",
        "rig_learn_one_2" = "In this class we have time to explain our ideas.",
        "rig_learn_two" = "In this class I get to develop my own ideas.",
        "relevance_one" = "In this class what I’m learning matters a lot to me.",
        "relevance_two_1" = "In this class what we learn is often connected to life outside the classroom.",
        "relevance_two_2" = "In this class I get to learn things I'm interested in.",
        "affirm_1" = "In this class it feels like being yourself is a great thing. I feel safe and appreciated for who I am.",
        "affirm_2" = "In this class I feel proud of who I am.",
        "affirm_3" = "I can be myself in this class",
        "connect_one_1" = "I feel part of the community [in this class].",
        "connect_one_2" = "In this class I feel included by other students.",
        "connect_two" = "Overall, how much do you feel like you belong in this class?",
        "custom_one" = "In this class, I have the resources I need to support my learning.",
        "custom_two_1" = "In this class, I do work that meets me where I am in my learning.",
        "custom_two_2" = "In this class I am able to catch up if I am behind.",
        "asd_one" = "In this class I feel like I have a say about what happens to me.",
        "asd_two_1" = "In this class I can choose how to do my work.",
        "asd_two_2" = "In this class I have goals for my learning, and I have choices about how I pursue those goals.",
        "asd_two_3" = "My teacher(s) respect(s) my ideas and suggestions.",
        "overall_experience_1" = "Overall, most of the time, I love this class.",
        "overall_experience_2" = "Overall, most of the time, I’m learning a lot in this class."
      )),
      question = stringr::str_remove_all(question, string_remove),
      question = tlShiny::html_wrap(question, 35),
      question = ifelse(negative == TRUE, paste0("<span style = 'color:red;'>", question, "</span>"), paste0("<span style = 'color:black;'>", question, "</span>")),
      percent = percent * 100
    )

  subtitle <- if (length(agree_select) > 1) {
    glue::glue('The following percentages show the % that selected<br>"{agree_select[1]}" or "{agree_select[2]}"')
  } else {
    glue::glue('The following percentages show the % that selected<br>"{agree_select[1]}"')
  }

  if (col_select == "mses_a6") {
    subtitle <- paste0(subtitle, " or \"1 - Not at all like me 👎👎\", \"2 - Not much like me 👎\" in the case of negatively coded questions <span style = 'color:red;'>(red)</span>")
  }

  p <- ggplot2::ggplot(student_data_percent, ggplot2::aes(
    x = forcats::fct_relevel(forcats::fct_reorder(question, percent, .desc = TRUE), "<span style = 'color:black;'>Overall</span>", after = length(question)),
    y = percent, fill = prepost
  )) +
    ggplot2::geom_col(position = ggplot2::position_dodge2(reverse = TRUE),
                      ggplot2::aes(group = prepost)) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = paste0(round(percent), "%")
      ),
      size = 9,
      fontface = "bold",
      color = "black",
      position = ggplot2::position_dodge2(width = 1, preserve = "total", reverse = TRUE),
      hjust = -0.25,
      family = "Lora",
      show.legend = FALSE
    ) +
    ggplot2::labs(
      x = "", y = "",
      title = glue::glue("{title}<br><span style = 'color:#04abeb;'>(pre n = {n_size_1})</span> & (post n = {n_size_2})"),
      subtitle = subtitle,
      fill = "Race"
    ) +
    ggplot2::scale_fill_manual(values = c("Post" = "#040404", "Pre" = "#04ABEB")) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(reverse = TRUE),
      color = "none"
    ) +
    ggplot2::coord_flip() +
    # tlShiny::theme_tl(legend = F,
    #                   markdown = TRUE) +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(family = "Poppins", face = "bold", size = 30, hjust = 0.5, color = "black"),
      plot.subtitle = ggtext::element_markdown(family = "Poppins", hjust = 0, size = 20, color = "black", lineheight = 0.8),
      legend.position = "none",
      axis.text.y = ggtext::element_markdown(size = 23, family = "Lora"),
      axis.text.x = ggtext::element_markdown(size = 18, color = "black", family = "Lora"),
      plot.background = ggplot2::element_rect(fill = "white"),
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.grid.minor.x = ggplot2::element_line(color = "gray40"),
      panel.grid.major.y = ggplot2::element_line(color = "gray90")
    )

  suppressWarnings(print(p))
}

#' @title End of session feedback graph dependent on race and content area
#' @description Returns a barchart for selections in the relevant questions of the end of coaching survey
#' @param data the data to use
#' @return prints a ggplot object
#' @export
session_feedback_graph <- function(data) {
  session_survey <- data |>
    tidyr::drop_na(coach_ongoing_feed_1)

  ### Get second facilitator responses ###
  second_fac <- session_survey |>
    dplyr::select(
      `They demonstrated deep knowledge of the content they facilitated` = coach_ongoing_feed_2_1,
      `Their facilitation or coaching is clear` = coach_ongoing_feed_2_2,
      `They effectively built a safe learning environment` = coach_ongoing_feed_2_3,
      `They seemed fully prepared for the session` = coach_ongoing_feed_2_4,
      `They made adjustments based on participant needs` = coach_ongoing_feed_2_5
    )


  ### Get first facilitator combined with second agree per question for end of session survey ###
  plot_agree <- session_survey |>
    dplyr::select(
      `They demonstrated deep knowledge of the content they facilitated` = coach_ongoing_feed_1,
      `Their facilitation or coaching is clear` = coach_ongoing_feed_2,
      `They effectively built a safe learning environment` = coach_ongoing_feed_3,
      `They seemed fully prepared for the session` = coach_ongoing_feed_4,
      `They made adjustments based on participant needs` = coach_ongoing_feed_5
    ) |>
    dplyr::bind_rows(second_fac) |>
    tidyr::pivot_longer(tidyr::everything(), names_to = "Question", values_to = "Response") |>
    tidyr::drop_na() |>
    dplyr::group_by(Question, Response) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::group_by(Question) |>
    dplyr::mutate(Question = stringr::str_replace_all(Question, c(
      "They demonstrated deep knowledge of the content they facilitated" = "They demonstrated deep knowledge of\nthe content they facilitated",
      "They effectively built a safe learning environment" = "They effectively built a safe learning\nenvironment",
      "They seemed fully prepared for the session" = "They seemed fully prepared for the\nsession",
      "They made adjustments based on participant needs" = "They made adjustments based on\nparticipant needs"
    ))) |>
    dplyr::reframe(
      n = n,
      Response = Response,
      Percent = n / sum(n) * 100
    )

  if (nrow(plot_agree) >= 1) {
    ### Calculate n size ###
    n_size_agree <- session_survey |>
      dplyr::count(sort = T)

    ### Make ggplot of session survey agree percent ###
    p <- plot_agree |>
      dplyr::group_by(Question, Response) |>
      dplyr::summarise(Percent = weighted.mean(Percent, n)) |>
      dplyr::mutate(
        Question = factor(Question, levels = c(
          "They demonstrated deep knowledge of\nthe content they facilitated",
          "Their facilitation or coaching is clear",
          "They seemed fully prepared for the\nsession",
          "They effectively built a safe learning\nenvironment",
          "They made adjustments based on\nparticipant needs"
        )),
        Response = stringr::str_replace_all(Response, c(
          "Neither agree nor disagree" = "Neither agree\nnor disagree",
          "Strongly agree" = "Strongly\nagree",
          "Strongly disagree" = "Strongly\ndisagree"
        ))
      ) |>
      ggplot2::ggplot(ggplot2::aes(x = Question, y = Percent, fill = factor(Response))) +
      ggplot2::geom_col(color = NA, width = 0.95, position = ggplot2::position_stack(reverse = TRUE)) +
      ggplot2::geom_text(
        ggplot2::aes(
          label = dplyr::if_else(Percent >= 10, paste0(round(Percent), "%"), ""),
          color = Response
        ),
        size = 6,
        position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE),
        fontface = "bold",
        family = "Calibri Bold",
        show.legend = FALSE
      ) +
      ggplot2::scale_fill_manual(values = c(
        "1 - Strongly\ndisagree" = "#040404", "2 - Disagree" = "#032E3F",
        "3 - Neither agree\nnor disagree" = "#02587A", "4 - Agree" = "#0182B4", "5 - Strongly\nagree" = "#00ACF0"
      )) +
      ggplot2::scale_color_manual(values = c(
        "1 - Strongly\ndisagree" = "white", "2 - Disagree" = "white",
        "3 - Neither agree\nnor disagree" = "black", "4 - Agree" = "black", "5 - Strongly\nagree" = "black"
      )) +
      ggplot2::labs(
        fill = "", title = glue::glue("Participant Perceptions of Facilitation (n = {format(sum(n_size_agree$n), big.mark = ',')})"),
        x = "", y = ""
      ) +
      ggplot2::coord_flip() +
      ggplot2::guides(
        fill = ggplot2::guide_legend(),
        color = "none"
      ) +
      ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      ggplot2::scale_x_discrete(limits = rev) +
      tlShiny::theme_tl(legend = TRUE) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = 15.5, margin = margin(t = 0, l = 0, r = -10, b = 0), lineheight = 0.7),
        axis.text.x = ggplot2::element_text(size = 9),
        plot.title = ggplot2::element_text(size = 22, face = "bold", family = "Calibri Bold"),
        legend.text = ggplot2::element_text(size = 13.5, lineheight = 0.7, color = "black", family = "Calibri"),
        legend.spacing = ggplot2::unit(0.8, "cm"),
        legend.key.width = ggplot2::unit(0.8, "cm"),
        legend.key.height = ggplot2::unit(0.4, "cm"),
        legend.position = "bottom",
        legend.margin = ggplot2::margin(t = -20, l = -120, r = 0, b = 0)
      )

    p
  } else {
    tlShiny::no_data_plot_custom(title = paste0("No End of Session Data as of ", format.Date(Sys.Date(), "%b %d, %Y")))
  }
}

#' @title Course survey feedback graph dependent on race and content area
#' @description Returns a barchart for selections in the relevant questions of the end of coaching survey
#' @param data the data to use
#' @return prints a ggplot object
#' @export
course_feedback_graph <- function(data) {
  course_survey <- data |>
    tidyr::drop_na(coach_end_feed_1)

  ### Get first facilitator combined with second agree per question for end of course survey ###
  plot_agree <- course_survey |>
    dplyr::select(
      `I looked forward to attending this PL` = coach_end_feed_1,
      `I was fully present/"minds-on" during these PL sessions` = coach_end_feed_4,
      `The activities were well-designed to help me meet the learning targets` = coach_end_feed_15,
      `I am satisfied with how the sessions were facilitated` = coach_end_feed_5,
      `This PL was a good use of my time` = coach_end_feed_6,
      `I talk to other teachers about the things I learned in this PL` = coach_end_feed_7,
      `I felt a sense of community with the other participants in this course` = coach_end_feed_16,
      `The PL was relevant to my instructional practices` = coach_end_feed_8,
      `The strategies I’ve learned will improve my instruction` = coach_end_feed_9,
      `The strategies I’ve learned will improve my coaching or supervision of teachers` = coach_end_feed_10,
      `I have applied or will apply what I have learned to my practice` = coach_end_feed_11,
      `The PL has supported me in being responsive to students' backgrounds, cultures, and points of view.` = coach_end_feed_12,
      `I am satisfied with the overall quality of this PL` = coach_end_feed_13
    ) |>
    tidyr::pivot_longer(tidyr::everything(), names_to = "Question", values_to = "Response") |>
    tidyr::drop_na() |>
    dplyr::group_by(Question, Response) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::group_by(Question) |>
    dplyr::mutate(Question = stringr::str_replace_all(Question, c(
      "I was fully present/\"minds-on\" during these PL sessions" = "I was fully present/\"minds-on\"\nduring these PL sessions",
      "The activities were well-designed to help me meet the learning targets" = "The activities were well-designed\nto help me meet the learning targets",
      "I am satisfied with how the sessions were facilitated" = "I am satisfied with how the sessions\nwere facilitated",
      "I talk to other teachers about the things I learned in this PL" = "I talk to other teachers about the\nthings I learned in this PL",
      "I felt a sense of community with the other participants in this course" = "I felt a sense of community with\nthe other participants in this course",
      "The PL was relevant to my instructional practices" = "The PL was relevant to my\ninstructional practices",
      "The strategies I’ve learned will improve my instruction" = "The strategies I’ve learned will\nimprove my instruction",
      "The strategies I’ve learned will improve my coaching or supervision of teachers" = "The strategies I’ve learned will\nimprove my coaching or supervision",
      "I have applied or will apply what I have learned to my practice" = "I have applied or will apply what\nI have learned to my practice",
      "The PL has supported me in being responsive to students' backgrounds, cultures, and points of view." = "The PL has supported me in being\nresponsive to students' backgrounds",
      "I am satisfied with the overall quality of this PL" = "I am satisfied with the overall\nquality of this PL"
    ))) |>
    dplyr::reframe(
      n = n,
      Response = Response,
      Percent = n / sum(n) * 100
    )

  if (nrow(plot_agree) >= 1) {
    ### Calculate n size ###
    n_size_agree <- course_survey |>
      dplyr::count(sort = T)

    ### Make ggplot of course survey agree percent ###
    p <- plot_agree |>
      dplyr::group_by(Question, Response) |>
      dplyr::summarise(Percent = weighted.mean(Percent, n)) |>
      dplyr::mutate(
        Question = factor(Question, levels = c(
          "I looked forward to attending this PL",
          "I was fully present/\"minds-on\"\nduring these PL sessions",
          "The activities were well-designed\nto help me meet the learning targets",
          "I am satisfied with how the sessions\nwere facilitated",
          "This PL was a good use of my time",
          "I talk to other teachers about the\nthings I learned in this PL",
          "I felt a sense of community with\nthe other participants in this course",
          "The PL was relevant to my\ninstructional practices",
          "The strategies I’ve learned will\nimprove my instruction",
          "The strategies I’ve learned will\nimprove my coaching or supervision",
          "I have applied or will apply what\nI have learned to my practice",
          "The PL has supported me in being\nresponsive to students' backgrounds",
          "I am satisfied with the overall\nquality of this PL"
        )),
        Response = stringr::str_replace_all(Response, c(
          "Neither agree nor disagree" = "Neither agree\nnor disagree",
          "Strongly agree" = "Strongly\nagree",
          "Strongly disagree" = "Strongly\ndisagree"
        ))
      ) |>
      ggplot2::ggplot(ggplot2::aes(x = Question, y = Percent, fill = factor(Response))) +
      ggplot2::geom_col(color = NA, width = 0.95, position = ggplot2::position_stack(reverse = TRUE)) +
      ggplot2::geom_text(
        ggplot2::aes(
          label = dplyr::if_else(Percent >= 10, paste0(round(Percent), "%"), ""),
          color = Response
        ),
        size = 6,
        position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE),
        fontface = "bold",
        family = "Calibri Bold",
        show.legend = FALSE
      ) +
      ggplot2::scale_fill_manual(values = c(
        "1 - Strongly\ndisagree" = "#040404", "2 - Disagree" = "#032E3F",
        "3 - Neither agree\nnor disagree" = "#02587A", "4 - Agree" = "#0182B4", "5 - Strongly\nagree" = "#00ACF0"
      )) +
      ggplot2::scale_color_manual(values = c(
        "1 - Strongly\ndisagree" = "white", "2 - Disagree" = "white",
        "3 - Neither agree\nnor disagree" = "black", "4 - Agree" = "black", "5 - Strongly\nagree" = "black"
      )) +
      ggplot2::labs(
        fill = "", title = glue::glue("Participant Perceptions of Course (n = {format(sum(n_size_agree$n), big.mark = ',')})"),
        x = "", y = ""
      ) +
      ggplot2::coord_flip() +
      ggplot2::guides(
        fill = ggplot2::guide_legend(),
        color = "none"
      ) +
      ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      ggplot2::scale_x_discrete(limits = rev) +
      tlShiny::theme_tl(legend = TRUE) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = 15.5, margin = margin(t = 0, l = 0, r = -10, b = 0), lineheight = 0.7),
        axis.text.x = ggplot2::element_text(size = 9),
        plot.title = ggplot2::element_text(size = 22, face = "bold", family = "Calibri Bold", hjust = 0.5),
        legend.text = ggplot2::element_text(size = 13.5, lineheight = 0.7, color = "black", family = "Calibri"),
        legend.spacing = ggplot2::unit(0.8, "cm"),
        legend.margin = ggplot2::margin(t = -20, l = -120, r = 0, b = 0),
        legend.key.width = ggplot2::unit(0.8, "cm"),
        legend.key.height = ggplot2::unit(0.4, "cm"),
        legend.position = "bottom"
      )

    p
  } else {
    tlShiny::no_data_plot_custom(title = paste0("No End of Course Data as of ", format.Date(Sys.Date(), "%b %d, %Y")))
  }
}

#' @title Ongoing Coaching feedback graph dependent on race and content area
#' @description Returns a barchart for selections in the relevant questions of the end of coaching survey
#' @param data the data to use
#' @return prints a ggplot object
#' @export
ongoing_coaching_feedback_graph <- function(data) {
  ongoing_coaching <- data |>
    tidyr::drop_na(coach_ongoing_feed_1)

  coaching_plot_agree <- ongoing_coaching |>
    dplyr::select(
      `They demonstrated deep knowledge of the content they coach` = coach_ongoing_feed_1,
      `Their coaching is clear` = coach_ongoing_feed_2,
      `They seem fully prepared for the coaching sessions` = coach_ongoing_feed_3,
      `They effectively build a safe learning environment` = coach_ongoing_feed_4,
      `They make necessary adjustments based on my needs` = coach_ongoing_feed_5
    ) |>
    tidyr::pivot_longer(tidyr::everything(), names_to = "Question", values_to = "Response") |>
    tidyr::drop_na(Response) |>
    dplyr::group_by(Question, Response) |>
    dplyr::count(sort = T) |>
    dplyr::ungroup() |>
    dplyr::group_by(Question) |>
    dplyr::mutate(
      Percent = round(100 * n / sum(n), 2),
      Response = stringr::str_replace_all(Response, c(
        "Neither agree nor disagree" = "Neither agree\nnor disagree",
        "Strongly agree" = "Strongly\nagree",
        "Strongly disagree" = "Strongly\ndisagree"
      )),
      Response = factor(Response, levels = c(
        "1 - Strongly\ndisagree",
        "2 - Disagree",
        "3 - Neither agree\nnor disagree",
        "4 - Agree",
        "5 - Strongly\nagree"
      )),
      Question = stringr::str_replace_all(
        Question,
        c(
          "They demonstrated deep knowledge of the content they coach" = "They demonstrated deep knowledge\nof the content they coach",
          "They seem fully prepared for the coaching sessions" = "They seem fully prepared for the\ncoaching sessions",
          "They effectively build a safe learning environment" = "They effectively build a safe\nlearning environment",
          "They make necessary adjustments based on my needs" = "They make necessary adjustments based\non my needs"
        )
      )
    )

  if (nrow(coaching_plot_agree) >= 1) {
    n_size_agree <- ongoing_coaching |>
      dplyr::count(sort = T)


    p <- coaching_plot_agree |>
      dplyr::mutate(
        Percent = round(100 * n / sum(n), 2),
        Question = factor(Question, levels = c(
          "They demonstrated deep knowledge\nof the content they coach",
          "Their coaching is clear",
          "They seem fully prepared for the\ncoaching sessions",
          "They effectively build a safe\nlearning environment",
          "They make necessary adjustments based\non my needs"
        ))
      ) |>
      ggplot2::ggplot(ggplot2::aes(
        x = Question,
        y = Percent,
        color = Response,
        fill = Response
      )) +
      ggplot2::geom_col(color = NA, position = ggplot2::position_stack(reverse = TRUE)) +
      ggplot2::geom_text(
        ggplot2::aes(
          label = dplyr::if_else(Percent >= 10, paste0(round(Percent), "%"), ""),
          color = Response
        ),
        position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE),
        fontface = "bold",
        size = 6,
        family = "Calibri Bold",
        show.legend = FALSE
      ) +
      ggplot2::labs(
        x = "", y = "",
        title = glue::glue("Ongoing Coaching Participant Feedback (n = {sum(n_size_agree$n, na.rm = T)})"),
        fill = ""
      ) +
      ggplot2::scale_fill_manual(values = c(
        "1 - Strongly\ndisagree" = "#040404",
        "2 - Disagree" = "#032E3F",
        "3 - Neither agree\nnor disagree" = "#02587A",
        "4 - Agree" = "#0182B4",
        "5 - Strongly\nagree" = "#00ACF0"
      )) +
      ggplot2::scale_color_manual(values = c(
        "1 - Strongly\ndisagree" = "white",
        "2 - Disagree" = "black",
        "3 - Neither agree\nnor disagree" = "black",
        "4 - Agree" = "black",
        "5 - Strongly\nagree" = "black"
      )) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(),
        color = "none"
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::label_percent(scale = 1),
        expand = c(0.14, 0)
      ) +
      ggplot2::scale_x_discrete(limits = rev) +
      ggplot2::coord_flip() +
      tlShiny::theme_tl(legend = TRUE) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = 15.5, margin = margin(t = 0, l = 0, r = -10, b = 0), lineheight = 0.7),
        axis.text.x = ggplot2::element_text(size = 9),
        plot.title = ggplot2::element_text(size = 22, face = "bold", family = "Calibri Bold"),
        legend.text = ggplot2::element_text(size = 13.5, lineheight = 0.7, color = "black", family = "Calibri"),
        legend.spacing = ggplot2::unit(0.8, "cm"),
        legend.key.width = ggplot2::unit(0.8, "cm"),
        legend.key.height = ggplot2::unit(0.4, "cm"),
        legend.position = "bottom",
        legend.margin = ggplot2::margin(t = -20, l = -120, r = 0, b = 0)
      )

    p
  } else {
    tlShiny::no_data_plot_custom(title = paste0("No Ongoing Coaching Data as of ", format.Date(Sys.Date(), "%b %d, %Y")))
  }
}

#' @title End of Coaching feedback graph dependent on race and content area
#' @description Returns a barchart for selections in the relevant questions of the end of coaching survey
#' @param data the data to use
#' @return prints a ggplot object
#' @export
end_coaching_feedback_graph <- function(data) {
  end_coaching <- data |>
    tidyr::drop_na(coach_end_feed_1)

  coaching_plot_agree <- end_coaching |>
    dplyr::select(
      `I looked forward to attending this PL` = coach_end_feed_1,
      `I was fully present/"minds-on" during these PL sessions` = coach_end_feed_4,
      `The activities were well-designed to help me meet the learning targets` = coach_end_feed_15,
      `I am satisfied with how the sessions were facilitated` = coach_end_feed_5,
      `This PL was a good use of my time` = coach_end_feed_6,
      `I talk to other teachers about the things I learned in this PL` = coach_end_feed_7,
      `I felt a sense of community with the other participants in this course` = coach_end_feed_16,
      `The PL was relevant to my instructional practices` = coach_end_feed_8,
      `The strategies I’ve learned will improve my instruction` = coach_end_feed_9,
      `The strategies I’ve learned will improve my coaching or supervision of teachers` = coach_end_feed_10,
      `I have applied or will apply what I have learned to my practice` = coach_end_feed_11,
      `The PL has supported me in being responsive to students' backgrounds, cultures, and points of view.` = coach_end_feed_12,
      `I am satisfied with the overall quality of this PL` = coach_end_feed_13
    ) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    tidyr::pivot_longer(tidyr::everything(), names_to = "Question", values_to = "Response") |>
    tidyr::drop_na(Response) |>
    dplyr::group_by(Question, Response) |>
    dplyr::count(sort = T) |>
    dplyr::ungroup() |>
    dplyr::group_by(Question) |>
    dplyr::mutate(
      Percent = round(100 * n / sum(n), 2),
      Response = stringr::str_replace_all(Response, c(
        "Neither agree nor disagree" = "Neither agree\nnor disagree",
        "Strongly agree" = "Strongly\nagree",
        "Strongly disagree" = "Strongly\ndisagree"
      )),
      Response = factor(Response, levels = c(
        "1 - Strongly\ndisagree",
        "2 - Disagree",
        "3 - Neither agree\nnor disagree",
        "4 - Agree",
        "5 - Strongly\nagree"
      )),
      Question = stringr::str_replace_all(Question, c(
        "I was fully present/\"minds-on\" during these PL sessions" = "I was fully present/\"minds-on\"\nduring these PL sessions",
        "The activities were well-designed to help me meet the learning targets" = "The activities were well-designed\nto help me meet the learning targets",
        "I am satisfied with how the sessions were facilitated" = "I am satisfied with how the sessions\nwere facilitated",
        "I talk to other teachers about the things I learned in this PL" = "I talk to other teachers about the\nthings I learned in this PL",
        "I felt a sense of community with the other participants in this course" = "I felt a sense of community with\nthe other participants in this course",
        "The PL was relevant to my instructional practices" = "The PL was relevant to my\ninstructional practices",
        "The strategies I’ve learned will improve my instruction" = "The strategies I’ve learned will\nimprove my instruction",
        "The strategies I’ve learned will improve my coaching or supervision of teachers" = "The strategies I’ve learned will\nimprove my coaching or supervision",
        "I have applied or will apply what I have learned to my practice" = "I have applied or will apply what\nI have learned to my practice",
        "The PL has supported me in being responsive to students' backgrounds, cultures, and points of view." = "The PL has supported me in being\nresponsive to students' backgrounds",
        "I am satisfied with the overall quality of this PL" = "I am satisfied with the overall\nquality of this PL"
      ))
    )

  if (nrow(coaching_plot_agree) >= 1) {
    n_size_agree <- end_coaching |>
      dplyr::count(sort = T)

    p <- coaching_plot_agree |>
      dplyr::mutate(
        Percent = round(100 * n / sum(n), 2),
        Question = factor(Question, levels = c(
          "I looked forward to attending this PL",
          "I was fully present/\"minds-on\"\nduring these PL sessions",
          "The activities were well-designed\nto help me meet the learning targets",
          "I am satisfied with how the sessions\nwere facilitated",
          "This PL was a good use of my time",
          "I talk to other teachers about the\nthings I learned in this PL",
          "I felt a sense of community with\nthe other participants in this course",
          "The PL was relevant to my\ninstructional practices",
          "The strategies I’ve learned will\nimprove my instruction",
          "The strategies I’ve learned will\nimprove my coaching or supervision",
          "I have applied or will apply what\nI have learned to my practice",
          "The PL has supported me in being\nresponsive to students' backgrounds",
          "I am satisfied with the overall\nquality of this PL"
        ))
      ) |>
      ggplot2::ggplot(ggplot2::aes(
        x = Question,
        y = Percent,
        color = Response,
        fill = Response
      )) +
      ggplot2::geom_col(color = NA, position = ggplot2::position_stack(reverse = TRUE)) +
      ggplot2::geom_text(
        ggplot2::aes(
          label = dplyr::if_else(Percent >= 10, paste0(round(Percent), "%"), ""),
          color = Response
        ),
        position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE),
        fontface = "bold",
        size = 6,
        family = "Calibri Bold",
        show.legend = FALSE
      ) +
      ggplot2::labs(
        x = "", y = "",
        title = glue::glue("End of Coaching Participant Feedback (n = {sum(n_size_agree$n, na.rm = T)})"),
        fill = ""
      ) +
      ggplot2::scale_fill_manual(values = c(
        "1 - Strongly\ndisagree" = "#040404",
        "2 - Disagree" = "#032E3F",
        "3 - Neither agree\nnor disagree" = "#02587A",
        "4 - Agree" = "#0182B4",
        "5 - Strongly\nagree" = "#00ACF0"
      )) +
      ggplot2::scale_color_manual(values = c(
        "1 - Strongly\ndisagree" = "white",
        "2 - Disagree" = "black",
        "3 - Neither agree\nnor disagree" = "black",
        "4 - Agree" = "black",
        "5 - Strongly\nagree" = "black"
      )) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(),
        color = "none"
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::label_percent(scale = 1),
        expand = c(0.14, 0)
      ) +
      ggplot2::scale_x_discrete(limits = rev) +
      ggplot2::coord_flip() +
      tlShiny::theme_tl(legend = TRUE) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = 15,5, margin = margin(t = 0, l = 0, r = -10, b = 0), lineheight = 0.7),
        axis.text.x = ggplot2::element_text(size = 9),
        plot.title = ggplot2::element_text(size = 22, face = "bold", family = "Calibri Bold", hjust = 0.5),
        legend.text = ggplot2::element_text(size = 13.5, lineheight = 0.6, color = "black", family = "Calibri"),
        legend.spacing = ggplot2::unit(0.8, "cm"),
        legend.margin = ggplot2::margin(t = -20, l = -100, r = 0, b = 0),
        legend.key.width = ggplot2::unit(0.8, "cm"),
        legend.key.height = ggplot2::unit(0.4, "cm"),
        legend.position = "bottom"
      )

    p
  } else {
    tlShiny::no_data_plot_custom(title = paste0("No End of Coaching Data as of ", format.Date(Sys.Date(), "%b %d, %Y")))
  }
}



#' @title Get percent positive on any of the student surveys
#' @description Uses column names and finds the percent in each attributed to positive answers (or negative if reverse coded), summarises by prepost
#' @param data the data to use
#' @return a tibble
#' @export
get_percent_positive_student_survey <- function(data) {

  all_data <- data |>
    dplyr::select(tidyselect::matches("crse|teacher_student|self_efficacy|happiness_belonging|being_challenged|growth_mindsets|math_enjoyment|mses|math_a7|high_exp|rig_learn|relevance|affirm|connect_one|custom_one|asd_one|asd_two|overall_experience"), prepost) |>
    dplyr::group_by(prepost) |>
    dplyr::summarise(
      dplyr::across(dplyr::matches("crse"), ~ tlShiny:::tl_select_percent(.x, c("4 - Often", "5 - Always"))),
      dplyr::across(dplyr::matches("happiness_belonging_3"), ~ tlShiny:::tl_select_percent(.x, c("1 - Disagree", "2 - Somewhat disagree"))),
      dplyr::across(dplyr::matches("teacher_student_rel|self_efficacy|happiness_belonging_1|happiness_belonging_2|happiness_belonging_4|happiness_belonging_5|happiness_belonging_6"), ~ tlShiny:::tl_select_percent(.x, c("4 - Somewhat agree", "5 - Agree", "5 - Agree 👍", "6 - Strongly agree 👍👍"))),
      dplyr::across(dplyr::matches("being_challenged"), ~ tlShiny:::tl_select_percent(.x, c("4 - Mostly true", "5 - Totally true"))),
      dplyr::across(dplyr::matches("growth_mindsets_a1"), ~ tlShiny:::tl_select_percent(.x, c("1 - Strongly Disagree  👎👎", "2 - Disagree 👎"))),
      dplyr::across(dplyr::matches("growth_mindsets_a2"), ~ tlShiny:::tl_select_percent(.x, c("Very true 👍", "Extremely true 👍👍"))),
      dplyr::across(dplyr::matches("growth_mindsets_a3"), ~ tlShiny:::tl_select_percent(.x, c("Agree 👍", "Strongly agree 👍👍"))),
      dplyr::across(dplyr::matches("math_enjoyment_a5"), ~ tlShiny:::tl_select_percent(.x, c("1 - Agree a lot 👍👍", "2 - Agree a little 👍"))),
      dplyr::across(c("mses_a6_3", "mses_a6_9", "mses_a6_8", "mses_a6_1"), ~ tlShiny:::tl_select_percent(.x, c("1 - Not at all like me 👎👎", "2 - Not much like me 👎"))),
      dplyr::across(c("mses_a6_2", "mses_a6_4", "mses_a6_5", "mses_a6_6", "mses_a6_7", "mses_a6_9", , "mses_a6_10", , "mses_a6_11", "mses_a6_12", "mses_a6_13"), ~ tlShiny:::tl_select_percent(.x, c("4 - Mostly like me 👍", "5 - Very much like me 👍👍"))),
      dplyr::across(dplyr::matches("math_a7"), ~ tlShiny:::tl_select_percent(.x, c("3 - Some of the time 👍", "4 - Most or all of the time 👍👍"))),
      dplyr::across(dplyr::matches("high_exp_one|rig_learn_one|relevance_two|affirm|connect_one|custom_two|asd_two|overall_experience"), ~ tlShiny:::tl_select_percent(.x, c("4 - Agree 👍", "5 - Strongly Agree 👍 👍"))),
      dplyr::across(dplyr::matches("high_exp_two"), ~ tlShiny:::tl_select_percent(.x, c("Very likely 👍", "Extremely likely 👍👍"))),
      dplyr::across(dplyr::matches("rig_learn_two|relevance_one|custom_one|asd_one"), ~ tlShiny:::tl_select_percent(.x, c("Often 👍", "Almost always 👍👍"))),
      dplyr::across(dplyr::matches("connect_two"), ~ tlShiny:::tl_select_percent(.x, c("Mostly belong 👍", "Completely belong 👍👍"))),
      n = dplyr::n()
    ) |>
    tidyr::drop_na(prepost) |>
    tidyr::pivot_longer(!c(prepost, n), names_to = "grouping", values_to = "score")

  all_data |>
    dplyr::group_by(prepost) |>
    dplyr::reframe(
      score = round(100 * mean(score, na.rm = TRUE), 2),
      n = dplyr::first(n)
    )
}

