#' @title Knowledge Assessment Graph Summary
#' @description Creates a graph specifically for Knowledge Assessments in mid year reports
#' @param data the data
#' @param know_assess_filter the knowledge assessment to make plot for
#' @return a ggplot
#' @export
know_assess_summary <- function(data, know_assess_filter) {

  if (nrow(data) >= 1) {
    plot_data <- data |>
      dplyr::filter(question1 == "Score" & know_assess == know_assess_filter) |>
      dplyr::mutate(percent = 100 * score / max_score) |>
      dplyr::group_by(prepost, know_assess) |>
      dplyr::summarise(
        percent = round(mean(percent, na.rm = T), 2),
        n = dplyr::n()
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        prepost = ifelse(prepost == "pre",
                         "Before",
                         "After"
        ),
        prepost = factor(prepost, levels = c("Before", "After")),
        know_assess = paste0(know_assess, " % Correct")
      ) |>
      print()

    n1 <- plot_data$n[plot_data$prepost == "Before"]
    if (length(plot_data$n[plot_data$prepost == "After"]) != 0) {
      n2 <- plot_data$n[plot_data$prepost == "After"]
    } else {
      n2 <- 0
    }

    p <- plot_data |>
      ggplot2::ggplot(ggplot2::aes(x = prepost, y = percent, fill = prepost)) +
      ggplot2::geom_col() +
      ggplot2::geom_text(ggplot2::aes(label = paste0(round(percent), "%")),
                         vjust = -0.8,
                         color = "black",
                         fontface = "bold",
                         family = "Poppins",
                         size = 20) +
      ggplot2::scale_fill_manual(values = c("Before" = "#D17DF7", "After" = "#55BBC7")) +
      ggplot2::labs(x = "", y = "",
                    title = paste0("<b>", plot_data$know_assess[1], "<br>% Correct <span style='color:#d17df7'>before (n = ", n1, ")</span> and <span style='color:#55bbc7'>after (n = ", n2, ")</span></b>")
      ) +
      ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1), expand = c(0.1, 0),
                                  limits = c(0, 100)) +
      tlShiny::theme_tl() +
      ggplot2::theme(
        plot.title = ggtext::element_markdown(lineheight = 1.1, hjust = 0.5, size = 25, face = "bold", family = "Poppins"),
        legend.position = "none",
        axis.text.x = ggplot2::element_text(face = "bold", size = 25, family = "Lora"),
        axis.text.y = ggplot2::element_text(face = "bold", size = 25, family = "Lora"))

    return(p)
  } else {
    tlShiny::no_data_plot_custom(title = paste0("No Course Assessment Data Available as of ", format.Date(Sys.Date(), "%b %d, %Y")))
  }

}


#' @title Knowledge Assessment Graph Question-level Summary
#' @description Creates a graph specifically for Knowledge Assessments Scored by Question from Qualtrics
#' @param data the data
#' @param know_assess the knowledge assessment to make plot for
#' @return a ggplot
#' @export
know_assess_summary_detailed <- function(data, know_assess) {

  plot_data <- data |>
    dplyr::select(-site) |> # Get rid of site for when there is more than one
    dplyr::group_by(prepost, question) |>
    dplyr::summarise(score = mean(score, na.rm = T)) |>
    dplyr::ungroup() |>
    dplyr::mutate(name = ifelse(prepost == "pre",
                                "Before",
                                "After"),
                  value = score,
                  name = factor(name, levels = c("Before", "After")),
                  question = stringr::str_wrap(question, width = 55)) |>
    dplyr::select(name, value, question)

  ### Make sure no over 100's ###
  plot_data$value <- ifelse(plot_data$value >= 100, 100, plot_data$value)

  title <- stringr::str_to_title(stringr::str_replace_all(know_assess, "_", " ")) |>
    stringr::str_replace_all("Ela", "ELA") |>
    stringr::str_replace_all("Ana", "ANA") |>
    stringr::str_replace_all("Eic", "EIC") # Correct title casing


  p <- plot_data |>
    dplyr::mutate(value = 100 * value) |>
    ggplot2::ggplot(ggplot2::aes(x = name, y = value, fill = name)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(value), "%")),
                       vjust = -0.1,
                       fontface = "bold",
                       family = "Lora",
                       size = 10) +
    ggplot2::facet_wrap( ~ factor(question, levels = c("Total", unique(plot_data$question)[!plot_data$question %in% "Total"]))) +
    ggplot2::scale_fill_manual(values = c("Before" = "#D17DF7", "After" = "#55BBC7")) +
    ggplot2::labs(x = "", y = "",
                  title = paste0("<b>", title, " % Correct <span style='color:#d17df7'>Before</span> and <span style='color:#55bbc7'>After</span> Per Question</b>")
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1), expand = c(0.1, 0),
                                limits = c(0, 100)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(lineheight = 1.1, hjust = 0.5, size = 14),
      legend.position = "none",
      strip.text = ggplot2::element_text(size = 11, face = "bold"),
      axis.text.x = ggplot2::element_text(face = "bold", size = 12),
      axis.text.y = ggplot2::element_text(face = "bold", size = 12))

  return(p)

}

#' @title ELA IPG Chart Summary Maker
#' @description Creates a chart to summarise the ELA IPG based on the round selected
#' @param data the data
#' @param round the round of the IPG for which data should be pulled (can be one of "Baseline (first observation of the year)", "Mid-year (middle of service, if applicable)", "End of year (last observation of the year)", "Other", or "Ongoing")
#' @return a ggplot
#' @export

make_ipg_ela_summary_chart <- function(data, round = "Baseline (first observation of the year)") {

  base_color <- "#040404"
  end_color <- "#04abeb"

  ipg_adjust <- data |>
    dplyr::filter(direct_to_ts_obs == round)

  k_12_ela <- ipg_adjust |>
    dplyr::select(
      k12_ela_ca1a, k12_ela_ca1b, k12_ela_ca1c, # Core Action 1, Yes/No
      k12_ela_ca2a, k12_ela_ca2b, k12_ela_ca2c, k12_ela_ca2d, # Core Action 2, 1-4
      k12_ela_ca3a, k12_ela_ca3b, k12_ela_ca3c, k12_ela_ca3d, k12_ela_ca3e, k12_ela_ca3f
    ) |> # Core Action 3, 1-4
    dplyr::mutate(
      dplyr::across(everything(), ~ as.character(.x))
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      overall_score = mean(
        c(
          tlShiny::grade_ipg(k12_ela_ca1a, type = "character"),
          tlShiny::grade_ipg(k12_ela_ca1b, type = "character"),
          tlShiny::grade_ipg(k12_ela_ca1c, type = "character"),
          tlShiny::grade_ipg(k12_ela_ca2a, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca2b, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca2c, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca2d, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca3a, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca3b, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca3c, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca3d, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca3e, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca3f, type = "numeric")
        ),
        na.rm = TRUE
      ),
      ca1_score = mean(
        c(
          tlShiny::grade_ipg(k12_ela_ca1a, type = "character"),
          tlShiny::grade_ipg(k12_ela_ca1b, type = "character"),
          tlShiny::grade_ipg(k12_ela_ca1c, type = "character")
        ),
        na.rm = TRUE
      ),
      ca2_score = mean(
        c(
          tlShiny::grade_ipg(k12_ela_ca2a, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca2b, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca2c, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca2d, type = "numeric")
        ),
        na.rm = TRUE
      ),
      ca3_score = mean(
        c(
          tlShiny::grade_ipg(k12_ela_ca3a, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca3b, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca3c, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca3d, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca3e, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca3f, type = "numeric")
        ),
        na.rm = TRUE
      )
    ) |>
    dplyr::ungroup() |>
    tidyr::drop_na(overall_score) |>
    dplyr::summarise(
      ca1_n = sum(!is.na(ca1_score)),
      ca1_score = mean(ca1_score, na.rm = TRUE),
      ca2_n = sum(!is.na(ca2_score)),
      ca2_score = mean(ca2_score, na.rm = TRUE),
      ca3_n = sum(!is.na(ca3_score)),
      ca3_score = mean(ca3_score, na.rm = TRUE),
      n = sum(!is.na(overall_score)),
      overall_score = mean(overall_score, na.rm = TRUE)
    ) |>
    tidyr::pivot_longer(cols = dplyr::ends_with("score"), names_to = "Core Action", values_to = "Score") |>
    dplyr::mutate(
      n = case_when(
        `Core Action` == "ca1_score" ~ ca1_n,
        `Core Action` == "ca2_score" ~ ca2_n,
        `Core Action` == "ca3_score" ~ ca3_n,
        `Core Action` == "overall_score" ~ n
      ),
      `Core Action` = stringr::str_replace_all(`Core Action`, c(
        "overall_score" = "Overall",
        "ca1_score" = "Core Action 1",
        "ca2_score" = "Core Action 2",
        "ca3_score" = "Core Action 3"
      ))
    ) |>
    dplyr::select(-dplyr::ends_with("_n")) |>
    dplyr::mutate(
      `Core Action` = factor(`Core Action`, c(
        "Overall",
        "Core Action 1",
        "Core Action 2",
        "Core Action 3"
      ))
    )

  unique_color_n <- length(unique(k_12_ela$`Core Action`))

  ela_plot <- k_12_ela |>
    ggplot2::ggplot(aes(x = `Core Action`, y = Score, fill = `Core Action`)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(Score), "% (n = ", n, ")")),
              fontface = "bold",
              color = "black",
              family = "Poppins",
              vjust = -0.4,
              size = 7.25
    ) +
    ggplot2::scale_fill_manual(values = tlShiny::tl_palette2(n = unique_color_n, base_color_start = base_color, end_color_start = end_color)) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(scale = 1),
      limits = c(0, 105),
      expand = c(0, 12),
      breaks = seq(10, 100, 10)
    ) +
    ggplot2::labs(
      x = "", y = "",
      title = dplyr::case_when(round == "Baseline (first observation of the year)" ~ "% Positive Indicators Baseline K-12 ELA IPG",
                               round == "Mid-year (middle of service, if applicable)" ~ "% Positive Indicators Mid-year K-12 ELA IPG",
                               round == "End of year (last observation of the year)" ~ "% Positive Indicators End of Year K-12 ELA IPG",
                               round == "Ongoing" ~ "% Positive Indicators Ongoing K-12 ELA IPG",
                               round == "Other" ~ "% Positive Indicators Other K-12 ELA IPG"),
      caption = "Note that n sizes represent the number of overall scores per grouping, the actual number of responses that qualify for scores may vary"
    ) +
    tlShiny::theme_tl() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 24),
      axis.text.y = ggplot2::element_text(size = 24),
      plot.title = ggplot2::element_text(face = "bold", family = "Poppins", size = 30),
      plot.caption = ggplot2::element_text(size = 12, face = "italic")
    )

  ela_plot
}


#' @title FSOT IPG Chart Summary Maker
#' @description Creates a chart to summarise the FSOT IPG based on the round selected
#' @param data the data
#' @param round the round of the IPG for which data should be pulled (can be one of "Baseline (first observation of the year)", "Mid-year (middle of service, if applicable)", "End of year (last observation of the year)", "Other", or "Ongoing")
#' @return a ggplot
#' @export

make_ipg_fsot_summary_chart <- function(data, round = "Baseline (first observation of the year)") {

  base_color <- "#040404"
  end_color <- "#04abeb"

  ipg_adjust <- data |>
    dplyr::filter(direct_to_ts_obs == round)

  fsot <- ipg_adjust |>
    dplyr::select(
      fsot_ac1, fsot_ac2, # AC1/AC2, 1-4, Q69 is not 1-4
      fsot_td1, fsot_td2, fsot_td3, fsot_td4, # TD, 1-4
      fsot_sp1, fsot_sp2, fsot_sp3, fsot_sp4, # SP, 1-4
      fsot_ad1, fsot_ad2
    ) |> # AD1/AD2, 1-3
    dplyr::mutate(
      dplyr::across(dplyr::everything(), ~ as.character(.x))
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      overall_score = mean(
        c(
          tlShiny::grade_ipg(fsot_ac1, type = "numeric"),
          tlShiny::grade_ipg(fsot_ac2, type = "numeric"),
          tlShiny::grade_ipg(fsot_td1, type = "numeric"),
          tlShiny::grade_ipg(fsot_td2, type = "numeric"),
          tlShiny::grade_ipg(fsot_td3, type = "numeric"),
          tlShiny::grade_ipg(fsot_td4, type = "numeric"),
          tlShiny::grade_ipg(fsot_sp1, type = "numeric"),
          tlShiny::grade_ipg(fsot_sp2, type = "numeric"),
          tlShiny::grade_ipg(fsot_sp3, type = "numeric"),
          tlShiny::grade_ipg(fsot_sp4, type = "numeric"),
          tlShiny::grade_ipg(fsot_ad1, type = "numeric_low"),
          tlShiny::grade_ipg(fsot_ad2, type = "numeric_low")
        ),
        na.rm = TRUE
      ),
      ac_score = mean(
        c(
          tlShiny::grade_ipg(fsot_ac1, type = "numeric"),
          tlShiny::grade_ipg(fsot_ac2, type = "numeric")
        ),
        na.rm = TRUE
      ),
      ad_score = mean(
        c(
          tlShiny::grade_ipg(fsot_ad1, type = "numeric_low"),
          tlShiny::grade_ipg(fsot_ad2, type = "numeric_low")
        ),
        na.rm = TRUE
      ),
      sp_score = mean(
        c(
          tlShiny::grade_ipg(fsot_sp1, type = "numeric"),
          tlShiny::grade_ipg(fsot_sp2, type = "numeric"),
          tlShiny::grade_ipg(fsot_sp3, type = "numeric"),
          tlShiny::grade_ipg(fsot_sp4, type = "numeric")
        ),
        na.rm = TRUE
      ),
      td_score = mean(
        c(
          tlShiny::grade_ipg(fsot_td1, type = "numeric"),
          tlShiny::grade_ipg(fsot_td2, type = "numeric"),
          tlShiny::grade_ipg(fsot_td3, type = "numeric"),
          tlShiny::grade_ipg(fsot_td4, type = "numeric")
        ),
        na.rm = TRUE
      )
    ) |>
    dplyr::ungroup() |>
    tidyr::drop_na(overall_score) |>
    dplyr::summarise(
      ac_n = sum(!is.na(ac_score)),
      ac_score = mean(ac_score, na.rm = TRUE),
      ad_n = sum(!is.na(ad_score)),
      ad_score = mean(ad_score, na.rm = TRUE),
      sp_n = sum(!is.na(sp_score)),
      sp_score = mean(sp_score, na.rm = TRUE),
      td_n = sum(!is.na(td_score)),
      td_score = mean(td_score, na.rm = TRUE),
      n = sum(!is.na(overall_score)),
      overall_score = mean(overall_score, na.rm = TRUE)
    ) |>
    tidyr::pivot_longer(cols = tidyr::ends_with("score"), names_to = "Core Action", values_to = "Score") |>
    dplyr::mutate(
      n = dplyr::case_when(
        `Core Action` == "ac_score" ~ ac_n,
        `Core Action` == "ad_score" ~ ad_n,
        `Core Action` == "sp_score" ~ sp_n,
        `Core Action` == "td_score" ~ td_n,
        `Core Action` == "overall_score" ~ n
      ),
      `Core Action` = stringr::str_replace_all(`Core Action`, c(
        "overall_score" = "Overall",
        "ac_score" = "Aligned\nContent",
        "ad_score" = "Assessment\n& Differentiation",
        "sp_score" = "Student\nPractice",
        "td_score" = "Teacher-Directed\nInstruction"
      ))
    ) |>
    dplyr::select(-dplyr::ends_with("_n")) |>
    dplyr::mutate(
      `Core Action` = factor(`Core Action`, c(
        "Overall",
        "Aligned\nContent",
        "Assessment\n& Differentiation",
        "Student\nPractice",
        "Teacher-Directed\nInstruction"
      ))
    )

  fsot_plot <- fsot |>
    ggplot2::ggplot(ggplot2::aes(x = `Core Action`, y = Score, fill = `Core Action`)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(Score), "% (n = ", n, ")")),
                       fontface = "bold",
                       color = "black",
                       family = "Poppins",
                       vjust = -0.4,
                       size = 7.25
    ) +
    ggplot2::scale_fill_manual(values = tlShiny::tl_palette2(n = length(unique(fsot$`Core Action`)), base_color_start = base_color, end_color_start = end_color)) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(scale = 1),
      limits = c(0, 100),
      expand = c(0, 12),
      breaks = seq(10, 100, 10)
    ) +
    ggplot2::labs(
      x = "", y = "",
      title = dplyr::case_when(round == "Baseline (first observation of the year)" ~ "% Positive Indicators\nBaseline FSOT IPG",
                               round == "Mid-year (middle of service, if applicable)" ~ "% Positive Indicators\nMid-year FSOT IPG",
                               round == "End of year (last observation of the year)" ~ "% Positive Indicators\nEnd of Year FSOT IPG",
                               round == "Ongoing" ~ "% Positive Indicators\nOngoing FSOT IPG",
                               round == "Other" ~ "% Positive Indicators\nOther FSOT IPG"),
      caption = "Note that n sizes represent the number of overall scores per grouping, the actual number of responses that qualify for scores may vary"
    ) +
    tlShiny::theme_tl() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 24),
      axis.text.y = ggplot2::element_text(size = 24),
      plot.title = ggplot2::element_text(face = "bold", family = "Poppins", size = 30),
      plot.caption = ggplot2::element_text(size = 12, face = "italic")
    )

  fsot_plot
}

#' @title Math IPG Chart Summary Maker
#' @description Creates a chart to summarise the Math IPG based on the round selected
#' @param data the data
#' @param round the round of the IPG for which data should be pulled (can be one of "Baseline (first observation of the year)", "Mid-year (middle of service, if applicable)", "End of year (last observation of the year)", "Other", or "Ongoing")
#' @return a ggplot
#' @export


make_ipg_math_summary_chart <- function(data, round = "Baseline (first observation of the year)") {

  base_color <- "#040404"
  end_color <- "#04abeb"

  ipg_adjust <- data |>
    dplyr::filter(direct_to_ts_obs == round)

  k_12_math <- ipg_adjust |>
    dplyr::select(
      k12_m_ca1a, k12_m_ca1b, k12_m_ca1c, # Core Action 1 yes-no
      k12_m_ca2a, k12_m_ca2b, k12_m_ca2c, k12_m_ca2d, # Core Action 2 1-4
      k12_m_ca3a, k12_m_ca3b, k12_m_ca3c, k12_m_ca3d, k12_m_ca3e
    ) |> # Core Action 3 1-4
    dplyr::mutate(
      dplyr::across(dplyr::everything(), ~ as.character(.x))
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      overall_score = mean(
        c(
          tlShiny::grade_ipg(k12_m_ca1a, type = "character"),
          tlShiny::grade_ipg(k12_m_ca1b, type = "character"),
          tlShiny::grade_ipg(k12_m_ca1c, type = "character"),
          tlShiny::grade_ipg(k12_m_ca2a, type = "numeric"),
          tlShiny::grade_ipg(k12_m_ca2b, type = "numeric"),
          tlShiny::grade_ipg(k12_m_ca2c, type = "numeric"),
          tlShiny::grade_ipg(k12_m_ca2d, type = "numeric"),
          tlShiny::grade_ipg(k12_m_ca3a, type = "numeric"),
          tlShiny::grade_ipg(k12_m_ca3b, type = "numeric"),
          tlShiny::grade_ipg(k12_m_ca3c, type = "numeric"),
          tlShiny::grade_ipg(k12_m_ca3d, type = "numeric"),
          tlShiny::grade_ipg(k12_m_ca3e, type = "numeric")
        ),
        na.rm = TRUE
      ),
      ca1_score = mean(
        c(
          tlShiny::grade_ipg(k12_m_ca1a, type = "character"),
          tlShiny::grade_ipg(k12_m_ca1b, type = "character"),
          tlShiny::grade_ipg(k12_m_ca1c, type = "character")
        ),
        na.rm = TRUE
      ),
      ca2_score = mean(
        c(
          tlShiny::grade_ipg(k12_m_ca2a, type = "numeric"),
          tlShiny::grade_ipg(k12_m_ca2b, type = "numeric"),
          tlShiny::grade_ipg(k12_m_ca2c, type = "numeric"),
          tlShiny::grade_ipg(k12_m_ca2d, type = "numeric")
        ),
        na.rm = TRUE
      ),
      ca3_score = mean(
        c(
          tlShiny::grade_ipg(k12_m_ca3a, type = "numeric"),
          tlShiny::grade_ipg(k12_m_ca3b, type = "numeric"),
          tlShiny::grade_ipg(k12_m_ca3c, type = "numeric"),
          tlShiny::grade_ipg(k12_m_ca3d, type = "numeric"),
          tlShiny::grade_ipg(k12_m_ca3e, type = "numeric")
        ),
        na.rm = TRUE
      )
    ) |>
    dplyr::ungroup() |>
    tidyr::drop_na(overall_score) |>
    dplyr::summarise(
      ca1_n = sum(!is.na(ca1_score)),
      ca1_score = mean(ca1_score, na.rm = TRUE),
      ca2_n = sum(!is.na(ca2_score)),
      ca2_score = mean(ca2_score, na.rm = TRUE),
      ca3_n = sum(!is.na(ca3_score)),
      ca3_score = mean(ca3_score, na.rm = TRUE),
      n = sum(!is.na(overall_score)),
      overall_score = mean(overall_score, na.rm = TRUE)
    ) |>
    tidyr::pivot_longer(cols = tidyr::ends_with("score"), names_to = "Core Action", values_to = "Score") |>
    dplyr::mutate(
      n = case_when(
        `Core Action` == "ca1_score" ~ ca1_n,
        `Core Action` == "ca2_score" ~ ca2_n,
        `Core Action` == "ca3_score" ~ ca3_n,
        `Core Action` == "overall_score" ~ n
      ),
      `Core Action` = stringr::str_replace_all(`Core Action`, c(
        "overall_score" = "Overall",
        "ca1_score" = "Core Action 1",
        "ca2_score" = "Core Action 2",
        "ca3_score" = "Core Action 3"
      ))
    ) |>
    dplyr::select(-dplyr::ends_with("_n")) |>
    dplyr::mutate(
      `Core Action` = factor(`Core Action`, c(
        "Overall",
        "Core Action 1",
        "Core Action 2",
        "Core Action 3"
      ))
    )

  unique_color_n <- length(unique(k_12_math$`Core Action`))

  math_plot <- k_12_math |>
    ggplot2::ggplot(aes(x = `Core Action`, y = Score, fill = `Core Action`)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(Score), "% (n = ", n, ")")),
              fontface = "bold",
              vjust = -0.4,
              color = "black",
              family = "Poppins",
              size = 7.25
    ) +
    ggplot2::scale_fill_manual(values = tlShiny::tl_palette2(n = unique_color_n, base_color_start = base_color, end_color_start = end_color)) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(scale = 1),
      limits = c(0, 105),
      expand = c(0, 12),
      breaks = seq(10, 100, 10)
    ) +
    ggplot2::labs(
      x = "", y = "",
      title = dplyr::case_when(round == "Baseline (first observation of the year)" ~ "% Positive Indicators Baseline K-12 Math IPG",
                               round == "Mid-year (middle of service, if applicable)" ~ "% Positive Indicators Mid-year K-12 Math IPG",
                               round == "End of year (last observation of the year)" ~ "% Positive Indicators End of Year K-12 Math IPG",
                               round == "Ongoing" ~ "% Positive Indicators Ongoing K-12 Math IPG",
                               round == "Other" ~ "% Positive Indicators Other K-12 Math IPG"),
      caption = "Note that n sizes represent the number of overall scores per grouping, the actual number of responses that qualify for scores may vary"
    ) +
    tlShiny::theme_tl() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 24),
      axis.text.y = ggplot2::element_text(size = 24),
      plot.title = ggplot2::element_text(face = "bold", family = "Poppins", size = 30),
      plot.caption = ggplot2::element_text(size = 12, face = "italic")
    )

  math_plot
}

#' @title Teacher Curriculum Usage Graph
#' @description Creates a chart to summarise the Teacher Curriculum Usage from the Educator Survey
#' @param data the data
#' @return a ggplot
#' @export

make_teacher_curriculum_usage <- function(data) {

  if (nrow(data) >= 1) {
    curriculum_usage <- data |>
      dplyr::select(prepost, dplyr::contains("materials")) |>
      dplyr::pivot_longer(!prepost, names_to = "name", values_to = "value") |>
      tidyr::drop_na(value) |>
      dplyr::mutate(name = stringr::str_replace_all(name, c(
        "materials_1" = "curriculum materials adopted by your district",
        "materials_2" = "materials developed by your school or district",
        "materials_3" = "materials you found on the internet",
        "materials_4" = "materials developed by yourself or with colleagues"
      ))) |>
      dplyr::group_by(name, value, prepost) |>
      dplyr::count(sort = T) |>
      dplyr::ungroup() |>
      tidyr::drop_na(value) |>
      dplyr::mutate(
        name = stringr::str_wrap(name, 25),
        value = stringr::str_wrap(value, 20)
      ) |>
      dplyr::group_by(name, prepost) |>
      dplyr::mutate(
        Percent = round(100 * n / sum(n), 2),
        value = factor(value, levels = c(
          "Never use",
          "Sometimes (once a\nmonth)",
          "Use often (once or\ntwice weekly)",
          "Use everyday"
        ))
      ) |>
      dplyr::filter(value %in% c("Use often (once or\ntwice weekly)", "Use everyday")) |>
      dplyr::summarise(
        Percent = sum(Percent),
        n = sum(n)
      )

    n_size_1 <- sum(!is.na(data$prepost[data$prepost == "Pre"]))
    n_size_2 <- sum(!is.na(data$prepost[data$prepost == "Post"]))

    curriculum_usage |>
      ggplot2::ggplot(ggplot2::aes(
        x = forcats::fct_reorder(name, Percent, .desc = T),
        y = Percent
      )) +
      ggplot2::geom_col(ggplot2::aes(fill = prepost, group = prepost),
                        color = NA, position = ggplot2::position_dodge2(width = 1, reverse = TRUE)
      ) +
      ggplot2::geom_text(
        ggplot2::aes(
          color = prepost,
          label = dplyr::if_else(Percent >= 10, paste0(round(Percent), "%"), "")
        ),
        position = ggplot2::position_dodge2(reverse = TRUE, width = 1),
        hjust = -0.25,
        fontface = "bold",
        size = 6
      ) +
      ggplot2::labs(
        x = "Please indicate the extent to which you use...", y = "",
        title = glue::glue("Teacher Curriculum Usage\n% selected use everyday or use often"),
        fill = ""
      ) +
      ggplot2::scale_fill_manual(values = c(
        "#00ACF0", "black"
      ), labels = c(glue::glue("Pre (n = {n_size_1})"), glue::glue("Post (n = {n_size_2})"))) +
      ggplot2::scale_color_manual(values = c(
        "#00ACF0", "black"
      )) +
      ggplot2::guides(
        fill = ggplot2::guide_legend()
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::label_percent(scale = 1),
        expand = c(0.14, 0)
      ) +
      ggplot2::coord_flip() +
      ggplot2::guides(color = "none") +
      tlShiny::theme_tl(legend = FALSE) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(
          margin = margin(t = 0, l = 0, r = -110, b = 0),
          size = 19
        ),
        axis.title.y = ggplot2::element_text(size = 19),
        axis.text.x = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(size = 25, face = "bold", family = "Poppins"),
        legend.position = "bottom",
        legend.key.height = ggplot2::unit(1.3, "cm"),
        legend.key.width = ggplot2::unit(1.3, "cm"),
        legend.key.size = ggplot2::unit(0.75, "cm"),
        legend.text = ggplot2::element_text(size = 18),
        legend.margin = ggplot2::margin(-25, 0, 0, -150)
      )
  } else {
    tlShiny::no_data_plot_custom(title = paste0("No Teacher Curriculum Usage\nData Available as of ", format.Date(Sys.Date(), "%b %d, %Y")))
  }

}

#' @title Teacher Use of Lessons Graph
#' @description Creates a chart to summarise the teacher use of lessons from district or school-adopted materials
#' @param data the data
#' @return a ggplot
#' @export

make_teacher_lesson_usage <- function(data) {

  if (nrow(data) >= 1) {
    lesson_usage <- data |>
      dplyr::select(prepost, lesson_modifications) |>
      dplyr::mutate(lesson_modifications = as.character(lesson_modifications)) |>
      tidyr::drop_na(lesson_modifications) |>
      dplyr::group_by(lesson_modifications, prepost) |>
      dplyr::count(sort = T) |>
      dplyr::ungroup() |>
      dplyr::group_by(prepost) |>
      dplyr::mutate(
        lesson_modifications = stringr::str_wrap(lesson_modifications, 20),
        Percent = round(100 * n / sum(n), 2),
        lesson_modifications = factor(lesson_modifications, levels = c(
          "with no or few\nmodifications",
          "with modifications\nto less than half of\na lesson plan",
          "with modifications\nto more than half of\na lesson plan",
          "my main materials do\nnot include lesson\nplans or I typically\ncreate my own lesson\nplans"
        ))
      )

    n_size_1 <- sum(!is.na(data$prepost[data$prepost == "Pre"]))
    n_size_2 <- sum(!is.na(data$prepost[data$prepost == "Post"]))

    lesson_usage |>
      ggplot2::ggplot(ggplot2::aes(
        x = prepost,
        y = Percent,
        fill = lesson_modifications
      )) +
      ggplot2::geom_col(ggplot2::aes(group = lesson_modifications),
                        color = NA,
                        position = ggplot2::position_stack(),
                        width = 0.7) +
      ggplot2::geom_text(
        ggplot2::aes(
          color = lesson_modifications,
          label = dplyr::if_else(Percent >= 10, paste0(round(Percent), "%"), "")
        ),
        position = ggplot2::position_stack(vjust = 0.5),
        fontface = "bold",
        family = "Poppins",
        size = 6
      ) +
      ggplot2::labs(
        y = "I typically use lessons with...", x = "",
        title = glue::glue("Teachers' use of lessons from district or\nschool-adopted materials (pre n = {format(n_size_1, big.mark = ',')}, post n = {format(n_size_2, big.mark = ',')})"),
        fill = ""
      ) +
      ggplot2::scale_fill_manual(values = c(
        "with no or few\nmodifications" = "#032E3F",
        "with modifications\nto less than half of\na lesson plan" = "#02587A",
        "with modifications\nto more than half of\na lesson plan" = "#0182B4",
        "my main materials do\nnot include lesson\nplans or I typically\ncreate my own lesson\nplans" = "gray30"
      )) +
      ggplot2::scale_color_manual(values = c(
        "with no or few\nmodifications" = "white",
        "with modifications\nto less than half of\na lesson plan" = "white",
        "with modifications\nto more than half of\na lesson plan" = "black",
        "my main materials do\nnot include lesson\nplans or I typically\ncreate my own lesson\nplans" = "white"
      )) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(reverse = TRUE),
        color = "none"
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::label_percent(scale = 1),
        expand = c(0.14, 0)
      ) +
      ggplot2::coord_flip() +
      tlShiny::theme_tl(legend = TRUE) +
      ggplot2::theme(
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(
          size = 20.5,
          margin = ggplot2::margin(t = 0, l = 0, r = -115, b = 0)
        ),
        axis.text.x = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_text(size = 19, face = "bold", family = "Poppins"),
        legend.position = "bottom",
        legend.text = ggplot2::element_text(size = 19.5, face = "bold", family = "Poppins"),
        legend.key.width = ggplot2::unit(3, "cm"),
        legend.key.height = ggplot2::unit(1, "cm"),
        plot.title = ggplot2::element_text(size = 26.5, face = "bold", family = "Poppins"),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      )
  } else {
    tlShiny::no_data_plot_custom(title = paste0("No Teacher Lesson Usage\nData Available as of ", format.Date(Sys.Date(), "%b %d, %Y")))
  }


}


#' @title Teacher Curriculum Perceptions Graph
#' @description Creates a chart to summarise the teacher perceptions of curriculum
#' @param data the data
#' @return a ggplot
#' @export

make_teacher_curriculum_perceptions <- function(data) {

  if (nrow(data) >= 1) {
    curriculum_perc <- data |>
      dplyr::select(prepost, dplyr::contains("curriculum_sch_dist")) |>
      dplyr::pivot_longer(!prepost, names_to = "name", values_to = "value") |>
      dplyr::mutate(name = stringr::str_replace_all(name, c(
        "curriculum_sch_dist_1" = "The curriculum materials adopted by my school or district are well-suited to the needs of my students",
        "curriculum_sch_dist_2" = "The  curriculum materials adopted by my school or district offer students high-quality opportunities to learn.",
        "curriculum_sch_dist_3" = "The  curriculum materials adopted by my school or district are well-organized and easy to use.",
        "curriculum_sch_dist_4" = "I like the  curriculum materials adopted by my school or district.",
        "curriculum_sch_dist_5" = "The  curriculum materials adopted by my school or district will help my students learn.",
        "curriculum_sch_dist_6" = "The  curriculum materials adopted by my school are too scripted and don't provide me with enough autonomy."
      ))) |>
      dplyr::group_by(name, value, prepost) |>
      dplyr::count(sort = T) |>
      dplyr::ungroup() |>
      tidyr::drop_na(value) |>
      dplyr::mutate(name = stringr::str_wrap(name, 25)) |>
      dplyr::group_by(name, prepost) |>
      dplyr::mutate(
        Percent = round(100 * n / sum(n), 2),
        value = factor(value, levels = c(
          "Strongly disagree",
          "Disagree",
          "Neither agree nor disagree",
          "Agree",
          "Strongly agree"
        ))
      ) |>
      dplyr::filter(value %in% c("Agree", "Strongly agree")) |>
      dplyr::ungroup() |>
      dplyr::group_by(name, prepost) |>
      dplyr::summarise(
        Percent = sum(Percent),
        n = sum(n)
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(prepost = factor(prepost, levels = c("Pre", "Post")))

    n_size_1 <- sum(!is.na(data$prepost[data$prepost == "Pre"]))
    n_size_2 <- sum(!is.na(data$prepost[data$prepost == "Post"]))

    curriculum_perc |>
      ggplot2::ggplot(ggplot2::aes(
        x = forcats::fct_reorder(name, Percent, .desc = T),
        y = Percent
      )) +
      ggplot2::geom_col(ggplot2::aes(fill = prepost),
                        color = NA, position = ggplot2::position_dodge2(width = 1, reverse = TRUE)
      ) +
      ggplot2::geom_text(
        ggplot2::aes(
          color = prepost,
          label = dplyr::if_else(Percent >= 10, paste0(round(Percent), "%"), "")
        ),
        position = ggplot2::position_dodge2(reverse = TRUE, width = 1),
        hjust = -0.25,
        fontface = "bold",
        family = "Poppins",
        size = 8
      ) +
      ggplot2::labs(
        x = "", y = "",
        title = glue::glue("Teacher perceptions of curriculum % that agree or strongly agree"),
        fill = ""
      ) +
      ggplot2::scale_fill_manual(values = c(
        "Pre" = "#00ACF0",
        "Post" = "black"
      ), labels = c(glue::glue("Pre (n = {format(n_size_1, big.mark = ',')})"), glue::glue("Post (n = {n_size_2})"))) +
      ggplot2::scale_color_manual(values = c(
        "Pre" = "#00ACF0",
        "Post" = "black"
      )) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(),
        color = "none"
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::label_percent(scale = 1),
        expand = c(0.14, 0)
      ) +
      ggplot2::coord_flip() +
      tlShiny::theme_tl(legend = T) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(
          margin = ggplot2::margin(t = 0, l = 0, r = -100, b = 0),
          size = 19
        ),
        axis.text.x = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(size = 25, face = "bold", family = "Poppins"),
        legend.position = "bottom",
        legend.key.height = ggplot2::unit(1.3, "cm"),
        legend.key.width = ggplot2::unit(1.3, "cm"),
        legend.key.size = ggplot2::unit(0.75, "cm"),
        legend.text = ggplot2::element_text(size = 22, face = "bold", family = "Poppins"),
        legend.margin = ggplot2::margin(-25, 0, 0, -80)
      )
  } else {
    tlShiny::no_data_plot_custom(title = paste0("No Teacher Curriculum Perceptions\nData Available as of ", format.Date(Sys.Date(), "%b %d, %Y")))
  }

}


#' @title Contact Lead Graph Summary
#' @description Returns a barchart for selections in the relevant questions of the contact lead survey
#' @param data the data to use
#' @return prints a ggplot object
#' @export
contact_lead_graph <- function(data) {
  contact_lead <- data

  if (nrow(contact_lead) >= 1) {
    contact_lead_likert <- contact_lead |>
      dplyr::select(
        `I am satisfied with the overall quality of Teaching Lab’s professional learning and/or coaching sessions.` = mid_year_likert_qs_1,
        `I am satisfied with the overall quality of facilitation of the professional learning and/or coaching sessions.` = mid_year_likert_qs_2,
        `Teaching Lab’s professional learning has been responsive to the needs of our educators/partnership.` = mid_year_likert_qs_3,
        `I am satisfied with the logistics and communication from Teaching Lab.` = mid_year_likert_qs_4,
        `I believe Teaching Lab’s professional learning work has improved the ability of teachers and educators in my school system to deliver high-quality instruction.` = mid_year_likert_qs_5,
        `Teaching Lab is helping us to advance our goals.` = mid_year_likert_qs_6,
        `I believe teachers in my school system better understand their curriculum because of Teaching Lab’s professional learning work.` = curriculum
      ) |>
      dplyr::pivot_longer(dplyr::everything(), names_to = "Question", values_to = "Response") |>
      tidyr::drop_na(Response) |>
      dplyr::group_by(Question, Response) |>
      dplyr::count(sort = T) |>
      dplyr::ungroup() |>
      dplyr::group_by(Question) |>
      dplyr::mutate(
        Percent = round(100 * n / sum(n), 2)
      ) |>
      dplyr::ungroup()

    contact_lead_agree <- contact_lead |>
      dplyr::count(sort = T)

    contact_lead_likert_final <- contact_lead_likert |>
      dplyr::mutate(
        Question = factor(Question),
        Response = stringr::str_replace_all(Response, c(
          "3- Neither agree nor disagree" = "3- Neither agree\nnor disagree",
          "5- Strongly agree" = "5- Strongly\nagree"
        )),
        Response = factor(Response, levels = c(
          "1- Strongly disagree",
          "2- Disagree",
          "3- Neither agree\nnor disagree",
          "4- Agree",
          "5- Strongly\nagree"
        )),
        Question = stringr::str_wrap(Question, width = 50)
      )

      p <- contact_lead_likert_final |>
        ggplot2::ggplot(aes(
          x = forcats::fct_reorder(Question, Percent, .desc = T),
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
          family = "Poppins",
          size = 11
        ) +
        ggplot2::labs(
          x = "", y = "",
          title = glue::glue("Mid/End Year Contact Lead Feedback (n = {sum(contact_lead_agree$n, na.rm = T)})"),
          fill = ""
        ) +
        ggplot2::scale_fill_manual(values = c(
          "1- Strongly disagree" = "#040404",
          "2- Disagree" = "#032E3F",
          "3- Neither agree\nnor disagree" = "#02587A",
          "4- Agree" = "#0182B4",
          "5- Strongly\nagree" = "#00ACF0"
        )) +
        ggplot2::scale_color_manual(values = c(
          "1- Strongly disagree" = "white",
          "2- Disagree" = "black",
          "3- Neither agree\nnor disagree" = "black",
          "4- Agree" = "black",
          "5- Strongly\nagree" = "black"
        )) +
        ggplot2::guides(
          fill = ggplot2::guide_legend(),
          color = "none"
        ) +
        ggplot2::scale_y_continuous(
          labels = scales::label_percent(scale = 1),
          expand = c(0.14, 0)
        ) +
        ggplot2::coord_flip() +
        tlShiny::theme_tl(legend = TRUE) +
        ggplot2::theme(
          axis.text.y = ggplot2::element_text(
            size = 18,
            margin = ggplot2::margin(r = -80)
          ),
          plot.title = ggplot2::element_text(size = 30, face = "bold", family = "Poppins"),
          legend.box.margin = ggplot2::margin(t = -10, 0, 0, l = -50),
          legend.text = ggplot2::element_text(size = 23),
          legend.key.width = ggplot2::unit(2, "cm"),
          legend.key.height = ggplot2::unit(0.5, "cm"),
          legend.position = "bottom"
        )

      p
  } else {
    tlShiny::no_data_plot_custom(title = paste0("No Contact Lead Data Available as of ", format.Date(Sys.Date(), "%b %d, %Y")))
  }
}

#' @title Teacher School Leader Perceptions Graph
#' @description Creates a chart to summarise the teacher perceptions of school leaders
#' @param data the data
#' @return a ggplot
#' @export

make_teacher_perceptions_school_leaders <- function(data) {

  if (nrow(data) >= 1) {
    teacher_perc_sl <- data |>
      dplyr::select(prepost, dplyr::contains("ts_perceptions_sl")) |>
      # dplyr::select(-dplyr::all_of(c(ts_perceptions_sl_6, ts_perceptions_sl_7, ts_perceptions_sl_8))) |>
      dplyr::pivot_longer(!prepost, names_to = "name", values_to = "value") |>
      dplyr::mutate(name = stringr::str_replace_all(name, c(
        "ts_perceptions_sl_2" = "My school leaders have created a shared vision for instruction that my curriculum-related professional development experiences is helping our school to implement",
        "ts_perceptions_sl_3" = "My school leaders make sure I have access to all the materials and resources I need to implement our adopted curriculum",
        "ts_perceptions_sl_4" = "My school leaders press me to implement the ideas I learn in curriculum-related professional development",
        "ts_perceptions_sl_5" = "My school leaders make time for my curriculum-related professional development.",
        "ts_perceptions_sl_9" = "My school leader is knowledgeable about the curriculum that I have been asked to implement"
      ))) |>
      dplyr::group_by(name, value, prepost) |>
      dplyr::count(sort = T) |>
      dplyr::ungroup() |>
      tidyr::drop_na(value) |>
      dplyr::mutate(name = stringr::str_wrap(name, 60)) |>
      dplyr::group_by(name, prepost) |>
      dplyr::mutate(
        Percent = round(100 * n / sum(n), 2),
        value = factor(value, levels = c(
          "Not at all",
          "Rarely",
          "Sometimes",
          "Often",
          "All the time"
        ))
      ) |>
      dplyr::filter(value %in% c("Often", "All the time")) |>
      dplyr::ungroup() |>
      dplyr::group_by(name, prepost) |>
      dplyr::summarise(
        Percent = sum(Percent),
        n = sum(n)
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(prepost = factor(prepost, levels = c("Pre", "Post")))

    n_size_1 <- sum(!is.na(data$prepost[data$prepost == "Pre"]))
    n_size_2 <- sum(!is.na(data$prepost[data$prepost == "Post"]))

    teacher_perc_sl |>
      ggplot2::ggplot(ggplot2::aes(
        x = forcats::fct_reorder(name, Percent, .desc = T),
        y = Percent
      )) +
      ggplot2::geom_col(ggplot2::aes(fill = prepost),
                        color = NA, position = ggplot2::position_dodge2(width = 1, reverse = TRUE)
      ) +
      ggplot2::geom_text(
        aes(
          color = prepost,
          label = paste0(round(Percent), "%")
        ),
        position = ggplot2::position_dodge2(reverse = TRUE, width = 1),
        hjust = -0.1,
        fontface = "bold",
        size = 8,
        family = "Poppins"
      ) +
      ggplot2::labs(
        x = "", y = "",
        title = "Educators’ Perceptions of School Leaders (% \"often\" or \"all the time\")",
        fill = ""
      ) +
      ggplot2::scale_fill_manual(values = c(
        "#00ACF0", "black"
      ), labels = c(glue::glue("Pre (n = {format(n_size_1, big.mark = ',')})"), glue::glue("Post (n = {n_size_2})"))) +
      ggplot2::scale_color_manual(values = c(
        "#00ACF0", "black"
      )) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(),
        color = "none"
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::label_percent(scale = 1),
        expand = c(0.14, 0)
      ) +
      ggplot2::coord_flip() +
      tlShiny::theme_tl(legend = T) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(
          margin = ggplot2::margin(t = 0, l = 0, r = -80, b = 0),
          size = 21
        ),
        axis.text.x = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(size = 25, face = "bold", family = "Poppins"),
        legend.position = "bottom",
        legend.key.height = ggplot2::unit(1.3, "cm"),
        legend.key.width = ggplot2::unit(1.3, "cm"),
        legend.key.size = ggplot2::unit(0.75, "cm"),
        legend.text = ggplot2::element_text(size = 22, face = "bold", family = "Poppins"),
        legend.margin = ggplot2::margin(-25, 0, 0, -50)
      )
  } else {
    tlShiny::no_data_plot_custom(title = paste0("No Teacher School Leader Perceptions\nData Available as of ", format.Date(Sys.Date(), "%b %d, %Y")))
  }

}

#' @title Teacher Perceptions Peer Relationships Graph
#' @description Creates a chart to summarise the teacher perceptions of of peer relationships
#' @param data the data
#' @return a ggplot
#' @export

make_teacher_perceptions_peer_relationships <- function(data) {

  if (nrow(data) >= 1) {
    teacher_perc_peer <- data |>
      dplyr::select(prepost, dplyr::contains("school_environment")) |>
      dplyr::select(-school_environment_3) |>
      dplyr::pivot_longer(!prepost, names_to = "name", values_to = "value") |>
      dplyr::mutate(name = stringr::str_replace_all(name, c(
        "school_environment_1" = "I trust my fellow teachers in the school",
        "school_environment_2" = "I feel connected to my fellow teachers in the school",
        "school_environment_4" = "I collaborate with my fellow teachers regularly"
      ))) |>
      dplyr::group_by(name, value, prepost) |>
      dplyr::count(sort = T) |>
      dplyr::ungroup() |>
      tidyr::drop_na(value) |>
      dplyr::mutate(name = stringr::str_wrap(name, 25)) |>
      dplyr::group_by(name, prepost) |>
      dplyr::mutate(Percent = round(100 * n / sum(n), 2)) |>
      dplyr::filter(value %in% c("4 - Agree", "5 - Strongly agree")) |>
      dplyr::summarise(
        Percent = sum(Percent),
        n = sum(n)
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(prepost = factor(prepost, levels = c("Pre", "Post")))

    n_size_1 <- sum(!is.na(data$prepost[data$prepost == "Pre"]))
    n_size_2 <- sum(!is.na(data$prepost[data$prepost == "Post"]))

    teacher_perc_peer |>
      ggplot2::ggplot(ggplot2::aes(
        x = forcats::fct_reorder(name, Percent, .desc = T),
        y = Percent
      )) +
      ggplot2::geom_col(aes(fill = prepost),
                        color = NA, position = ggplot2::position_dodge2(width = 1, reverse = TRUE)
      ) +
      ggplot2::geom_text(
        ggplot2::aes(
          color = prepost,
          label = dplyr::if_else(Percent >= 10, paste0(round(Percent), "%"), "")
        ),
        position = ggplot2::position_dodge2(reverse = TRUE, width = 1),
        hjust = -0.25,
        fontface = "bold",
        size = 6
      ) +
      ggplot2::labs(
        x = "", y = "",
        title = glue::glue("Educators’ Perceptions of School Culture and Climate\n(% that agree or strongly agree)"),
        fill = ""
      ) +
      ggplot2::scale_fill_manual(values = c(
        "#00ACF0", "black"
      ), labels = c(glue::glue("Pre (n = {format(n_size_1, big.mark = ',')})"), glue::glue("Post (n = {n_size_2})"))) +
      ggplot2::scale_color_manual(values = c(
        "#00ACF0", "black"
      )) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(),
        color = "none"
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::label_percent(scale = 1),
        expand = c(0.14, 0)
      ) +
      ggplot2::coord_flip() +
      tlShiny::theme_tl(legend = T) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(
          margin = ggplot2::margin(t = 0, l = 0, r = -100, b = 0),
          size = 19
        ),
        axis.text.x = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(size = 25, face = "bold", family = "Poppins"),
        legend.position = "bottom",
        legend.key.height = ggplot2::unit(1.3, "cm"),
        legend.key.width = ggplot2::unit(1.5, "cm"),
        legend.key.size = ggplot2::unit(0.75, "cm"),
        legend.text = ggplot2::element_text(size = 22),
        legend.margin = ggplot2::margin(-25, 0, 0, -150)
      )
  } else {
    tlShiny::no_data_plot_custom(title = paste0("No Teacher Peer Perceptions\nData Available as of ", format.Date(Sys.Date(), "%b %d, %Y")))
  }

}



#' @title Student Work People Chart
#' @description Chart to show the % of people in each scoring category for student work
#' @param data the data for the plotter to use, should include all columns of interest
#' @return a ggplot
#' @export

make_student_work_chart_people <- function(data) {
  if (nrow(data) >= 1) {
    grade_breakdown <- data |>
      dplyr::rename(grades = `Submitted Grade/s`) |>
      dplyr::select(grades, `Student Work File`, Prepost) |>
      dplyr::mutate(
        Prepost = dplyr::replace_na(Prepost, "Pre")
      ) |>
      tidyr::separate_rows(grades, sep = ", ") |>
      dplyr::mutate(grades = as.numeric(grades)) |>
      tidyr::drop_na(grades) |>
      dplyr::group_by(grades, Prepost) |>
      dplyr::count(sort = T) |>
      dplyr::ungroup() |>
      dplyr::group_by(Prepost) |>
      dplyr::mutate(
        percent = round(100 * (n / sum(n)), 2)
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        Prepost = dplyr::case_when(
          Prepost == "Pre" ~ paste0("Pre (n = ", format(sum(n[Prepost == "Pre"], na.rm = TRUE), big.mark = ","), ")"),
          Prepost == "Post" ~ paste0("Post (n = ", format(sum(n[Prepost == "Post"], na.rm = TRUE), big.mark = ","), ")")
        )
      ) |>
      suppressWarnings()

    if (nrow(grade_breakdown) >= 1) {
      grade_breakdown$Prepost <- factor(grade_breakdown$Prepost, levels = c(sort(grade_breakdown$Prepost)[6], sort(grade_breakdown$Prepost)[1]))


      zero_grade_pre <- ifelse("0" %in% grade_breakdown$grades[stringr::str_detect(grade_breakdown$Prepost, "Pre")],
                               glue::glue("<b style='color:#040404'>{grade_breakdown$percent[grade_breakdown$grades == '0' & stringr::str_detect(grade_breakdown$Prepost, 'Pre')]}% received a 0.</b>"),
                               "no scores received a 0."
      )
      one_grade_pre <- ifelse("1" %in% grade_breakdown$grades[stringr::str_detect(grade_breakdown$Prepost, "Pre")],
                              glue::glue("<b style='color:#02587A'>{grade_breakdown$percent[grade_breakdown$grades == '1' & stringr::str_detect(grade_breakdown$Prepost, 'Pre')]}% received a 1,</b> and"),
                              "no scores received a 1."
      )
      two_grade_pre <- ifelse("2" %in% grade_breakdown$grades[stringr::str_detect(grade_breakdown$Prepost, "Pre")],
                              glue::glue("<b style='color:#04abeb'>{grade_breakdown$percent[grade_breakdown$grades == '2' & stringr::str_detect(grade_breakdown$Prepost, 'Pre')]}% received a 2,</b>"),
                              "no scores received a 2."
      )

      zero_grade_post <- ifelse("0" %in% grade_breakdown$grades[stringr::str_detect(grade_breakdown$Prepost, "Post")],
                                glue::glue("<b style='color:#040404'>{grade_breakdown$percent[grade_breakdown$grades == '0' & stringr::str_detect(grade_breakdown$Prepost, 'Post')]}% received a 0.</b>"),
                                "no scores received a 0."
      )
      one_grade_post <- ifelse("1" %in% grade_breakdown$grades[stringr::str_detect(grade_breakdown$Prepost, "Post")],
                               glue::glue("<b style='color:#02587A'>{grade_breakdown$percent[grade_breakdown$grades == '1' & stringr::str_detect(grade_breakdown$Prepost, 'Post')]}% received a 1,</b> and"),
                               "no scores received a 1."
      )
      two_grade_post <- ifelse("2" %in% grade_breakdown$grades[stringr::str_detect(grade_breakdown$Prepost, "Post")],
                               glue::glue("<b style='color:#04abeb'>{grade_breakdown$percent[grade_breakdown$grades == '2' & stringr::str_detect(grade_breakdown$Prepost, 'Post')]}% received a 2,</b>"),
                               "no scores received a 2."
      )

      student_work_plot_2 <- grade_breakdown |>
        dplyr::mutate(grades = factor(grades, levels = c("0", "1", "2"))) |>
        dplyr::arrange(grades) |>
        ggplot2::ggplot(ggplot2::aes(label = grades, values = n)) +
        ggplot2::geom_text(
          stat = "waffle",
          family = "Font Awesome 5 Free",
          ggplot2::aes(color = grades),
          flip = TRUE,
          n_rows = 10,
          size = 20,
          alpha = 0.85,
          make_proportional = TRUE,
          na.rm = TRUE
        ) +
        ggplot2::facet_wrap(~Prepost) +
        ggplot2::scale_color_manual(values = c("0" = "#040404", "1" = "#02587A", "2" = "#00ACF0")) +
        waffle::scale_label_pictogram(
          name = NULL,
          values = c("0" = "male", "1" = "male", "2" = "male")
        ) +
        ggplot2::labs(
          title = paste0("Student Performance On Grade-Level Tasks"),
          subtitle = glue::glue("<br>Of <b>pre</b> on grade-level tasks, {two_grade_pre} {one_grade_pre} {zero_grade_pre}<br>Of <b>post</b> on grade-level tasks, {two_grade_post} {one_grade_post} {zero_grade_post}")
        ) +
        ggplot2::theme_void(base_family = "Lora") +
        ggplot2::theme(
          legend.position = "none",
          plot.title = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 48, family = "Poppins"),
          plot.subtitle = ggtext::element_markdown(
            hjust = 0.5,
            face = "italic",
            family = "Lora",
            lineheight = 0.3,
            size = 38
          ),
          strip.text = ggplot2::element_text(face = "bold", size = 41, family = "Poppins", color = "black", margin = ggplot2::margin(t = 15))
        )

      student_work_plot_2
    } else {
      ggplot2::ggplot(data.frame(text = "No submitted student work was graded as asking for mathematical thinking or responding/writing from evidence.", x = 0, y = 0)) +
        ggplot2::geom_text(ggplot2::aes(label = text, x, y), fontface = "bold", family = "Poppins", size = 10, color = "black") +
        ggplot2::theme_void()
    }

  } else {
    tlShiny::no_data_plot_custom(title = paste0("No Student Work Data has been submitted\nas of ", format.Date(Sys.Date(), "%b %d, %Y")))
  }
}

#' @title Student Work Circle Charts
#' @description Chart to show the % of people for student work that are on grade level
#' @param data the data for the plotter to use, should include all columns of interest
#' @return a ggplot
#' @export

make_student_work_chart_circle <- function(data) {
  if (nrow(data) >= 1) {
    on_grade_level <- data |>
      dplyr::rename(grades = `Submitted Grade/s`) |>
      dplyr::select(grades, `Student Work File`, Prepost) |>
      dplyr::mutate(Prepost = dplyr::replace_na(Prepost, "Pre")) |>
      tidyr::drop_na(grades) |>
      tidyr::separate_rows(grades, sep = ", ") |>
      dplyr::mutate(`Grade Level` = if_else(grades == "Not on grade level", 0, 1)) |>
      dplyr::group_by(`Grade Level`, Prepost) |>
      dplyr::count(sort = T) |>
      dplyr::ungroup() |>
      dplyr::group_by(Prepost) |>
      dplyr::mutate(`Percent` = round(100 * n / sum(n), 2)) |>
      suppressWarnings()

    student_work_plot_1 <- ggplot() +
      ggplot2::geom_col(
        data = data.frame(x = 1, y = 100, fill = "gray90"),
        ggplot2::aes(x = x, y = y, fill = fill)
      ) +
      ggplot2::geom_col(
        data = data.frame(x = 1, y = on_grade_level$Percent[on_grade_level$`Grade Level` == 1 & on_grade_level$Prepost == "Pre"], fill = "#04abeb"),
        aes(x = x, y = y, fill = fill)
      ) +
      ggplot2::annotate("text",
                        label = paste0(on_grade_level$Percent[on_grade_level$`Grade Level` == 1 & on_grade_level$Prepost == "Pre"], "%"),
                        family = "Poppins",
                        fontface = "bold",
                        color = "#04abeb",
                        size = 22,
                        x = -2,
                        y = 0
      ) +
      ggplot2::labs(title = paste0("% of analyzed student work samples on\nGrade Level (Pre n = ", format(sum(on_grade_level$n[on_grade_level$Prepost == "Pre"], na.rm = T), big.mark = ","), ")")) +
      ggplot2::scale_fill_identity() +
      ggplot2::coord_polar(theta = "y", direction = 1) +
      ggplot2::xlim(c(-2, 2)) +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          hjust = 0.5, family = "Poppins", face = "bold", size = 42,
          vjust = -3
        ),
        panel.background = ggplot2::element_rect(fill = "white", color = "white"),
        plot.background = ggplot2::element_rect(fill = "white", color = "white")
      )

    student_work_plot_1_1 <- ggplot2::ggplot() +
      ggplot2::geom_col(
        data = data.frame(x = 1, y = 100, fill = "gray90"),
        ggplot2::aes(x = x, y = y, fill = fill)
      ) +
      ggplot2::geom_col(
        data = data.frame(x = 1, y = on_grade_level$Percent[on_grade_level$`Grade Level` == 1 & on_grade_level$Prepost == "Post"], fill = "#04abeb"),
        ggplot2::aes(x = x, y = y, fill = fill)
      ) +
      ggplot2::annotate("text",
                        label = paste0(on_grade_level$Percent[on_grade_level$`Grade Level` == 1 & on_grade_level$Prepost == "Post"], "%"),
                        family = "Poppins",
                        fontface = "bold",
                        color = "#04abeb",
                        size = 22,
                        x = -2,
                        y = 0
      ) +
      ggplot2::labs(title = paste0("% of analyzed student work samples on\nGrade Level (Post n = ", format(sum(on_grade_level$n[on_grade_level$Prepost == "Post"], na.rm = T), big.mark = ","), ")")) +
      ggplot2::scale_fill_identity() +
      ggplot2::coord_polar(theta = "y", direction = 1) +
      ggplot2::xlim(c(-2, 2)) +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          hjust = 0.5, family = "Poppins", face = "bold", size = 35,
          vjust = -3
        ),
        panel.background = ggplot2::element_rect(fill = "white", color = "white"),
        plot.background = ggplot2::element_rect(fill = "white", color = "white")
      )

    patchwork::wrap_plots(student_work_plot_1, student_work_plot_1_1) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", color = "white"),
                     plot.background = ggplot2::element_rect(fill = "white", color = "white"))
  } else {
    tlShiny::no_data_plot_custom(title = paste0("No Student Work Data has been submitted\nas of ", format.Date(Sys.Date(), "%b %d, %Y")))
  }
}
