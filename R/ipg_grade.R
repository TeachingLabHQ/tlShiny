#' @title IPG Character Columns
#' @description Character column names from the Instructional Practice Guide (IPG) rubric.
#' These are typically binary values like "Yes"/"No" indicating presence of a practice.
#' @export
ipg_character_cols <- c(
  "k12_m_ca1a", "k12_m_ca1b", "k12_m_ca1c",
  "k12_ela_ca1a", "k12_ela_ca1b", "k12_ela_ca1c",
  "tl_sci_ca1a", "tl_sci_ca1b", "tl_sci_ca1c"
)

#' @title IPG Numeric Columns
#' @description Numeric column names from the IPG rubric, scored from 1 to 4.
#' Higher scores indicate stronger alignment with the rubric.
#' @export
ipg_numeric_cols <- c(
  "k12_m_ca2a", "k12_m_ca2b", "k12_m_ca2c", "k12_m_ca2d",
  "k12_m_ca3a", "k12_m_ca3b", "k12_m_ca3c", "k12_m_ca3d", "k12_m_ca3e",
  "k12_ela_ca2a", "k12_ela_ca2b", "k12_ela_ca2c", "k12_ela_ca2d",
  "k12_ela_ca3a", "k12_ela_ca3b", "k12_ela_ca3c", "k12_ela_ca3d",
  "k12_ela_ca3e", "k12_ela_ca3f", "fsot_ac1", "fsot_ac2", "fsot_td1",
  "fsot_td2", "fsot_td3", "fsot_td4", "fsot_sp1", "fsot_sp2",
  "fsot_sp3", "fsot_sp4", "ss_ca1a", "ss_ca1b", "ss_ca1c",
  "sci_ca1a", "sci_ca1b", "sci_ca1c", "ss_ca2a", "ss_ca2b",
  "ss_ca2c", "ss_ca2d", "sci_ca2a", "sci_ca2b", "sci_ca2c",
  "sci_ca2d", "ss_ca3a", "ss_ca3b", "ss_ca3c", "ss_ca3d",
  "sci_ca3a", "sci_ca3b", "sci_ca3c", "sci_ca3d",
  "tl_sci_ca2a", "tl_sci_ca2b", "tl_sci_ca2c", "tl_sci_ca2d",
  "tl_sci_ca3a", "tl_sci_ca3b", "tl_sci_ca3c", "tl_sci_ca3d"
)

#' @title IPG Numeric Low Columns
#' @description Numeric columns from the IPG rubric with a lower scale where 2 and 3 are considered "high."
#' @export
ipg_numeric_low_cols <- c("fsot_ad1", "fsot_ad2")

#' @title Core Action Column Groups
#' @description Convenience vectors grouping IPG rubric items by Core Action (CA1, CA2, CA3) for scoring.
#' These include both character and numeric columns.
#' @export
ca1_cols <- c(ipg_numeric_cols[stringr::str_detect(ipg_numeric_cols, "ca1")], ipg_character_cols[stringr::str_detect(ipg_character_cols, "ca1")])
#' @export
ca2_cols <- c(ipg_numeric_cols[stringr::str_detect(ipg_numeric_cols, "ca2")], ipg_character_cols[stringr::str_detect(ipg_character_cols, "ca2")])
#' @export
ca3_cols <- c(ipg_numeric_cols[stringr::str_detect(ipg_numeric_cols, "ca3")], ipg_character_cols[stringr::str_detect(ipg_character_cols, "ca3")])

#' @title FSOT Domain Columns
#' @description Column groups used in FSOT scoring for Aligned Content (AC), Assessment & Differentiation (AD), Student Practice (SP), and Teacher-Directed Instruction (TD).
#' @export
ac_cols <- c("fsot_ac1", "fsot_ac2")
#' @export
ad_cols <- c("fsot_ad1", "fsot_ad2")
#' @export
td_cols <- c("fsot_td1", "fsot_td2", "fsot_td3", "fsot_td4")
#' @export
sp_cols <- c("fsot_sp1", "fsot_sp2", "fsot_sp3", "fsot_sp4")

#' @title PreK Language Columns
#' @description Character column names from the PreK rubric for Language.
#' Values are Likert-type, with "Always" and "Often" treated as proficient.
#' @export
prek_lang_cols <- c(
  "prek_lang_1", "prek_lang_2", "prek_lang_3", "prek_lang_4", "prek_lang_5",
  "prek_lang_6", "prek_lang_7", "prek_lang_8", "prek_lang_9", "prek_lang_10"
)

#' @title PreK Literacy Columns
#' @description Character column names from the PreK rubric for Literacy.
#' Values are Likert-type, with "Always" and "Often" treated as proficient.
#' @export
prek_lit_cols <- c(
  "prek_lit_1", "prek_lit_2", "prek_lit_3", "prek_lit_4", "prek_lit_5",
  "prek_lit_6", "prek_lit_7", "prek_lit_8", "prek_lit_9"
)

#' @title PreK Rubric Columns
#' @description Convenience vector combining all PreK rubric items (Language + Literacy).
#' @export
prek_cols <- c(prek_lang_cols, prek_lit_cols)

#' @title Grade IPG Rubric Item
#' @description Converts a character-based rubric score into a logical value indicating proficiency.
#' Supports three types: `"character"` (Yes/No), `"numeric"` (1–4 scale), and `"numeric_low"` (1–3 scale).
#' @param x A character vector of rubric values.
#' @param type One of `"character"`, `"numeric"`, or `"numeric_low"` determining the rubric logic.
#' @return A logical vector with TRUE (proficient), FALSE (not proficient), or NA.
#' @export
new_grade_ipg <- function(x, type) {
  if (type == "character") {
    x <- dplyr::case_when(
      grepl("Yes", x) ~ TRUE,
      grepl("No", x) ~ FALSE,
      TRUE ~ NA
    )
  } else if (type == "numeric") {
    x <- dplyr::case_when(
      grepl("3|4", x) ~ TRUE,
      grepl("1|2", x) ~ FALSE,
      TRUE ~ NA
    )
  } else if (type == "numeric_low") {
    x <- dplyr::case_when(
      grepl("2|3", x) ~ TRUE,
      grepl("1", x) ~ FALSE,
      TRUE ~ NA
    )
  } else if (type == "prek") {
    x <- dplyr::case_when(
      stringr::str_detect(x, "Always|Often") ~ TRUE,
      stringr::str_detect(x, "Sometimes|Rarely|Never") ~ FALSE,
      TRUE ~ NA
    )
  }

  return(x)
}


#' @title Summarize IPG Scores Per Teacher
#' @description Converts raw IPG rubric responses to logical scores and computes summary scores by core action and domain.
#' @param df A data frame with raw IPG rubric columns and metadata (e.g., `direct_to_ts_obs`, `ipg_rubric`).
#' @param ipg_character_cols character columns
#' @param ipg_numeric_cols numeric columns
#' @param ipg_numeric_low_cols numeric columns with low score range
#' @return A data frame of teacher-level scores, one row per observation.
#'
#' @examples
#' ipg_summarise_teacher(ipg_data)
#' @export
ipg_summarise_teacher <- function(df, character_cols = ipg_character_cols, numeric_cols = ipg_numeric_cols, numeric_low_cols = ipg_numeric_low_cols) {
  ipg_scored <- df |>
    collapse::fsubset(!is.na(direct_to_ts_obs) & direct_to_ts_obs != "Ongoing") |>
    collapse::fselect("direct_to_ts_obs", "ipg_rubric", c(character_cols, numeric_cols, numeric_low_cols)) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(character_cols), ~ new_grade_ipg(.x, type = "character"))) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(c(numeric_cols)), ~ new_grade_ipg(.x, type = "numeric"))) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(c(numeric_low_cols)), ~ new_grade_ipg(.x, type = "numeric_low"))) |>
    dplyr::mutate(
      ca1_score = 100 * rowMeans(dplyr::pick(dplyr::all_of(ca1_cols)), na.rm = TRUE),
      ca2_score = 100 * rowMeans(dplyr::pick(dplyr::all_of(ca2_cols)), na.rm = TRUE),
      ca3_score = 100 * rowMeans(dplyr::pick(dplyr::all_of(ca3_cols)), na.rm = TRUE),
      ac_score = 100 * rowMeans(dplyr::pick(dplyr::all_of(ac_cols)), na.rm = TRUE),
      ad_score = 100 * rowMeans(dplyr::pick(dplyr::all_of(ad_cols)), na.rm = TRUE),
      sp_score = 100 * rowMeans(dplyr::pick(dplyr::all_of(sp_cols)), na.rm = TRUE),
      td_score = 100 * rowMeans(dplyr::pick(dplyr::all_of(td_cols)), na.rm = TRUE),
      overall_score = 100 * rowMeans(dplyr::pick(dplyr::all_of(c(character_cols, numeric_cols, numeric_low_cols))), na.rm = TRUE)
    ) |>
    collapse::fselect(
      direct_to_ts_obs,
      ipg_rubric,
      ca1_score,
      ca2_score,
      ca3_score,
      ac_score,
      ad_score,
      sp_score,
      td_score,
      overall_score
    )

  return(ipg_scored)
}

#' @title Summarize IPG Scores Across Observations
#' @description Aggregates teacher-level IPG scores by observation timepoint (`direct_to_ts_obs`) and reshapes to long format with scores and Ns.
#' Includes core actions and FSOT domain scores.
#' @param data Output from \code{\link{ipg_summarise_teacher}}.
#' @return A long-format data frame with columns: direct_to_ts_obs, Core Action, Score, n.
#'
#' @examples
#' teacher_scores <- ipg_summarise_teacher(ipg_data)
#' ipg_summarise_overall(teacher_scores)
#' @export
ipg_summarise_overall <- function(data) {
  data1 <- data |>
    collapse::fgroup_by(direct_to_ts_obs) |>
    collapse::fsummarise(
      overall_score = collapse::fmean(overall_score, na.rm = TRUE),
      ca1_n = collapse::fsum(!is.na(ca1_score)),
      ca1_score = collapse::fmean(ca1_score, na.rm = TRUE),
      ca2_n = collapse::fsum(!is.na(ca2_score)),
      ca2_score = collapse::fmean(ca2_score, na.rm = TRUE),
      ca3_n = collapse::fsum(!is.na(ca3_score)),
      ca3_score = collapse::fmean(ca3_score, na.rm = TRUE),
      ac_n = collapse::fsum(!is.na(ac_score)),
      ac_score = collapse::fmean(ac_score, na.rm = TRUE),
      ad_n = collapse::fsum(!is.na(ad_score)),
      ad_score = collapse::fmean(ad_score, na.rm = TRUE),
      sp_n = collapse::fsum(!is.na(sp_score)),
      sp_score = collapse::fmean(sp_score, na.rm = TRUE),
      td_n = collapse::fsum(!is.na(td_score)),
      td_score = collapse::fmean(td_score, na.rm = TRUE),
      n = length(overall_score)
    )

  data_ns <- data1 |>
    collapse::fselect("direct_to_ts_obs", "ca1_n", "ca2_n", "ca3_n", "ac_n", "ad_n", "sp_n", "td_n", "n") |>
    collapse::pivot(
      how = "longer",
      names = list("Core Action", "n"),
      values = c("ca1_n", "ca2_n", "ca3_n", "ac_n", "ad_n", "sp_n", "td_n", "n")
    ) |>
    collapse::fmutate(
      `Core Action` = stringr::str_replace_all(`Core Action`, c(
        "ca1_n" = "Core Action 1",
        "ca2_n" = "Core Action 2",
        "ca3_n" = "Core Action 3",
        "ac_n" = "Aligned Content",
        "ad_n" = "Assessment & Differentiation",
        "sp_n" = "Student Practice",
        "td_n" = "Teacher-Directed Instruction",
        "^n$" = "Overall"
      ))
    )

  data_final <- data1 |>
    collapse::pivot(
      how = "longer",
      ids = "direct_to_ts_obs",
      names = list("Core Action", "Score"),
      values = c(
        "overall_score",
        "ca1_score", "ca2_score", "ca3_score",
        "ac_score", "ad_score", "sp_score", "td_score"
      )
    ) |>
    collapse::fmutate(
      `Core Action` = stringr::str_replace_all(`Core Action`, c(
        "overall_score" = "Overall",
        "ca1_score" = "Core Action 1",
        "ca2_score" = "Core Action 2",
        "ca3_score" = "Core Action 3",
        "ac_score" = "Aligned Content",
        "ad_score" = "Assessment & Differentiation",
        "sp_score" = "Student Practice",
        "td_score" = "Teacher-Directed Instruction"
      ))
    ) |>
    collapse::join(data_ns, how = "left", on = c("Core Action", "direct_to_ts_obs")) |>
    collapse::fmutate(direct_to_ts_obs = factor(direct_to_ts_obs, levels = c(
      "Baseline (first observation of the year)",
      "Mid-year (middle of service, if applicable)",
      "End of year (last observation of the year)"
    ))) |>
    collapse::roworderv(cols = "direct_to_ts_obs") |>
    collapse::fsubset(!is.na(Score) & !is.na(direct_to_ts_obs))

  return(data_final)
}


#' @title Bar Chart of Overall IPG Scores by Observation Timepoint
#' @description Creates a column chart of overall IPG scores by observation timepoint (`direct_to_ts_obs`), using the
#' "Overall" row from \code{\link{ipg_summarise_overall}} and annotating bars with percent and sample size (n).
#' @param data Long-format data from \code{\link{ipg_summarise_overall}} containing columns:
#' \code{direct_to_ts_obs}, \code{Core Action}, \code{Score}, and \code{n}.
#' @return A \code{ggplot} object.
#'
#' @examples
#' teacher_scores <- ipg_summarise_teacher(ipg_data)
#' overall_long <- ipg_summarise_overall(teacher_scores)
#' ipg_bar_chart(overall_long)
#' @export
ipg_bar_chart <- function(data) {
  data |>
    collapse::fsubset(`Core Action` == "Overall") |>
    # dplyr::filter(`Core Action` == "Overall") |>
    ggplot2::ggplot(ggplot2::aes(x = direct_to_ts_obs, y = Score)) +
    ggplot2::geom_col(fill = "#04abeb") +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(Score, 1), "%\n (n = ", n, ")")),
      fontface = "bold",
      vjust = -0.5,
      size = 11,
      lineheight = 0.8,
      family = "Poppins",
      color = "black"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(scale = 1),
      limits = c(0, 100),
      expand = c(0.13, 0)
    ) +
    ggplot2::scale_x_discrete(
      labels = ~ stringr::str_wrap(as.character(.x), 20)
    ) +
    ggplot2::labs(
      x = "", y = "",
      title = "% Positive Indicators Across\nAll Observation Rubrics",
      caption = '*Note that "Ongoing" observations are not included in this analysis'
    ) +
    tlShiny::theme_tl(base_family = "Lora", plot_title_family = "Poppins", axis_title_family = "Lora") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 25, family = "Lora"),
      axis.text.y = ggplot2::element_text(size = 20, family = "Lora"),
      plot.title = ggplot2::element_text(size = 30, face = "bold", family = "Poppins"),
      plot.caption = ggplot2::element_text(size = 14, family = "Lora")
    )
}

#' @title Faceted Bar Chart of IPG Domain Scores Across Timepoints
#' @description Plots domain-level IPG scores (CA1–CA3, AC, AD, SP, TDI, and Overall) with facets by
#' observation timepoint. Labels bars with percent and sample size (n). Uses \code{tlShiny::tl_palette}
#' for fills and abbreviates domain labels in the plot.
#' @param data Long-format data from \code{\link{ipg_summarise_overall}} containing columns:
#' \code{direct_to_ts_obs}, \code{Core Action}, \code{Score}, and \code{n}.
#' @param rubric Character string used in the plot title (e.g., rubric name shown to users).
#' @param colors Integer for the number of colors to request from \code{tlShiny::tl_palette}; choose
#' a value \eqn{\ge} 8 to cover all categories.
#' @return A \code{ggplot} object.
#'
#' @examples
#' teacher_scores <- ipg_summarise_teacher(ipg_data)
#' overall_long <- ipg_summarise_overall(teacher_scores)
#' ipg_rubric_chart(overall_long, rubric = "Math IPG", colors = 8)
#' @export
ipg_rubric_chart <- function(data, rubric, colors) {
  data |>
    collapse::fsubset(!is.na(Score)) |>
    collapse::fmutate(
      `Core Action` = stringr::str_replace_all(`Core Action`, c(
        "Core Action" = "CA",
        "Student Practice" = "SP",
        "Aligned Content" = "AC",
        "Assessment & Differentiation" = "AD",
        "Teacher-Directed Instruction" = "TDI"
      ))
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = forcats::fct_relevel(`Core Action`, "Overall", after = 4L), y = Score)) +
    ggplot2::geom_col(ggplot2::aes(fill = `Core Action`)) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(Score, 1), "%\n (n = ", n, ")")),
      fontface = "bold",
      vjust = -0.25,
      size = 8,
      family = "Poppins",
      lineheight = 0.85
    ) +
    ggplot2::facet_wrap(~direct_to_ts_obs) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(scale = 1),
      limits = c(0, 100),
      expand = c(0.1, 0)
    ) +
    ggplot2::scale_fill_manual(values = tlShiny::tl_palette(color = "blue", n = colors)[c(2:colors)]) +
    ggplot2::labs(
      x = "", y = "",
      title = paste0("% Positive Indicators Across ", stringr::str_remove_all(rubric, "\\(please use this tool for K-2 observations that are not focused on foundational skills\\)")),
      caption = '*Note that "Ongoing" observations are not included in this analysis\n CA = Core Action, Aligned Content = AC, Assessment & Differentiation = AD, Student Practice = SP, Teacher-Directed Instruction = TDI'
    ) +
    tlShiny::theme_tl(base_family = "Lora", plot_title_family = "Poppins", axis_title_family = "Lora") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 22, family = "Lora"),
      axis.text.y = ggplot2::element_text(size = 18, family = "Lora"),
      plot.title = ggplot2::element_text(size = 30, face = "bold", family = "Poppins"),
      strip.text = ggplot2::element_text(size = 21, face = "bold", hjust = 0.5, family = "Poppins"),
      plot.caption = ggplot2::element_text(size = 14, family = "Lora")
    )
}

#' @title Summarize PreK Rubric Scores Per Teacher
#' @description Converts raw PreK rubric responses to logical scores and computes
#' language, literacy, and overall scores (percent proficient) per observation.
#' @param df A data frame with PreK rubric columns plus `direct_to_ts_obs`, `ipg_rubric`.
#' @param prek_lang_cols Language item columns.
#' @param prek_lit_cols Literacy item columns.
#' @return A data frame with one row per observation.
#' @export
prek_summarise_teacher <- function(df,
                                   prek_lang_cols = prek_lang_cols,
                                   prek_lit_cols = prek_lit_cols) {
  prek_scored <- df |>
    collapse::fsubset(!is.na(direct_to_ts_obs) & direct_to_ts_obs != "Ongoing") |>
    collapse::fselect(
      "direct_to_ts_obs",
      "ipg_rubric",
      c(prek_lang_cols, prek_lit_cols)
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c(prek_lang_cols, prek_lit_cols)),
        ~ tlShiny::new_grade_ipg(.x, type = "prek")
      )
    ) |>
    dplyr::mutate(
      prek_lang_score = 100 * rowMeans(dplyr::pick(dplyr::all_of(prek_lang_cols)), na.rm = TRUE),
      prek_lit_score = 100 * rowMeans(dplyr::pick(dplyr::all_of(prek_lit_cols)), na.rm = TRUE),
      prek_overall_score = 100 * rowMeans(dplyr::pick(dplyr::all_of(c(prek_lang_cols, prek_lit_cols))), na.rm = TRUE)
    ) |>
    collapse::fselect(
      direct_to_ts_obs,
      ipg_rubric,
      prek_lang_score,
      prek_lit_score,
      prek_overall_score
    )

  prek_scored
}

#' @title Summarize PreK Scores Across Observations
#' @description Aggregates teacher-level PreK scores by observation timepoint
#' (`direct_to_ts_obs`) and reshapes to long format with Scores and Ns.
#' Domains: Language, Literacy, Overall.
#' @param data Output from \code{\link{prek_summarise_teacher}}.
#' @return A long-format data frame with columns: direct_to_ts_obs, Domain, Score, n.
#' @export
prek_summarise_overall <- function(data) {
  data1 <- data |>
    collapse::fgroup_by(direct_to_ts_obs) |>
    collapse::fsummarise(
      prek_overall_score = collapse::fmean(prek_overall_score, na.rm = TRUE),
      prek_lang_n = collapse::fsum(!is.na(prek_lang_score)),
      prek_lang_score = collapse::fmean(prek_lang_score, na.rm = TRUE),
      prek_lit_n = collapse::fsum(!is.na(prek_lit_score)),
      prek_lit_score = collapse::fmean(prek_lit_score, na.rm = TRUE),
      n = length(prek_overall_score)
    )

  data_ns <- data1 |>
    collapse::fselect(
      "direct_to_ts_obs",
      "prek_lang_n",
      "prek_lit_n",
      "n"
    ) |>
    collapse::pivot(
      how = "longer",
      names = list("Domain", "n"),
      values = c("prek_lang_n", "prek_lit_n", "n")
    ) |>
    collapse::fmutate(
      Domain = stringr::str_replace_all(
        Domain,
        c(
          "prek_lang_n" = "PreK Language",
          "prek_lit_n" = "PreK Literacy",
          "^n$" = "PreK Overall"
        )
      )
    )

  data_final <- data1 |>
    collapse::pivot(
      how = "longer",
      ids = "direct_to_ts_obs",
      names = list("Domain", "Score"),
      values = c("prek_overall_score", "prek_lang_score", "prek_lit_score")
    ) |>
    collapse::fmutate(
      Domain = stringr::str_replace_all(
        Domain,
        c(
          "prek_overall_score" = "PreK Overall",
          "prek_lang_score" = "PreK Language",
          "prek_lit_score" = "PreK Literacy"
        )
      )
    ) |>
    collapse::join(
      data_ns,
      how = "left",
      on = c("Domain", "direct_to_ts_obs")
    ) |>
    collapse::fmutate(
      direct_to_ts_obs = factor(
        direct_to_ts_obs,
        levels = c(
          "Baseline (first observation of the year)",
          "Mid-year (middle of service, if applicable)",
          "End of year (last observation of the year)"
        )
      )
    ) |>
    collapse::roworderv(cols = "direct_to_ts_obs") |>
    collapse::fsubset(!is.na(Score) & !is.na(direct_to_ts_obs))

  data_final
}
