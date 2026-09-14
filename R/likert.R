# Qualtrics stores the recode integer and resolves labels at export time, so the
# words attached to a Likert response change whenever the survey definition does
# while the integer stays put. Everything here keys on the leading integer so it
# stays correct across label rewrites and the FY27 5 -> 6 point agreement scale.

#' @title Likert Score
#' @description Extracts the leading integer from a Qualtrics Likert label, e.g.
#' "4 - Somewhat Agree" -> 4. Numeric input is returned as integer.
#' @param x a character, factor, or numeric vector of Likert responses
#' @return an integer vector, NA where no leading integer is present
#' @examples
#' likert_score(c("1 - Strongly disagree", "5 - Agree", NA))
#' @export
likert_score <- function(x) {
  if (is.numeric(x)) {
    return(as.integer(x))
  }

  as.integer(stringr::str_extract(as.character(x), "^\\s*\\d+"))
}

#' @title Percent Agree
#' @description Percent of non-missing responses scoring at or above a threshold.
#' The default of 4 selects Agree/Strongly agree on the 5 point scale and
#' Somewhat Agree/Agree/Strongly Agree on the 6 point scale, so it gives the
#' same answer on historical data under both.
#' @param x a vector of Likert responses
#' @param threshold the lowest score counted as agreement
#' @return a single percentage between 0 and 100, NA when there are no responses
#' @examples
#' pct_agree(c("4 - Agree", "5 - Strongly agree", "2 - Disagree", NA))
#' @export
pct_agree <- function(x, threshold = 4) {
  scores <- likert_score(x)
  scores <- scores[!is.na(scores)]

  if (length(scores) == 0) {
    return(NA_real_)
  }

  100 * mean(scores >= threshold)
}

#' @title Agreement Scale Levels
#' @description The FY27 6 point agreement scale labels in order.
#' @param wrap whether to break the long labels over two lines for plot legends
#' @return a character vector of length 6
#' @export
agree_scale_levels <- function(wrap = FALSE) {
  scale_levels <- c(
    "1 - Strongly Disagree",
    "2 - Disagree",
    "3 - Somewhat Disagree",
    "4 - Somewhat Agree",
    "5 - Agree",
    "6 - Strongly Agree"
  )

  if (wrap) {
    scale_levels <- stringr::str_replace(scale_levels, "^(\\d - \\w+) ", "\\1\n")
  }

  scale_levels
}

#' @title Satisfaction Scale Levels
#' @description The FY27 6 point satisfaction scale labels in order.
#' @param wrap whether to break the long labels over two lines for plot legends
#' @return a character vector of length 6
#' @export
satisfaction_scale_levels <- function(wrap = FALSE) {
  scale_levels <- c(
    "1 - Very dissatisfied",
    "2 - Dissatisfied",
    "3 - Somewhat dissatisfied",
    "4 - Somewhat satisfied",
    "5 - Satisfied",
    "6 - Very satisfied"
  )

  if (wrap) {
    scale_levels <- stringr::str_replace(scale_levels, "^(\\d - \\w+) ", "\\1\n")
  }

  scale_levels
}

#' @title Agreement Scale Factor
#' @description Relabels Likert responses onto the 6 point agreement scale by
#' their leading integer, discarding whatever words Qualtrics exported. Responses
#' collected on the 5 point scale keep their integer, so a historical
#' "3 - Neither agree nor disagree" becomes "3 - Somewhat Disagree".
#' @param x a vector of Likert responses
#' @param wrap whether to use the two line labels from agree_scale_levels()
#' @param scale_levels the ordered labels to map scores 1..n onto
#' @return a factor with all scale levels present
#' @examples
#' as_agree_scale(c("3 - Neither agree nor disagree", "6 - Strongly Agree"))
#' @export
as_agree_scale <- function(x, wrap = FALSE, scale_levels = agree_scale_levels(wrap = wrap)) {
  scores <- likert_score(x)
  scores[!scores %in% seq_along(scale_levels)] <- NA_integer_

  factor(scale_levels[scores], levels = scale_levels)
}

#' @title Agreement Scale Colors
#' @description Six brand blues from black to #00ACF0 for the agreement scale,
#' plus the text colour readable on each.
#' @param wrap whether to name the colours with the two line labels
#' @param scale_levels the labels to name the colours with
#' @param type "fill" for bar colours or "text" for label colours
#' @return a named character vector of hex colours
#' @export
agree_scale_colors <- function(wrap = FALSE, scale_levels = agree_scale_levels(wrap = wrap), type = c("fill", "text")) {
  type <- match.arg(type)

  scale_colors <- switch(type,
    fill = c("#040404", "#092633", "#034661", "#016890", "#008ABF", "#00ACF0"),
    text = c("white", "white", "white", "white", "black", "black")
  )

  stats::setNames(scale_colors, scale_levels)
}
