test_that("multi_value keeps a multi-subject row under each of its subjects", {
  responses <- data.frame(
    id = 1:4,
    content_area = c("Math", "ELA/Literacy", "ELA/Literacy, Math", NA)
  )

  math_ids <- neg_cond_filter(responses, "Math", "content_area", multi_value = TRUE)$id
  ela_ids <- neg_cond_filter(responses, "ELA/Literacy", "content_area", multi_value = TRUE)$id
  both_ids <- neg_cond_filter(responses, c("Math", "ELA/Literacy"), "content_area", multi_value = TRUE)$id

  expect_equal(math_ids, c(1L, 3L))
  expect_equal(ela_ids, c(2L, 3L))
  # Picking both subjects still counts the leader once
  expect_equal(both_ids, 1:3)
  expect_equal(nrow(neg_cond_filter(responses, "", "content_area", multi_value = TRUE)), 4L)
})

test_that("default exact match is unchanged", {
  responses <- data.frame(
    id = 1:3,
    race = c("Asian", "Asian, White", "White")
  )

  expect_equal(neg_cond_filter(responses, "White", "race")$id, 3L)
  expect_equal(neg_cond_filter(responses, c("Asian", "White"), "race")$id, c(1L, 3L))
  expect_equal(nrow(neg_cond_filter(responses, character(0), "race")), 3L)
})
