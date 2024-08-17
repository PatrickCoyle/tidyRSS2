test_that("Patrick Coyle's tidyRSS feed works", {
  expect_no_error(
    myfeeds <- dplyr::tribble(
      ~source, ~url,
      "tidyverse", "https://www.tidyverse.org/blog/index.xml",
      "rOpenSci", "https://ropensci.org/rbloggers/index.xml"
    ) %>%
    dplyr::mutate(feed = url %>% purrr::map(
      ~.x %>%
      tidyRSS2::tidyfeed() %>%
      dplyr::mutate(item_pub_date = lubridate::ymd(item_pub_date)) %>%
      dplyr::filter(item_pub_date >= lubridate::today() - 60)
    )) %>%
    tidyr::unnest(cols = c(feed)) %>%
    dplyr::arrange(dplyr::desc(item_pub_date), source) %>%
    dplyr::mutate(
      link = paste0('<a target=_blank href="',
      item_link,
      '">',
      item_title,
      '</a>')
    ) %>%
    dplyr::select(item_pub_date, source, link)
  )
})
