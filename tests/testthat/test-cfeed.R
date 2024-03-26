test_that("Patrick Coyle's tidyRSS feed works", {
  expect_no_error(
    myfeeds <- tribble(
      ~source, ~url,
      "tidyverse", "https://www.tidyverse.org/blog/index.xml",
      "rOpenSci", "https://ropensci.org/rbloggers/index.xml",
      "brodrigues", "https://www.brodrigues.co/index.xml",
      "nrennie", "https://nrennie.rbind.io/blog/index.xml"
    ) %>%
      mutate(feed = url %>% map(
        ~.x %>%
          tidyRSS2::tidyfeed() %>%
          mutate(item_pub_date = ymd(item_pub_date)) %>%
          filter(item_pub_date >= today() - 60)
      )) %>%
      unnest(cols = c(feed)) %>%
      arrange(desc(item_pub_date), source) %>%
      mutate(
        link = paste0('<a target=_blank href="',
                      item_link,
                      '">',
                      item_title,
                      '</a>')
      ) %>%
      select(item_pub_date, source, link)
  )
})
