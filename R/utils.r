



connect_to_Domo <- function() {
  #connect to Domo if no connection already up.
  if (class(try(DomoR::owner(), silent = TRUE)) == "try-error") {
    customer <- "rapid7"
    token <- Sys.getenv("DOMO_TOKEN")

    if (token == "") {
      token <- rstudioapi::askForPassword("Enter Domo Token")
    }

    DomoR::init(customer, token)
  }
}



get_domo_dataset_hash_dict <- function() {
  #Used to fetch dataset from Domo.
  #Returns dictionary associating a Domo dataset name with its hash value.

  c(
    all_renewal_information_product = "6ab25d5d-ecdf-4566-9825-aaf81621b6a1",
    renewal_opp_snapshots = "bb3fba00-150c-4eee-8fe3-bd284a7d85a9",
    customer_health_score_overall = "df501f4b-6489-4f85-9a70-440cc2e1b1c6",
    customer_trending = "7b04334b-a362-45c0-8bd1-81ef91851f35",
    customer_cohort = "3194de2d-9fcb-4c8a-8164-3da0ffa14b37",
    renewal_pipeline = "87f5154f-5316-11e5-a45c-5b5e5910d4fb",
    total_bookings_with_arr = "e0f4733b-3fc9-4b5d-9452-76025c5c97b8",
    arr_historical_with_cust_segmentation = "6e861e44-08fe-4483-980a-5ade2484c3f3",
    live_total_bookings = "a9fe0b0d-2880-4674-a9e4-c0b315a948d8",
    item_cat_product_map = "8541ce3e-932b-49be-8c75-d26c1438dbba",
    cohort = "3194de2d-9fcb-4c8a-8164-3da0ffa14b37",
    arr_waterfall = "7f602a73-b2b0-4f61-9456-7c73ce04ba35",
    all_time_customer = "827b57b0-9713-4882-a548-3025bf467df1"
  )
}



browse_dataset <- function(dataset_hash) {
  url <-
    sprintf("https://rapid7.domo.com/datasources/%s/details/overview",
            dataset_hash)
  browseURL(url)
}



rm_outlier <- function(x, min_quantile, max_quantile) {
  #1.5 * inter-quartile range to detect outliers and remove them from x.

  assertthat::assert_that(is.numeric(x),
                          is.numeric(min_quantile),
                          is.numeric(max_quantile))

  q_25 <- quantile(x, min_quantile, na.rm = TRUE)
  q_75 <- quantile(x, max_quantile, na.rm = TRUE)
  IQR = q_75 - q_25

  x <- x[x < (1.5 * IQR + q_75) & x > (q_25 - 1.5 * IQR)]
}



calculate_quarter_of_date <- function(date_vec) {
  #---
  assertthat::assert_that(lubridate::is.Date(date_vec))
  #---

  month = lubridate::month(date_vec)
  year = lubridate::year(date_vec)

  quarter <- dplyr::case_when(
    month %in% c(1, 2, 3) ~ 1,
    month %in% c(4, 5, 6) ~ 2,
    month %in% c(7, 8, 9) ~ 3,
    month %in% c(10, 11, 12) ~ 4
  )

  quarter <- paste0(year, "-Q", quarter)
}


get_quarter_lag <- function(quarter, lag = 1) {
  #lag < 0 will get future quarters.

  assertthat::assert_that(is.character(quarter))
  assertthat::assert_that(all(stringr::str_detect(quarter, "^[0-9]+-")),
                          all(stringr::str_detect(quarter, "-Q[1-4]")),
                          msg = "Quarter must be in YYYY-Q[1-4] format")

  y <-
    stringr::str_extract(quarter, stringr::regex("^[0-9]+(?=-)")) %>% as.numeric()
  q <- stringr::str_extract(quarter, "[1-4]$") %>% as.numeric()

  q_lag <- q - lag
  y <- y + floor((q_lag - 1) / 4)

  q_lag_mod <- q_lag %% 4
  q_lag <- dplyr::if_else(q_lag_mod == 0, 4, q_lag_mod)

  paste0(y, "-Q", q_lag)
}


quarter_to_numeric <- function(quarter) {
  assertthat::assert_that(is.character(quarter))
  assertthat::assert_that(all(stringr::str_detect(quarter, "^[0-9]+-")),
                          all(stringr::str_detect(quarter, "-Q[1-4]")),
                          msg = "Quarter must be in YYYY-Q[1-4] format")

  gsub("-Q", "", quarter) %>%
    as.numeric()
}


numeric_to_quarter <- function(quarter_numeric) {
  assertthat::assert_that(is.numeric(quarter_numeric))

  quarter <- quarter_numeric %% 10
  assertthat::assert_that(all(quarter %in% c(1, 2, 3, 4)),
                          msg = "Quarter must be one of 1,2,3,4")

  year <- floor(quarter_numeric / 10)

  paste0(year, "-Q", quarter)
}


one_hot_encode <- function(df, components) {
  #One hot encode components of `character` and `factor` types (note that factors are first converted to character type),
  #and returns a dataframe consisting of undummified components and dummified components.

  #using namespace with caret's dummyvars raises error with msg "missing contr.ltfr object'. So we create our own version of
  #contr.ltfr.
  contr.ltfr <-
    assign("contr.ltfr", caret::contr.ltfr, envir = .GlobalEnv)

  components <- intersect(colnames(df), components)
  df <- df %>% dplyr::mutate_if(is.factor, as.character)
  chr_components <-
    components[lapply(components, function(x) {
      is.character(df[[x]])
    }) %>%
      unlist()]

  #---
  lapply(chr_components, function(x) {
    assertthat::assert_that(is.character(df[[x]]))
  })
  #---

  if (length(chr_components) > 0) {
    my_formula <-
      as.formula(paste0("~", glue::collapse(chr_components, sep = "+")))

    dummy <- caret::dummyVars(my_formula, data = df)

    dplyr::bind_cols(df[, setdiff(components, chr_components)],
                     predict(dummy, newdata = df) %>% tibble::as.tibble())
  }
  else {
    df[, components]
  }
}
