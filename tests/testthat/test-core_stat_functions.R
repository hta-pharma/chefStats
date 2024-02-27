
test_that("bernard works", {
  two_by_two_1 <-
    data.frame(N_subev = c(14, 5), N_no_event = c(396, 404))
  results <- barnard_test(two_by_two_1)
  expect_equal(results, 0.03893186, tolerance = 1e-6)
})

# test_that("bernard works with 1 and 0 events", {
# two_by_two_2<- data.frame(N_subev = c(1, 0), N_no_event = c(396, 404))
#   results <- barnard_test(two_by_two_2)
#   expect_equal(results, 0.3693807, tolerance = 1e-6)
# })

test_that("bernard fails when no events", {
  tbt_no_events <-
    data.frame(N_subev = c(0, 0), N_no_event = c(396, 404))
  expect_error(barnard_test(tbt_no_events))
})

test_that("bernard fails when no patients in one arm", {
  tbt_no_exposed <-
    data.frame(N_subev = c(0, 5), N_no_event = c(0, 404))
  expect_error(barnard_test(tbt_no_exposed))
})

test_that("relative risk works", {
  two_by_two_1 <-
    data.frame(N_subev = c(14, 5), N_no_event = c(396, 404))
  results <- relative_risk(two_by_two_1)
  x <- unlist(results)
  check_vals <- c(2.793171, 0.5162795, 7.683435, 1.015406)
  names(check_vals) <- c("RR", "SE", "RRUL", "RRLL")
  expect_equal(x, check_vals, tolerance = 1e-6)
})

test_that("Risk difference works", {
  two_by_two_1 <-
    data.frame(N_subev = c(14, 5), N_no_event = c(396, 404))
  results <- risk_diff(two_by_two_1)
  x <- unlist(results)
  check_vals <- c(2.19214, 0.01048639, 4.247435, 0.1368458)
  names(check_vals) <- c("RD", "SE", "RDUL", "RDLL")
  expect_equal(x, check_vals, tolerance = 1e-6)
})

test_that("OR works", {
  two_by_two_1 <-
    as.matrix(data.frame(N_subev = c(14, 5), N_no_event = c(396, 404)))
  results <- odds_ratio_amnog(two_by_two_1)
  x <- unlist(results)
  check_vals <- c(2.856566, 0.5257652, 8.005277, 1.019324)
  names(check_vals) <- c("OR", "SE", "ORUL", "ORLL")
  expect_equal(x, check_vals, tolerance = 1e-6)
})

test_that("Breslow-Day works", {
  x <-
    list(
      F = data.frame(N_subev = c(0, 8), N_no_event = c(200, 196)),
      M = data.frame(N_subev = c(5, 6), N_no_event = c(204, 200))
    )
  two_by_two_by_k <-
    array(c(unlist(x)),
          dim = c(2, 2, 2),
          dimnames = list(c("A", "B"), c("N_subev", "N_subjt"), c("F", "M")))
  expect_equal(breslowdaytest(two_by_two_by_k)$p.value, 0.02737539, tolerance = 1e-6)
})

test_that("Breslow-Day errors when no events in a subgourp", {
  x_no_event_subgroup <-
    list(F = data.frame(N_subev = c(0, 0), N_no_event = c(200, 196)),
         M = data.frame(N_subev = c(5, 6), N_no_event = c(204, 200)))

  two_by_two_by_k_2 <-
    array(c(unlist(x_no_event_subgroup)),
          dim = c(2, 2, 2),
          dimnames = list(c("A", "B"), c("N_subev", "N_subjt"), c("F", "M")))

  expect_error(breslowdaytest(two_by_two_by_k_2))
})
