#' Make new chefStats functions
#'
#' @param fn_name Name of the function. If file_name is `NULL` then `fn_name`
#'   will be used to make the file name
#' @param fn_type Type of statistical function, one of `stat_by_strata_by_trt`,
#'   `stat_by_strata_across_trt`, or `stat_across_strata_across_trt`
#' @param file_name Name of the file to store function in (if different from
#'   `fn_name`)
#'
#' @return Nothing, run for side-effect (writes file to disk)
#' @export
#'
#' @examples
use_chefStats <-
  function(fn_name,
           fn_type = c(
             "stat_by_strata_by_trt",
             "stat_by_strata_across_trt",
             "stat_across_strata_across_trt"
           ),
           file_name = NULL) {
    type <- match.arg(fn_type)
    fn_path <-
      normalizePath(file.path("R", paste0(fn_name, ".R")), mustWork = FALSE)
    switch(
      type,
      stat_by_strata_by_trt = usethis::use_template(
        "template-stat_by_strata_by_trt.R",
        data = list(fn_name = fn_name),
        package = "chefStats",
        save_as = fn_path,
        open = TRUE
      ),
      stat_by_strata_across_trt = usethis::use_template(
        "template-stat_by_strata_across_trt.R",
        data = list(fn_name = fn_name),

        package = "chefStats",
        save_as = fn_path,
        open = TRUE
      ),
      stat_across_strata_across_trt = usethis::use_template(
        "template-stat_across_strata_across_trt.R",
        data = list(fn_name = fn_name),
        package = "chefStats",
        save_as = fn_path,
        open = TRUE
      )
    )

  }
