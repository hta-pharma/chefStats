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
use_chefStats <-
  function(fn_name,
           fn_type = c(
             "stat_by_strata_by_trt",
             "stat_by_strata_across_trt",
             "stat_across_strata_across_trt"
           ),
           file_name = NULL) {
    check_use_chefStats_inputs(fn_name)
    type <- match.arg(fn_type)
    fn_path <-
      file.path("R", paste0(fn_name, ".R"))
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

error_no_dir <- function(dir){
  if (!dir.exists(dir)) {
    stop(paste0("Directory ", dir, " does not exist"))
  }  
}

extract_function_names <- function(dir) {
  x <- list.files(dir, full.names = TRUE, pattern = "*.[Rr]")
  fn_names_ls <- lapply(x, function(i) {
    lang_objs <- Filter(is.language, parse(i))
    fun_entries <-
      Filter(function(x) {
        grepl(", function", toString(x))
      }, lang_objs)
    sapply(fun_entries, function(fun_entry_i) {
      trimws(strsplit(toString(fun_entry_i), ",")[[1]][2])
    })
  })
  unlist(fn_names_ls)
}
error_function_already_exists <- function(new_fn_name, existing_fn_names){
  fn_exists <- any(new_fn_name == existing_fn_names)
  if (!fn_exists) {
    return(invisible(TRUE))
  }
  msg <- paste0(
    "The function name `",
    new_fn_name,
    "` already exists. Please choose another name"
  )
  stop(msg,call. = FALSE)
}

check_use_chefStats_inputs <- function(new_fn_name, dir = "R/"){
  error_no_dir(dir)
  existing_fn_names <- extract_function_names(dir)
  error_function_already_exists(new_fn_name, existing_fn_names)
  return(invisible(TRUE))
}
