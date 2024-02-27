ep_base <-
  purrr::partial(
    chef::mk_endpoint_str,
    study_metadata = list(),
    pop_var = "SAFFL",
    pop_value = "Y",
    treatment_var = "TRT01A",
    treatment_refval = "Xanomeline High Dose"
  )
