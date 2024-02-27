helper_pipeline <- function(ep) {
  ep <- chef::add_id(ep)
  ep_fn_map <-
    suppressWarnings(chef::unnest_endpoint_functions(ep))

  user_def_fn <-
    chef::mk_userdef_fn_dt(ep_fn_map, env = environment())

  fn_map <-
    merge(ep_fn_map[, .(endpoint_spec_id, fn_hash)], user_def_fn, by = "fn_hash")
  adam_db <-
    chef::fetch_db_data(study_metadata = list(),
                        fn_dt = user_def_fn)
  ep_and_data <- chef::filter_db_data(ep, ep_fn_map, adam_db)
  ep_data_key <- ep_and_data$ep
  analysis_data_container <-
    ep_and_data$analysis_data_container
  ep_expanded <-
    chef::expand_over_endpoints(ep_data_key, analysis_data_container)
  ep_ev_index <-
    chef::add_event_index(ep_expanded, analysis_data_container)
  ep_crit_endpoint <-
    chef::apply_criterion_endpoint(ep_ev_index, analysis_data_container, fn_map)
  crit_accept_by_strata_by_trt <-
    chef::apply_criterion_by_strata(ep_crit_endpoint,
                                    analysis_data_container,
                                    fn_map,
                                    type = "by_strata_by_trt")
  crit_accept_by_strata_across_trt <-
    chef::apply_criterion_by_strata(crit_accept_by_strata_by_trt,
                                    analysis_data_container,
                                    fn_map,
                                    type = "by_strata_across_trt")
  ep_prep_for_stats <- chef::prepare_for_stats(crit_accept_by_strata_across_trt,
                                               analysis_data_container,
                                               fn_map,
                                               type = "stat_by_strata_by_trt")
  return(list(ep = ep_prep_for_stats[], analysis_data_container = analysis_data_container))
}

helper_pipeline_by_strata_across_trt <- function(ep) {
  ep <- chef::add_id(ep)
  ep_fn_map <-
    suppressWarnings(chef::unnest_endpoint_functions(ep))

  user_def_fn <-
    chef::mk_userdef_fn_dt(ep_fn_map, env = environment())

  fn_map <-
    merge(ep_fn_map[, .(endpoint_spec_id, fn_hash)], user_def_fn, by = "fn_hash")
  adam_db <-
    chef::fetch_db_data(study_metadata = list(),
                        fn_dt = user_def_fn)
  ep_and_data <- chef::filter_db_data(ep, ep_fn_map, adam_db)
  ep_data_key <- ep_and_data$ep
  analysis_data_container <-
    ep_and_data$analysis_data_container
  ep_expanded <-
    chef::expand_over_endpoints(ep_data_key, analysis_data_container)
  ep_ev_index <-
    chef::add_event_index(ep_expanded, analysis_data_container)
  ep_crit_endpoint <-
    chef::apply_criterion_endpoint(ep_ev_index, analysis_data_container, fn_map)
  crit_accept_by_strata_by_trt <-
    chef::apply_criterion_by_strata(ep_crit_endpoint,
                                    analysis_data_container,
                                    fn_map,
                                    type = "by_strata_by_trt")
  crit_accept_by_strata_across_trt <-
    chef::apply_criterion_by_strata(crit_accept_by_strata_by_trt,
                                    analysis_data_container,
                                    fn_map,
                                    type = "by_strata_across_trt")
  ep_prep_for_stats <- chef::prepare_for_stats(crit_accept_by_strata_across_trt,
                                               analysis_data_container,
                                               fn_map,
                                               type = "stat_by_strata_across_trt")
  return(list(ep = ep_prep_for_stats[], analysis_data_container = analysis_data_container))
}

helper_pipeline_across_strata_across_trt <- function(ep) {
  ep <- chef::add_id(ep)
  ep_fn_map <-
    suppressWarnings(chef::unnest_endpoint_functions(ep))

  user_def_fn <-
    chef::mk_userdef_fn_dt(ep_fn_map, env = environment())

  fn_map <-
    merge(ep_fn_map[, .(endpoint_spec_id, fn_hash)], user_def_fn, by = "fn_hash")
  adam_db <-
    chef::fetch_db_data(study_metadata = list(),
                        fn_dt = user_def_fn)
  ep_and_data <- chef::filter_db_data(ep, ep_fn_map, adam_db)
  ep_data_key <- ep_and_data$ep
  analysis_data_container <-
    ep_and_data$analysis_data_container
  ep_expanded <-
    chef::expand_over_endpoints(ep_data_key, analysis_data_container)
  ep_ev_index <-
    chef::add_event_index(ep_expanded, analysis_data_container)
  ep_crit_endpoint <-
    chef::apply_criterion_endpoint(ep_ev_index, analysis_data_container, fn_map)
  crit_accept_by_strata_by_trt <-
    chef::apply_criterion_by_strata(ep_crit_endpoint,
                                    analysis_data_container,
                                    fn_map,
                                    type = "by_strata_by_trt")
  crit_accept_by_strata_across_trt <-
    chef::apply_criterion_by_strata(crit_accept_by_strata_by_trt,
                                    analysis_data_container,
                                    fn_map,
                                    type = "by_strata_across_trt")
  ep_prep_for_stats <- chef::prepare_for_stats(crit_accept_by_strata_across_trt,
                                               analysis_data_container,
                                               fn_map,
                                               type = "stat_across_strata_across_trt")
  return(list(ep = ep_prep_for_stats[], analysis_data_container = analysis_data_container))
}
