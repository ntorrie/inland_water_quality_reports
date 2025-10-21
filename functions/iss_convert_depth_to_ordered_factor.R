iss_convert_depth_to_ordered_factor <- function(dat) {
  dat %>%
    mutate(depth = factor(sensor_depth_at_deployment_m)) %>%
    # assign levels to the factor based on the numeric values of DEPTH
    mutate(depth = ordered(
      depth,
      levels = as.numeric(levels(depth))[order(as.numeric(levels(depth)))]
    )) %>%
    arrange(depth) %>%
    mutate(sensor_depth_at_deployment_m = depth) %>%
    select(-depth)
}