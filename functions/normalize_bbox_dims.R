normalize_bbox_dims <- function(bbox) {
  bbox_width <- abs(bbox[["xmax"]] - bbox[["xmin"]])
  bbox_height <- abs(bbox[["ymax"]] - bbox[["ymin"]])
  if (bbox_width > bbox_height) {
    split_diff <- (bbox_width - bbox_height)/2
    bbox[["ymax"]] <- bbox[["ymax"]] + split_diff
    bbox[["ymin"]] <- bbox[["ymin"]] - split_diff
  } else if (bbox_height > bbox_width) {
    split_diff <- (bbox_height - bbox_width)/2
    bbox[["xmax"]] <- bbox[["xmax"]] + split_diff
    bbox[["xmin"]] <- bbox[["xmin"]] - split_diff
  }
  bbox
}



