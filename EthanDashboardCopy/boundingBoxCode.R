# BOUNDING BOX CODE

bounding_boxes <- do.call(rbind, lapply(1:nrow(world), function(i) {
  bb <- st_bbox(world[i, ])
  data.frame(
    name = world$admin[i],
    xmin = bb["xmin"],
    ymin = bb["ymin"],
    xmax = bb["xmax"],
    ymax = bb["ymax"]
  )
}))