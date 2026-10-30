library(sf)

# Functions on this script:
# to_kriging_crs(): decide what's the best CRS for your data
# prep_for_gstlearn(): prepare your data (FU polygon and UWTV stations) for gstlearn
# suggest_lag_params(): get suggestions for nlag and dlag to use in kriging

# Decide + apply a good CRS for kriging
# - If polygon sits inside one UTM zone -> use that zone (EPSG:326xx / 327xx)
# - If it crosses a zone boundary -> use a local Transverse Mercator centered on centroid
# Returns list(poly_ll, poly_proj, crs_used)
to_kriging_crs <- function(coords) {# --- projection chooser ---
  # 1) Build lon/lat polygon (WGS84). 'coords' must be a closed ring (nx2).
  poly_ll <- st_sfc(st_polygon(list(coords)), crs = 4326)
  # 2) Get centroid (lon, lat)
  cen <- st_coordinates(st_centroid(poly_ll))
  lon0 <- cen[1]; lat0 <- cen[2]
  # 3) UTM zone(s) touched by polygon bbox
  bb  <- st_bbox(poly_ll)
  utm_zone <- function(lon) floor((lon + 180) / 6) + 1
  zmin <- utm_zone(bb["xmin"]); zmax <- utm_zone(bb["xmax"])
  crosses_zone <- (zmin != zmax)
  
  if (!crosses_zone) {
    # --- Use standard UTM of the centroid ---
    zone <- utm_zone(lon0)
    epsg <- if (lat0 >= 0) 32600 + zone else 32700 + zone
    poly_proj <- st_transform(poly_ll, crs = epsg)
    crs_used  <- paste0("EPSG:", epsg)
  } else {
    proj_tm <- paste0(
      "+proj=tmerc +lat_0=", lat0,
      " +lon_0=", lon0,
      " +k=0.9996 +x_0=500000 +y_0=0 +datum=WGS84 +units=m +no_defs"
    )
    poly_proj <- st_transform(poly_ll, crs = proj_tm)
    crs_used  <- proj_tm
  }
  # Return the projected polygon (meters) you can hand to gstlearn (or extract coordinates from)
  list(poly_ll = poly_ll, poly_proj = poly_proj, crs_used = crs_used)
}

# --- high-level helper: prep data for gstlearn ---
prep_for_gstlearn <- function(coords_polygon, pts_sf, grid_sf = NULL, value_col = NULL) {
  res <- to_kriging_crs(coords_polygon)
  crs_target <- st_crs(res$poly_proj)
  
  # project inputs
  pts_proj <- st_transform(pts_sf, crs = crs_target)
  grid_proj <- if (!is.null(grid_sf)) st_transform(grid_sf, crs = crs_target) else NULL
  
  # polygon coords (for PolyElem)
  poly_coords <- st_coordinates(res$poly_proj)[,1:2]
  
  # sample points coords
  pts_coords <- st_coordinates(pts_proj)
  df_pts <- data.frame(
    x = pts_coords[,1],
    y = pts_coords[,2]
  )
  if (!is.null(value_col)) {
    df_pts$AdjustedBurrowDensity <- pts_proj[[value_col]]
  }
  
  # grid points coords
  df_grid <- NULL
  if (!is.null(grid_proj)) {
    grid_coords <- st_coordinates(grid_proj)
    df_grid <- data.frame(x = grid_coords[,1], y = grid_coords[,2])
  }
  
  list(
    polygon_xy = poly_coords,  # matrix [n,2]
    points_df  = df_pts,       # data.frame for gstlearn
    grid_df    = df_grid,      # optional
    crs_used   = res$crs_used
  )
}


# What to use for nlag and dlag?
# inputs:
#   xy: matrix/data.frame with columns x,y in METERS (projected)
#   min_pairs: desired minimum number of point pairs per lag (soft target)
#   max_frac: fraction of max pair distance to cover (default 0.5)
#   dlag_mult: multiplier on median NN distance (tune 0.8-1.5)
suggest_lag_params <- function(xy, min_pairs = 50, max_frac = 0.5, dlag_mult = 1.0) {
  stopifnot(ncol(xy) >= 2)
  xy <- as.matrix(xy[,1:2])
  
  n <- nrow(xy)
  if (n < 10) stop("Too few points for a stable variogram")
  
  # 1) nearest-neighbor distance (fast enough for a few thousand points)
  # compute pairwise distances to find NN; avoid full n^2 if n is huge in production
  dmat <- as.matrix(dist(xy))
  diag(dmat) <- Inf
  nn <- apply(dmat, 1, min, na.rm = TRUE)
  med_nn <- median(nn)
  
  # 2) initial dlag from median NN distance
  dlag <- dlag_mult * med_nn
  if (dlag <= 0) stop("Non-positive dlag; check coordinates/CRS")
  
  # 3) max distance to model ~ half the max pair distance
  max_pair <- max(dmat[is.finite(dmat)])
  maxdist  <- max_frac * max_pair
  
  # 4) initial nlag
  nlag <- max(8, min(25, floor(maxdist / dlag))) # keep within a sensible range
  
  # 5) crude check on pair counts per lag (expected average)
  # total pairs:
  tot_pairs <- n * (n - 1) / 2
  # assume uniform distribution of pair distances as a rough approximation:
  exp_pairs_per_lag <- tot_pairs / nlag
  
  # If expected pairs per lag are low, widen dlag (reduce nlag) until OK
  while (nlag > 10 && exp_pairs_per_lag < min_pairs) {
    nlag <- nlag - 1
    exp_pairs_per_lag <- tot_pairs / nlag
  }
  
  list(dlag = dlag, nlag = nlag, maxdist = nlag * dlag,
       med_nn = med_nn, max_pair = max_pair,
       exp_pairs_per_lag = exp_pairs_per_lag)
}
