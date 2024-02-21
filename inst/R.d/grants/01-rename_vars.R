# Rename any variables first (for consistency). This should
# generally only be the grant_id (vs. a record_id).
function(x) {
  dplyr::rename(
    x,
    grant_id = record_id
  )
}
