### ============================================================================
### Integer and numeric matrix-like objects used in tests
###

### ----------------------------------------------------------------------------
### Setup
###

set_verbose(TRUE)

nrow <- 3L
ncol <- 4L
max_x <- nrow * ncol * 10L
modes <- c("integer", "double")
names(modes) <- modes

### ----------------------------------------------------------------------------
### List of matrix objects
### Adapted from those used in unit tests of matrixStats
###

list_of_matrix <- lapply(modes, function(mode) {
  # Base case: nrow * ncol matrix with no NA elements and with dimnames
  x <- matrix(seq_len(nrow * ncol) + 0.1, nrow, ncol,
              dimnames = list(paste0("R", seq_len(nrow)),
                              paste0("C", seq_len(ncol))))
  storage.mode(x) <- mode

  # Special case: Single-element matrix
  x_1x1 <- x[1, 1, drop = FALSE]

  # Special case: Empty matrix
  x_empty <- x[0, 0]

  # Special case: All NAs
  x_all_NA <- matrix(NA, nrow, ncol,
                     dimnames = list(paste0("R", seq_len(nrow)),
                                     paste0("C", seq_len(ncol))))
  storage.mode(x_all_NA) <- mode

  # Special case: All NaNs
  x_all_NaN <- matrix(NaN, nrow, ncol,
                      dimnames = list(paste0("R", seq_len(nrow)),
                                      paste0("C", seq_len(ncol))))
  storage.mode(x_all_NaN) <- mode

  # Special case: All NAs and NaNs
  x_all_NA_or_NaN <- matrix(c(NA, NaN), nrow, ncol,
                            dimnames = list(paste0("R", seq_len(nrow)),
                                            paste0("C", seq_len(ncol))))
  storage.mode(x_all_NA_or_NaN) <- mode

  if (mode == "double") {
    # Special case: All Inf
    x_all_Inf <- matrix(Inf, nrow, ncol,
                        dimnames = list(paste0("R", seq_len(nrow)),
                                        paste0("C", seq_len(ncol))))
    storage.mode(x_all_Inf) <- mode

    # Special case: All -Inf
    x_all_neg_Inf <- matrix(-Inf, nrow, ncol,
                            dimnames = list(paste0("R", seq_len(nrow)),
                                            paste0("C", seq_len(ncol))))
    storage.mode(x_all_neg_Inf) <- mode

    # Special case: Inf and -Inf
    x_all_Inf_or_neg_Inf <- matrix(c(Inf, -Inf), nrow, ncol,
                                   dimnames = list(paste0("R", seq_len(nrow)),
                                                   paste0("C", seq_len(ncol))))
    storage.mode(x_all_Inf_or_neg_Inf) <- mode

    val <- list("base_case" = x,
                "1x1" = x_1x1,
                "empty" = x_empty,
                "all_NA" = x_all_NA,
                "all_NaN" = x_all_NaN,
                "all_Inf" = x_all_Inf,
                "all_neg_Inf" = x_all_neg_Inf,
                "all_Inf_or_neg_Inf" = x_all_Inf_or_neg_Inf,
                "all_NA_or_NaN" = x_all_NA_or_NaN)
  } else {
    val <- list("base_case" = x,
                "1x1" = x_1x1,
                "empty" = x_empty,
                "all_NA" = x_all_NA,
                "all_NaN" = x_all_NaN,
                "all_NA_or_NaN" = x_all_NA_or_NaN)
  }
})

list_of_matrix_base_case <- lapply(list_of_matrix, "[[", "base_case")

### ----------------------------------------------------------------------------
### List of supported matrix-like objects that can be used as seeds of a
### DelayedArray
###

# TODO: Not testing matterArraySeed until these methods added to pkg
seed_types <- c("matrix", "Matrix",
                "data.frame", "DataFrame",
                "SolidRleArraySeed", "ChunkedRleArraySeed",
                "HDF5ArraySeed",
                # "matterArraySeed",
                "SeedBinder")
names(seed_types) <- seed_types

list_of_seeds <- lapply(seed_types, function(seed_type) {
  modes <- names(list_of_matrix)
  switch(seed_type,
         matrix = list_of_matrix,
         Matrix = setNames(
           object = lapply(modes, function(mode) {
             lapply(list_of_matrix[[mode]], Matrix::Matrix)
           }),
           nm = modes),
         data.frame = setNames(
           object = lapply(modes, function(mode) {
             # NOTE: Drop empty data.frame, which doesn't work work as a seed
             tmp <- list_of_matrix[[mode]]
             tmp <- tmp[-match("empty", names(tmp))]
             lapply(tmp, as.data.frame)
           }),
           nm = modes),
         DataFrame = setNames(
           object = lapply(modes, function(mode) {
             # NOTE: Drop empty DataFrame, which doesn't work work as a seed
             tmp <- list_of_matrix[[mode]]
             tmp <- tmp[-match("empty", names(tmp))]
             lapply(tmp, as, "DataFrame")
           }),
           nm = modes),
         SolidRleArraySeed = setNames(
           object = lapply(modes, function(mode) {
             lapply(list_of_matrix[[mode]], function(x) {
               DelayedArray:::RleArraySeed(
                 rle = Rle(x),
                 dim = dim(x),
                 dimnames = dimnames(x),
                 chunksize = NULL)
             })
           }),
           nm = modes),
         ChunkedRleArraySeed = setNames(
           object = lapply(modes, function(mode) {
             lapply(list_of_matrix[[mode]], function(x) {
               DelayedArray:::RleArraySeed(
                 rle = Rle(x),
                 dim = dim(x),
                 dimnames = dimnames(x),
                 chunksize = nrow(x))
             })
           }),
           nm = modes),
         HDF5ArraySeed = setNames(
           object = lapply(modes, function(mode) {
             lapply(list_of_matrix[[mode]], function(x) {
               seed(realize(x, "HDF5Array"))
             })
           }),
           nm = modes),
         matterArraySeed = setNames(
           object = lapply(modes, function(mode) {
             # NOTE: Drop empty matrix, which doesn't work work as a matter_mat
             tmp <- list_of_matrix[[mode]]
             tmp <- tmp[-match("empty", names(tmp))]
             lapply(tmp, function(x) {
               matterArray::matterArraySeed(
                 matter::matter_mat(data = x,
                                    datamode = mode,
                                    nrow = nrow(x),
                                    ncol = ncol(x),
                                    dimnames = dimnames(x)))
             })
           }),
           nm = modes),
         SeedBinder = setNames(
           object = lapply(modes, function(mode) {
             lapply(list_of_matrix[[mode]], function(x) {
               DelayedArray:::.new_SeedBinder(list(x), along = 1L)
             })
           }),
           nm = modes)
  )
})

### ----------------------------------------------------------------------------
### List of DelayedMatrix objects with different seed types
###

list_of_DelayedMatrix <- setNames(
  lapply(names(list_of_seeds), function(seed_type) {
    seeds <- list_of_seeds[[seed_type]]
    setNames(lapply(names(seeds), function(mode) {
      lapply(seeds[[mode]], DelayedArray)
    }),
    names(seeds))
  }),
  names(list_of_seeds))

list_of_DelayedMatrix_base_case <- lapply(list_of_DelayedMatrix, function(x) {
  lapply(x, "[[", "base_case")
})
