set.seed(17711L)

smoo <- tf_rgp(10, nugget = 0)
rough <- tf_rgp(10, arg = 121L, nugget = .2, scale = .005)
narrow <- tf_jiggle(tf_rgp(10, arg = 11L, nugget = 0))
irr <- tf_sparsify(tf_jiggle(smoo))
sparse <- tf_sparsify(smoo)

smoo_list <- tf_evaluations(smoo)
smoo_matrix <- as.matrix(smoo)
smoo_df <- tf_unnest(smoo)

irr_list <- tf_evaluations(irr)
irr_matrix <- suppressWarnings(as.matrix(irr))
irr_df <-  tf_unnest(irr)
narrow_df <-  tf_unnest(narrow) 

