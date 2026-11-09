# Saved counterpart to the archived Pearson PFpw table. Rebuild explicitly
# when input.RData changes; switching methods never computes the full matrix.
build_pairwise_spearman <- function(PFpw, run.data, joint.cor, joint.subject) {
  result <- data.table::copy(PFpw)
  a <- as.character(result$Metabolite.1)
  b <- as.character(result$Metabolite.2)
  for (tissue in c('plasma', 'fibroblast')) {
    dat <- run.data[[tissue]]$mData[, joint.cor$metabolite, drop = FALSE]
    for (scope in c('all', 'shared')) {
      selected <- if (scope == 'shared') joint.subject[[if (tissue == 'plasma') 'p.ind' else 'f.ind']] else seq_len(nrow(dat))
      r <- correlation_matrix_stats(dat[selected, , drop = FALSE], 'spearman')$r
      values <- r[cbind(match(a, rownames(r)), match(b, colnames(r)))]
      data.table::set(result, j = paste(scope, tissue, 'r', sep = '.'), value = round(values, 2))
    }
  }
  result
}

load_pairwise_spearman <- function(input_path = 'data/input.RData', cache_path = 'data/PFpw_spearman.rds') {
  rebuild <- 'Run Rscript scripts/build_pairwise_spearman.R from the app folder.'
  if (!file.exists(cache_path)) stop('Saved Spearman table is missing. ', rebuild)
  saved <- readRDS(cache_path)
  if (!identical(saved$metadata$schema_version, 1L) ||
      !identical(saved$metadata$input_md5, unname(tools::md5sum(input_path)))) {
    stop('Saved Spearman table does not match input.RData. ', rebuild)
  }
  data.table::as.data.table(saved$table)
}
