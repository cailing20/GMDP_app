# Shared correlation calculations. Spearman p values use the asymptotic test
# (exact = FALSE), which accommodates tied ranks. Only finite pairs are used.
correlation_method <- function(method = "pearson") {
  if (is.null(method) || length(method) == 0L) return("pearson")
  match.arg(tolower(method), c("pearson", "spearman"))
}

correlation_method_label <- function(method = "pearson") {
  if (correlation_method(method) == "spearman") "Spearman" else "Pearson"
}

correlation_coefficient_label <- function(method = "pearson") {
  if (correlation_method(method) == "spearman") "rho" else "r"
}

correlation_method_input <- function(inputId) {
  shiny::radioButtons(inputId, "Correlation method",
                      choices = c("Pearson" = "pearson", "Spearman" = "spearman"),
                      selected = "pearson", inline = TRUE)
}

correlation_summary <- function(x, y, method = "pearson") {
  method <- correlation_method(method)
  if (length(x) != length(y)) stop("Correlation vectors must have equal length.")
  keep <- is.finite(x) & is.finite(y)
  x <- x[keep]
  y <- y[keep]
  n <- length(x)
  missing <- list(estimate = NA_real_, p.value = NA_real_, n = n)
  if (n < 3L || length(unique(x)) < 2L || length(unique(y)) < 2L) return(missing)
  result <- tryCatch(
    if (method == "spearman") stats::cor.test(x, y, method = method, exact = FALSE)
    else stats::cor.test(x, y, method = method),
    error = function(e) NULL
  )
  if (is.null(result)) return(missing)
  list(estimate = unname(result$estimate), p.value = result$p.value, n = n)
}

correlation_label <- function(x, y, method = "pearson", digits = 2,
                              include_p = FALSE, stars = TRUE) {
  result <- correlation_summary(x, y, method)
  label <- paste0(correlation_method_label(method), " ",
                  correlation_coefficient_label(method), " = ",
                  if (is.finite(result$estimate)) round(result$estimate, digits) else "NA")
  if (include_p) {
    label <- paste0(label, ", p = ",
                    if (is.finite(result$p.value)) signif(result$p.value, 2) else "NA")
  } else if (stars && is.finite(result$p.value) && result$p.value < 0.05) {
    label <- paste0(label, "*")
  }
  if (!is.finite(result$estimate)) label <- paste0(label, " (n = ", result$n, ")")
  label
}

correlation_matrix_stats <- function(x, method = "pearson") {
  method <- correlation_method(method)
  x <- as.matrix(x)
  if (!is.numeric(x)) stop("Correlation matrix input must be numeric.")
  finite <- is.finite(x)
  n <- crossprod(finite * 1L)
  x[!finite] <- NA_real_
  p <- ncol(x)
  r <- matrix(NA_real_, p, p, dimnames = list(colnames(x), colnames(x)))
  if (nrow(x) > 0L && p > 0L) {
    r <- suppressWarnings(stats::cor(x, use = "pairwise.complete.obs", method = method))
    r[r > 1] <- 1
    r[r < -1] <- -1
    r[n < 3L] <- NA_real_
  }
  # Both Pearson and the Spearman asymptotic test use this t approximation.
  test <- abs(r) * sqrt(pmax(n - 2, 0) / pmax(1 - r * r, 0))
  P <- 2 * stats::pt(test, df = pmax(n - 2, 1), lower.tail = FALSE)
  P[!is.finite(r) | n < 3L] <- NA_real_
  diag(P) <- NA_real_
  list(r = r, P = P, n = n)
}

# Use a real one-line Plotly title rather than multiline facet-strip text.
# Fit long labels to the widget width, including after a window resize.
correlation_plotly <- function(plot, height = 300) {
  widget <- plotly::ggplotly(plot, height = height)
  widget <- plotly::layout(widget,
    title = list(text = plot$labels$title, x = 0.5, xanchor = "center", xref = "container",
                 font = list(size = 12)),
    margin = list(t = 45))
  htmlwidgets::onRender(widget, "function(el, x) {
    function fitTitle() {
      var title = el.querySelector('.gtitle');
      if (!title) return;
      title.style.fontSize = '12px';
      var width = title.getBBox().width;
      var available = Math.max(el.clientWidth - 24, 1);
      if (width > available) title.style.fontSize = (12 * available / width) + 'px';
    }
    fitTitle();
    el.on('plotly_afterplot', fitTitle);
  }")
}
