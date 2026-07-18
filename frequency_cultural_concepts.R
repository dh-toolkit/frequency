# =============================================================================
# Frequency of cultural concepts — Project #2.2
# Base R only (no package install required).
#
# Run:
#   & "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" r/frequency_cultural_concepts.R
# =============================================================================

root <- if (file.exists("possession_annotations_from_tei.csv")) {
  "."
} else if (file.exists("../possession_annotations_from_tei.csv")) {
  ".."
} else {
  stop("Run from project root or r/; CSV not found.")
}

csv_path <- file.path(root, "possession_annotations_from_tei.csv")
out_dir  <- file.path(root, "r", "output")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

matrix_levels <- c("time", "values", "customs", "beliefs", "postcolonialism")

raw <- read.csv(csv_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

trim_lower <- function(x) {
  x[is.na(x)] <- ""
  tolower(trimws(x))
}

raw$category_primary   <- trim_lower(raw$category_primary)
raw$category_secondary <- trim_lower(raw$category_secondary)
raw$cultural_markers[is.na(raw$cultural_markers)] <- ""

raw$chapter_label <- ifelse(
  !is.na(raw$div_type) & raw$div_type == "front",
  "front",
  ifelse(!is.na(raw$chapter) & raw$chapter != "",
         paste0("ch", raw$chapter),
         "unknown")
)

raw$annotated <- raw$category_primary %in% matrix_levels

n_passages  <- nrow(raw)
n_annotated <- sum(raw$annotated)

cat("\n=== Corpus size ===\n")
cat(sprintf("Passages total:     %d\n", n_passages))
cat(sprintf("Passages annotated: %d (%.1f%%)\n\n",
            n_annotated, 100 * n_annotated / n_passages))

# --- 1) Primary category frequency ------------------------------------------
ann <- raw[raw$annotated, , drop = FALSE]
tab <- table(factor(ann$category_primary, levels = matrix_levels))
freq_primary <- data.frame(
  category_primary   = names(tab),
  abs_freq           = as.integer(tab),
  rel_freq_annotated = as.numeric(tab) / n_annotated,
  rel_freq_corpus    = as.numeric(tab) / n_passages,
  stringsAsFactors   = FALSE
)
freq_primary$pct_annotated <- round(100 * freq_primary$rel_freq_annotated, 1)
freq_primary$pct_corpus    <- round(100 * freq_primary$rel_freq_corpus, 1)
freq_primary <- freq_primary[order(-freq_primary$abs_freq), ]

cat("=== 1. Primary category frequency ===\n")
print(freq_primary, row.names = FALSE)
write.csv(freq_primary, file.path(out_dir, "freq_primary_categories.csv"), row.names = FALSE)

# --- 2) Marker / concept frequency ------------------------------------------
markers <- unlist(strsplit(ann$cultural_markers, "\\s*[;|]\\s*"))
markers <- trimws(markers)
markers <- markers[markers != ""]
if (length(markers) > 0) {
  mtab <- sort(table(markers), decreasing = TRUE)
  freq_markers <- data.frame(
    concept            = names(mtab),
    abs_freq           = as.integer(mtab),
    rel_freq_annotated = as.numeric(mtab) / n_annotated,
    stringsAsFactors   = FALSE
  )
  freq_markers$pct_annotated <- round(100 * freq_markers$rel_freq_annotated, 1)
} else {
  freq_markers <- data.frame(
    concept = character(), abs_freq = integer(),
    rel_freq_annotated = numeric(), pct_annotated = numeric(),
    stringsAsFactors = FALSE
  )
}

cat("\n=== 2. Cultural concept (marker) frequency — top 20 ===\n")
print(head(freq_markers, 20), row.names = FALSE)
write.csv(freq_markers, file.path(out_dir, "freq_cultural_markers.csv"), row.names = FALSE)

# --- 3) Category × chapter --------------------------------------------------
ct <- as.data.frame(table(
  chapter_label = ann$chapter_label,
  category_primary = factor(ann$category_primary, levels = matrix_levels)
), stringsAsFactors = FALSE)
names(ct)[3] <- "abs_freq"
ct <- ct[ct$abs_freq > 0, ]
ct$chapter_total <- ave(ct$abs_freq, ct$chapter_label, FUN = sum)
ct$rel_within_chapter <- ct$abs_freq / ct$chapter_total
ct <- ct[order(ct$chapter_label, -ct$abs_freq), ]

cat("\n=== 3. Category × chapter ===\n")
print(ct, row.names = FALSE)
write.csv(ct, file.path(out_dir, "freq_category_by_chapter.csv"), row.names = FALSE)

# --- 4) Primary + secondary -------------------------------------------------
prim <- table(factor(raw$category_primary[raw$category_primary %in% matrix_levels],
                    levels = matrix_levels))
sec  <- table(factor(raw$category_secondary[raw$category_secondary %in% matrix_levels],
                    levels = matrix_levels))
freq_roles <- data.frame(
  category        = matrix_levels,
  primary         = as.integer(prim[matrix_levels]),
  secondary       = as.integer(sec[matrix_levels]),
  stringsAsFactors = FALSE
)
freq_roles$total_mentions <- freq_roles$primary + freq_roles$secondary
freq_roles <- freq_roles[order(-freq_roles$total_mentions), ]

cat("\n=== 4. Primary + secondary role counts ===\n")
print(freq_roles, row.names = FALSE)
write.csv(freq_roles, file.path(out_dir, "freq_primary_secondary.csv"), row.names = FALSE)

# --- 5) Plots (PNG via base graphics) ---------------------------------------
png(file.path(out_dir, "freq_primary_categories.png"), width = 900, height = 500)
op <- par(mar = c(5, 8, 4, 2))
barplot(
  rev(freq_primary$abs_freq),
  names.arg = rev(freq_primary$category_primary),
  horiz = TRUE, las = 1, col = "#2c5f7c",
  main = sprintf("Primary categories (n_annotated = %d / %d)", n_annotated, n_passages),
  xlab = "Absolute frequency (passages)"
)
par(op)
dev.off()

if (nrow(freq_markers) > 0) {
  top <- head(freq_markers, 15)
  png(file.path(out_dir, "freq_cultural_markers.png"), width = 900, height = 600)
  op <- par(mar = c(5, 14, 4, 2))
  barplot(
    rev(top$abs_freq),
    names.arg = rev(top$concept),
    horiz = TRUE, las = 1, col = "#6b4c3b",
    main = "Most frequent cultural markers",
    xlab = "Absolute frequency"
  )
  par(op)
  dev.off()
}

# stacked chapter plot
ch_levels <- unique(ct$chapter_label)
mat <- matrix(0, nrow = length(matrix_levels), ncol = length(ch_levels),
              dimnames = list(matrix_levels, ch_levels))
for (i in seq_len(nrow(ct))) {
  mat[ct$category_primary[i], ct$chapter_label[i]] <- ct$abs_freq[i]
}
png(file.path(out_dir, "freq_category_by_chapter.png"), width = 900, height = 500)
barplot(
  mat, beside = FALSE, col = c("#8da0cb", "#66c2a5", "#fc8d62", "#e78ac3", "#a6d854"),
  legend.text = TRUE, args.legend = list(x = "topright", bty = "n"),
  main = "Cultural category load by chapter",
  ylab = "Annotated passages"
)
dev.off()

cat(sprintf("\nWrote tables + charts to: %s\n", normalizePath(out_dir)))
cat("Done.\n")
