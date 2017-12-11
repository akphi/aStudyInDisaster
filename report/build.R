html <- function() {
  bookdown::render_book("index.Rmd", "bookdown::gitbook")
}

# problem with LaTeX not being able to render image of remote address
# e.g. ![sds](http://127.0.0.1:1923/result/data_analyzed/plots/all_features_scatter.png)
# problem with not being able to generate RAW HTML tage inside a document
pdf <- function() {
  bookdown::render_book("index.Rmd", "bookdown::pdf_book")
}