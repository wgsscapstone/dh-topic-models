**Goal:** Today, we are going to build an interactive website that displays a topic
model for your choosen corpus using R and GitHub pages. 

### Step 1: Fork repository and setup pages

The first step is to click on the "Fork" button in this repository. It will make a
copy under your username that you can control.

Next, click on the Settings tab (in your fork). Scroll to the GitHub Pages section.
Select "master Branch" under **Source**. This will make your repostiory show up 
as a website. It can sometimes take a while (upwards of 30 minutes or more) for 
this first push to work, so that is why we are starting here. There should be a 
link on the settings page once you do this pointing to your new website.

### Step 2: Download R

The next step is to download and install R for your computer (most of you should
already have done this). To do so, following the following link and proceed with
the prompts:

- [https://cran.r-project.org/](https://cran.r-project.org/)

Once downloaded, install the dmg or exe file as you would any software.

### Step 3: Install R packages

Now, we need to install several add-ons to the R language. We can do this from
within R. Open the program. Then, copy and paste the following lines one by one
into the R terminal:

```{r}
install.packages("topicmodels")
install.packages("tidytext")
install.packages("dplyr")
install.packages("jsonlite")
```

It may ask you some questions and I'll help you answer those. Note that this may
take a few minutes as each of these packages requires other packages, none of 
which you have yet.

Notice that I don't have to run these lines on my machine. These only has to be
run once and then they are installed for good.

### Step 4: Download zip files

You should have been given files through your DH GitHub repository with the raw
text and XML files for your choosen journal from JSTOR. Download these and 
unzip the files somewhere on your machine (perhaps the Desktop?). The important
thing is to keep each of the output directories in the same place.

### Step 5: Load Packages and Functions

Next, we need to load the packages you just installed in R. To do so, run the
following lines (you can copy and paste them as a block):

```{r}
library(topicmodels)
library(tidytext)
library(dplyr)
library(jsonlite)
```

Next, we need to define two functions that we will use the build topic models
and display them online. These look scary, but we are just copy and pasting 
them into R. First:

```{r}
load_data <- function(directory) {

  # get titles
  xml_in <- dir(file.path(directory, "all_xml"), full.names = TRUE)
  title_data <- data_frame(fin = basename(xml_in), title = "")

  for (i in seq_along(xml_in)) {
    x <- readLines(xml_in[i], warn = FALSE)
    title_data$title[i] <- x[grep("<article-title>", x)[1]]
  }

  title_data$title <- gsub("[ ]*<[^>]+>", "", title_data$title)
  title_data$title <- gsub("", "", title_data$title)

  # get text files
  fin <- dir(file.path(directory, "all_topic_model"), full.names = TRUE)
  data <- data_frame(fin = basename(fin), text = "")

  for (i in seq_along(fin)) {
    data$text[i] <- tolower(paste(readLines(fin[i], warn = FALSE), collapse = ""))
  }

  out <- strsplit(data$text, split = " ")
  names(out) <- data$fin

  df <- data_frame(id = unlist(mapply(function(u, v) rep(u, length(v)), data$fin, out)),
                   word = unlist(out))

  word_count_total <- count(df, word, sort = TRUE)[1:5000,]
  word_counts <- count(semi_join(df, word_count_total, by = "word"),
                       id, word, sort = TRUE)

  word_counts <- anti_join(word_counts, stop_words, by = "word")
  swords_vec <- c(letters, LETTERS, 0:1000, "cf", "op", "cit", "pp", "de", as.roman(1:100), tolower(as.roman(1:100)))
  word_counts <- word_counts[is.na(match(word_counts$word, swords_vec)),]

  text_dtm <- cast_dtm(word_counts, id, word, n)

  # join titles to the text_dtm dataset
  docs <- sapply(strsplit(text_dtm$dimnames$Docs, split = "-"), getElement, 1)
  title_data$doc_id <- sapply(strsplit(title_data$fin, split = "\\."), getElement, 1)
  id <- match(docs, title_data$doc_id)
  title <- rep("", length(id))
  title[!is.na(id)] <- title_data$title[id[!is.na(id)]]
  text_dtm$title <- title

  return(text_dtm)
}
```

And then this one:

```{r}
build_webpage <- function(name, obj, titles) {

  # grab docs, words, and vocabulary
  docs <- obj@gamma
  words <- obj@beta
  vocab <- obj@terms
  links <- obj@documents

  if (missing(titles)) titles <- links

  dir.create("data", FALSE, FALSE)

  # meta.csv
  meta <- cbind(links, titles)
  write.table(meta, file.path("data", "meta.csv"), sep=",",
      quote=TRUE, col.names=FALSE, row.names=FALSE)

  # topic_scaled.csv
  temp <- scale(t(scale(words)))
  dists <- acos(crossprod(temp) / nrow(temp)) / pi
  diag(dists) <- 0
  topic_scaled <- cmdscale(dists)
  write.table(topic_scaled, file.path("data", "topic_scaled.csv"),
      sep=",", quote=FALSE, col.names=FALSE, row.names=FALSE)

  # info.json
  info <- structure(list(title = "", meta_info = "", VIS = structure(list(
            bib_view = structure(list(major = "issue", minor = "journal",
                dir = "up"), .Names = c("major", "minor", "dir")), prefab_plots = FALSE,
            model_view = structure(list(w = 500L, aspect = 2L, words = 8L,
                size_range = c(6L, 14L), yearly = structure(list(w = 500L,
                    label_threshold = 30L), .Names = c("w", "label_threshold"
                )), name_size = 13L), .Names = c("w", "aspect", "words",
            "size_range", "yearly", "name_size")), word_view = structure(list(
                row_height = 100L, w = 700L, topic_label_leading = 16L,
                m = structure(list(left = 150L, right = 50L, top = 50L,
                    bottom = 0L), .Names = c("left", "right", "top",
                "bottom"))), .Names = c("row_height", "w", "topic_label_leading",
            "m")), bib = structure(list(author_delimiter = ", "), .Names = "author_delimiter"),
            special_issue_class = "special_issue_nohighlight", hidden_topics = c(100, 101), show_hidden_topics = FALSE), .Names = c("bib_view",
        "prefab_plots", "model_view", "word_view", "bib", "special_issue_class",
        "hidden_topics", "show_hidden_topics")), issues = list(), names = structure(list(
            `1` = "Topic 001", `2` = "Topic 002", `3` = "Topic 003",
            `4` = "Topic 004", `5` = "Topic 005", `6` = "Topic 006",
            `7` = "Topic 007", `8` = "Topic 008", `9` = "Topic 009",
            `10` = "Topic 010", `11` = "Topic 011", `12` = "Topic 012",
            `13` = "Topic 013", `14` = "Topic 014", `15` = "Topic 015",
            `16` = "Topic 016", `17` = "Topic 017", `18` = "Topic 018",
            `19` = "Topic 019", `20` = "Topic 020", `21` = "Topic 021",
            `22` = "Topic 022", `23` = "Topic 023", `24` = "Topic 024",
            `25` = "Topic 025"), .Names = c("1", "2", "3", "4", "5",
        "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16",
        "17", "18", "19", "20", "21", "22", "23", "24", "25"))), .Names = c("title",
        "meta_info", "VIS", "issues", "names"))
  info$names <- as.list(sprintf("Topic %03d", 1:ncol(docs)))
  names(info$names) <- as.character(1:ncol(docs))
  info <- jsonlite::toJSON(info, auto_unbox=TRUE, pretty=4)
  writeLines(info, file.path("data", "info.json"))

  # dt.json
  dtm <- as(docs, "CsparseMatrix")
  dtm <- toJSON(list(i=dtm@i, p=dtm@p, x=dtm@x), digits=4L)
  writeLines(dtm, file.path("data", "dt.json"))

  # tw.json
  n_top_words <- 50
  alpha <- rep(1, ncol(docs))
  tw <- apply(words, 1, function(v) {
    list(words = vocab[order(v, decreasing=TRUE)[1:n_top_words]],
         weights = sort(v, decreasing=TRUE)[1:n_top_words])
  })
  tw <- list(alpha=alpha, tw=tw)
  tw <- jsonlite::toJSON(tw, dataframe="columns", digits=4L)
  writeLines(tw, file.path("data", "tw.json"))

}
```

Now, we should be ready to do some real work in R!

### Step 6: Running Topic Models in R

We want to tell R where to save the output of our models. We can
do this by running the following command (to save at the Desktop
on a Mac):

```{r}
setwd("~/Desktop")
```

You can use the GUI menus as well if you have trouble with that.
Then, we read in the datasets that you just unzipped. To do this,
run the following line with the path *containing* your unzipped
files (not the files themselves). So, if you put them on the Desktop,
do this:

```{r}
text_dtm <- load_data("~/Desktop/")
```

Now, we'll use that data and run a topic model over it. Here I am
using 15 topics but you can change this later:

```{r}

lda_model <- LDA(text_dtm, k = 25, method = "Gibbs",
                  control = list(verbose = 1, iter = 100))
```

Warning: this may take a few minutes depending on the size of your dataset.

Now, we build the data that supports the website. 

```{r}
build_webpage(lda_model, text_dtm$title)
```

This should create a directory named "data" on your Desktop (or wherever
you choose to create the output).

### Step 7: Push Results to GitHub

Finally, we now commit the new data files you just built to the GitHub fork
you have. Drag and drop the data files into the "data" directory on GitHub.

If your page is published, you should see the new topic models! Note that you
may need to open an incognito mode to refresh the JSON files. If you site is, on
a mac you open a terminal and run the following:

```{sh}
cd ~/Desktop
python2 -m SimpleHTTPServer 8001
```

And then open a browser to the url `http://0.0.0.0:8001/`. 

### Step 8: Tweaks

Hopefully, you were able to get this all to work and run on GitHub. Next, you
should try to tweak the output. An obvious next step is to play with a different 
number of topics. You can also edit the file "info.json" and name the topics
and edit the "index.html" file to name your model.











