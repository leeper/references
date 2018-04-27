License: Public Domain (CC-0)

This is the bibtex (.bib) file containing all of my bibliographic references. Figured I'd share it publicly. It was last updated on `R Sys.Date()`.

``` r
library("ggplot2")
requireNamespace("bib2df", quietly = TRUE)
requireNamespace("igraph", quietly = TRUE)
requireNamespace("gender", quietly = TRUE)
requireNamespace("ggraph", quietly = TRUE)
theme_set(theme_minimal())
```

``` r
dat <- suppressWarnings(bib2df::bib2df("references.bib"))
```

    ## Column `YEAR` contains character strings.
    ##               No coercion to numeric applied.

``` r
suppressWarnings(dat[["YEAR"]] <- as.numeric(dat[["YEAR"]]))
```

The database contains 4173 references. What follows are some basic statistics on its contents.

Citation Types
--------------

Reference types in the database:

``` r
dat$CATEGORY <- factor(dat$CATEGORY, levels = names(sort(table(dat$CATEGORY))))
ggplot(dat[!is.na(dat$CATEGORY),], aes(x = CATEGORY)) + 
  geom_bar() + 
  xlab("Count") + 
  ylab("Citation Type") + 
  coord_flip()
```

![](https://i.imgur.com/OBs1u2E.png)

Journals
--------

Most common 50 journals:

``` r
dat$JOURNAL[is.na(dat$JOURNAL)] <- dat$JOURNALTITLE[is.na(dat$JOURNAL)]
topjournals <- aggregate(CATEGORY ~ JOURNAL, data = dat, FUN = length)
topjournals <- head(topjournals[order(topjournals$CATEGORY, decreasing = TRUE), ], 50)
topjournals$JOURNAL <- factor(topjournals$JOURNAL, levels = rev(topjournals$JOURNAL))
ggplot(topjournals, aes(x = JOURNAL, y = CATEGORY)) + 
  geom_bar(stat = "identity") + 
  ylab("Count") + 
  xlab("Journal") + 
  coord_flip()
```

![](https://i.imgur.com/lWPKJ7r.png)

Book Publishers
---------------

Most common 25 journals:

``` r
toppublishers <- aggregate(CATEGORY ~ PUBLISHER, data = dat[dat$CATEGORY == "BOOK",], FUN = length)
toppublishers <- head(toppublishers[order(toppublishers$CATEGORY, decreasing = TRUE), ], 25)
toppublishers$PUBLISHER <- factor(toppublishers$PUBLISHER, levels = rev(toppublishers$PUBLISHER))
ggplot(toppublishers, aes(x = PUBLISHER, y = CATEGORY)) + 
  geom_bar(stat = "identity") + 
  ylab("Count") + 
  xlab("Publisher") + 
  coord_flip()
```

![](https://i.imgur.com/K1NruPl.png)

Authors
-------

Most common 50 authors:

``` r
aut <- unlist(dat$AUTHOR)
topaut <- as.data.frame(head(sort(table(aut), decreasing = TRUE), 100))
topaut$aut <- factor(topaut$aut, levels = rev(topaut$aut))
ggplot(topaut[1:50, ], aes(x = aut, y = Freq)) + 
  geom_bar(stat = "identity") + 
  ylab("Count") + 
  xlab("Author Name") + 
  coord_flip()
```

![](https://i.imgur.com/lKBKmuv.png)

Number of coauthors per publication:

``` r
dat$nauthors <- lengths(dat$AUTHOR)
ggplot(dat[!is.na(dat$YEAR) & dat$YEAR > 1900, ], aes(x = YEAR, y = nauthors)) + 
  geom_point() + 
  geom_smooth(method = "gam") + 
  xlab("Publication Year") + 
  ylab("Coauthors per Publication")
```

![](https://i.imgur.com/AwRQA7W.png)

Coauthorship
------------

Coauthorship network among most common 100 authors:

``` r
# get all coauthor pairs
colist <- lapply(dat$AUTHOR, function(x) if (length(x) >= 2) combn(x, m = 2) else NA_character_)
# convert networks of top coauthors to igraph object
codat <- na.omit(data.frame(t(do.call("cbind", colist))))
codat$N <- 1L
# make coauthor graph from top coauthors
topco <- aggregate(N ~ X1 + X2, data = codat[codat$X1 %in% topaut$aut & codat$X2 %in% topaut$aut, ], FUN = sum)
cograph <- igraph::graph_from_data_frame(topco, directed = FALSE)
ggraph::ggraph(cograph, "igraph", algorithm = "nicely") + 
  ggraph::geom_edge_link(aes(edge_width = log(N)), colour = "gray") + 
  ggraph::geom_node_text(aes(label = name), fontface = 1, size = 2) + 
  theme_void()
```

![](https://i.imgur.com/EMrioRa.png)

Betweenness centrality of top 30 authors:

``` r
between <- igraph::betweenness(cograph)
topcoaut <- na.omit(data.frame(betweenness = head(sort(between, decreasing = TRUE), 30)))
topcoaut$aut <- factor(rownames(topcoaut), levels = rev(rownames(topcoaut)))
ggplot(topcoaut, aes(x = aut, y = betweenness)) + 
  geom_bar(stat = "identity") + 
  ylab("Network Betweenness") + 
  xlab("Author Name") + 
  coord_flip()
```

![](https://i.imgur.com/KkxrD99.png)

Publication Years
-----------------

Years of publication (post-1950):

``` r
ggplot(dat[!is.na(dat$YEAR) & dat$YEAR > 1950, ], aes(x = YEAR)) + 
  geom_bar() +
  xlab("Publication Year") + 
  ylab("Count")
```

![](https://i.imgur.com/y3TTnos.png)

Data missingness
----------------

Proportion missing data, by field, for articles:

``` r
articles <- dat[dat$CATEGORY == "ARTICLE", c("AUTHOR", "TITLE", "JOURNAL", "YEAR", "VOLUME", "NUMBER", "PAGES", "ABSTRACT", "DOI")]
articles <- cbind.data.frame(FIELD = names(articles), MISSINGNESS = unlist(lapply(articles, function(x) sum(is.na(x) == TRUE)/length(x))))
ggplot(articles, aes(x = FIELD, y = MISSINGNESS)) +
  geom_bar(stat = "identity") + 
  ylab("Proportion Missing") + 
  ylim(c(0,1)) +
  xlab("") + 
  coord_flip()
```

![](https://i.imgur.com/wLfEtoW.png)

Proportion missing data, by field, for books:

``` r
books <- dat[dat$CATEGORY == "BOOK", c("AUTHOR", "EDITOR", "TITLE", "PUBLISHER", "YEAR", "ADDRESS", "ISBN")]
books <- cbind.data.frame(FIELD = names(books), MISSINGNESS = unlist(lapply(books, function(x) sum(is.na(x) == TRUE)/length(x))))
ggplot(books, aes(x = FIELD, y = MISSINGNESS)) +
  geom_bar(stat = "identity") + 
  ylab("Proportion Missing") + 
  ylim(c(0,1)) +
  xlab("") + 
  coord_flip()
```

![](https://i.imgur.com/bXjxTmk.png)
