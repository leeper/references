License: Public Domain (CC-0)

This is the bibtex (.bib) file containing all of my bibliographic references. Figured I'd share it publicly.

Here are some basic statistics on its contents:

``` r
requireNamespace("bib2df")
```

    ## Loading required namespace: bib2df

``` r
requireNamespace("igraph")
```

    ## Loading required namespace: igraph

``` r
library("ggplot2")
library("ggraph")
theme_set(theme_minimal())
dat <- suppressWarnings(bib2df::bib2df("references.bib"))
```

Citation Types
--------------

``` r
dat$CATEGORY <- factor(dat$CATEGORY, levels = names(sort(table(dat$CATEGORY))))
ggplot(dat[!is.na(dat$CATEGORY),], aes(x = CATEGORY)) + geom_bar() + 
  xlab("Count") + ylab("Citation Type") + coord_flip()
```

![](http://i.imgur.com/CSyFG8u.png)

Journals
--------

``` r
dat$JOURNAL[is.na(dat$JOURNAL)] <- dat$JOURNALTITLE[is.na(dat$JOURNAL)]
topjournals <- aggregate(CATEGORY ~ JOURNAL, data = dat, FUN = length)
topjournals <- head(topjournals[order(topjournals$CATEGORY, decreasing = TRUE), ], 50)
topjournals$JOURNAL <- factor(topjournals$JOURNAL, levels = rev(topjournals$JOURNAL))
ggplot(topjournals, aes(x = JOURNAL, y = CATEGORY)) + geom_bar(stat = "identity") + 
  ylab("Count") + xlab("Journal") + coord_flip()
```

![](http://i.imgur.com/LF6iktI.png)

Authors
-------

``` r
aut <- unlist(dat$AUTHOR)
topaut <- as.data.frame(head(sort(table(aut), decreasing = TRUE), 100))
topaut$aut <- factor(topaut$aut, levels = rev(topaut$aut))
ggplot(topaut[1:50, ], aes(x = aut, y = Freq)) + geom_bar(stat = "identity") + 
  ylab("Count") + xlab("Author Name") + coord_flip()
```

![](http://i.imgur.com/8UmsXL9.png)

Coauthorship
------------

``` r
dat$nauthors <- lengths(dat$AUTHOR)
ggplot(dat[dat$YEAR > 1900, ], aes(x = YEAR, y = nauthors)) + geom_point() + 
  geom_smooth() + xlab("Publication Year") + ylab("Count")
```

    ## `geom_smooth()` using method = 'gam'

    ## Warning: Removed 118 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 118 rows containing missing values (geom_point).

![](http://i.imgur.com/ky4W7gx.png)

``` r
# get all coauthor pairs
colist <- lapply(dat$AUTHOR, function(x) if (length(x) >= 2) combn(x, m = 2) else NA_character_)
# convert networks of top coauthors to igraph object
codat <- na.omit(data.frame(t(do.call("cbind", colist))))
codat$N <- 1L
# make coauthor graph from top coauthors
topco <- aggregate(N ~ X1 + X2, data = codat[codat$X1 %in% topaut$aut & codat$X2 %in% topaut$aut, ], FUN = sum)
cograph <- igraph::graph_from_data_frame(topco, directed = FALSE)
ggraph(cograph, "igraph", algorithm = "nicely") + 
  geom_edge_link(aes(edge_width = N), colour = "gray") + 
  geom_node_text(aes(label = name), fontface = 2, size = 3) + theme_void()
```

![](http://i.imgur.com/OmLvKU6.png)

``` r
between <- igraph::betweenness(cograph)
topcoaut <- na.omit(data.frame(betweenness = head(sort(between, decreasing = TRUE), 30)))
topcoaut$aut <- factor(rownames(topcoaut), levels = rev(rownames(topcoaut)))
ggplot(topcoaut, aes(x = aut, y = betweenness)) + geom_bar(stat = "identity") + 
  ylab("Network Betweenness") + xlab("Author Name") + coord_flip()
```

![](http://i.imgur.com/GFmTUd9.png)

Publication Years
-----------------

``` r
ggplot(dat[dat$YEAR > 1900, ], aes(x = YEAR)) + geom_bar() +
  xlab("Publication Year") + ylab("Count")
```

    ## Warning: Removed 118 rows containing non-finite values (stat_count).

![](http://i.imgur.com/OONSd9q.png)
