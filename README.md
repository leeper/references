# My BibTeX database


```
## Last Updated: 2017-05-21 21:55:12
```

License: Public Domain (CC-0)

This is the bibtex (.bib) file containing all of my bibliographic references. Figured I'd share it publicly.



Here are some basic statistics on its contents:


```r
library("bib2df")
library("ggplot2")
library("igraph")
```

```
## Loading required package: methods
```

```
## 
## Attaching package: 'igraph'
```

```
## The following objects are masked from 'package:stats':
## 
##     decompose, spectrum
```

```
## The following object is masked from 'package:base':
## 
##     union
```

```r
theme_set(theme_minimal())
dat <- suppressWarnings(bib2df("references.bib"))
```

## Citation Types


```r
dat$CATEGORY <- factor(dat$CATEGORY, levels = names(sort(table(dat$CATEGORY))))
ggplot(dat[!is.na(dat$CATEGORY),], aes(x = CATEGORY)) + geom_bar() + 
  xlab("Count") + ylab("Citation Type") + coord_flip()
```

![plot of chunk bibtype](http://i.imgur.com/4O0vo6B.png)

## Journals


```r
dat$JOURNAL[is.na(dat$JOURNAL)] <- dat$JOURNALTITLE[is.na(dat$JOURNAL)]
topjournals <- aggregate(CATEGORY ~ JOURNAL, data = dat, FUN = length)
topjournals <- head(topjournals[order(topjournals$CATEGORY, decreasing = TRUE), ], 50)
topjournals$JOURNAL <- factor(topjournals$JOURNAL, levels = rev(topjournals$JOURNAL))
ggplot(topjournals, aes(x = JOURNAL, y = CATEGORY)) + geom_bar(stat = "identity") + 
  ylab("Count") + xlab("Journal") + coord_flip()
```

![plot of chunk journal](http://i.imgur.com/5X9SO83.png)

## Authors


```r
aut <- unlist(dat$AUTHOR)
topaut <- as.data.frame(head(sort(table(aut), decreasing = TRUE), 50))
topaut$aut <- factor(topaut$aut, levels = rev(topaut$aut))
ggplot(topaut, aes(x = aut, y = Freq)) + geom_bar(stat = "identity") + 
  ylab("Count") + xlab("Author Name") + coord_flip()
```

![plot of chunk authors](http://i.imgur.com/3nAofZg.png)

## Coauthorship


```r
dat$nauthors <- lengths(dat$AUTHOR)
ggplot(dat[dat$YEAR > 1900, ], aes(x = YEAR, y = nauthors)) + geom_point() + 
  geom_smooth() + xlab("Publication Year") + ylab("Count")
```

```
## `geom_smooth()` using method = 'gam'
```

```
## Warning: Removed 118 rows containing non-finite values (stat_smooth).
```

```
## Warning: Removed 118 rows containing missing values (geom_point).
```

![plot of chunk nauthors](http://i.imgur.com/1STkC4Y.png)


```r
# get all coauthor pairs
colist <- lapply(dat$AUTHOR, function(x) if (length(x) >= 2) combn(x, m = 2) else NA_character_)
# convert networks of top coauthors to igraph object
codat <- na.omit(data.frame(t(do.call("cbind", colist))))
codat$N <- 1L
topco <- aggregate(N ~ X1 + X2, data = codat[codat$X1 %in% topaut$aut | codat$X1 %in% topaut$aut, ], FUN = sum)
cograph <- igraph::graph_from_data_frame(topco, directed = FALSE)
```


```r
between <- betweenness(cograph)
topcoaut <- na.omit(data.frame(betweenness = head(sort(between, decreasing = TRUE), 30)))
topcoaut$aut <- factor(rownames(topcoaut), levels = rev(rownames(topcoaut)))
ggplot(topcoaut, aes(x = aut, y = betweenness)) + geom_bar(stat = "identity") + 
  ylab("Network Betweenness") + xlab("Author Name") + coord_flip()
```

![plot of chunk between](http://i.imgur.com/MW6vsin.png)

## Publication Years


```r
ggplot(dat[dat$YEAR > 1900, ], aes(x = YEAR)) + geom_bar() +
  xlab("Publication Year") + ylab("Count")
```

```
## Warning: Removed 118 rows containing non-finite values (stat_count).
```

![plot of chunk year](http://i.imgur.com/ijUG6nd.png)


