# My BibTeX database


```
## Last Updated: 2017-05-18 08:56:50
```

License: Public Domain (CC-0)

This is the bibtex (.bib) file containing all of my bibliographic references. Figured I'd share it publicly for no reason.



Here are some basic statistics on its contents:


```r
library("bib2df")
library("ggplot2")
dat <- suppressWarnings(bib2df("references.bib"))
dat$JOURNAL[is.na(dat$JOURNAL)] <- dat$JOURNALTITLE[is.na(dat$JOURNAL)]
```

## Citation Types


```r
dat$CATEGORY <- factor(dat$CATEGORY, levels = names(sort(table(dat$CATEGORY))))
ggplot(dat[!is.na(dat$CATEGORY),], aes(x = CATEGORY)) + geom_bar() + 
  xlab("Count") + ylab("Citation Type") + coord_flip()
```

![plot of chunk bibtype](http://i.imgur.com/qYdiWZb.png)

## Journals


```r
datj <- aggregate(CATEGORY ~ JOURNAL, data = dat, FUN = length)
datj <- head(datj[order(datj$CATEGORY, decreasing = TRUE), ], 30)
datj$JOURNAL <- factor(datj$JOURNAL, levels = rev(datj$JOURNAL))
ggplot(datj, aes(x = JOURNAL, y = CATEGORY)) + geom_bar(stat = "identity") + 
  ylab("Count") + xlab("Journal") + coord_flip()
```

![plot of chunk journal](http://i.imgur.com/FQZED9P.png)

## Authors


```r
aut <- unlist(dat$AUTHOR)
aut <- as.data.frame(head(sort(table(aut), decreasing = TRUE), 50))
aut$aut <- factor(aut$aut, levels = rev(aut$aut))
ggplot(aut, aes(x = aut, y = Freq)) + geom_bar(stat = "identity") + 
  ylab("Count") + xlab("Author Surname") + coord_flip()
```

![plot of chunk authors](http://i.imgur.com/xPgxjs1.png)

## Publication Years


```r
ggplot(dat[dat$YEAR > 1900, ], aes(x = YEAR)) + geom_bar() +
  xlab("Publication Year") + ylab("Count")
```

```
## Warning: Removed 127 rows containing non-finite values (stat_count).
```

![plot of chunk year](http://i.imgur.com/dVOaLB3.png)


