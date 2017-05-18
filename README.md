# My BibTeX database


```
## Last Updated: 2017-05-18 09:18:32
```

License: Public Domain (CC-0)

This is the bibtex (.bib) file containing all of my bibliographic references. Figured I'd share it publicly for no reason.



Here are some basic statistics on its contents:


```r
library("bib2df")
library("ggplot2")
dat <- suppressWarnings(bib2df("references.bib"))
```

## Citation Types


```r
dat$CATEGORY <- factor(dat$CATEGORY, levels = names(sort(table(dat$CATEGORY))))
ggplot(dat[!is.na(dat$CATEGORY),], aes(x = CATEGORY)) + geom_bar() + 
  xlab("Count") + ylab("Citation Type") + coord_flip()
```

![plot of chunk bibtype](http://i.imgur.com/hDd3hdt.png)

## Journals


```r
dat$JOURNAL[is.na(dat$JOURNAL)] <- dat$JOURNALTITLE[is.na(dat$JOURNAL)]
datj <- aggregate(CATEGORY ~ JOURNAL, data = dat, FUN = length)
datj <- head(datj[order(datj$CATEGORY, decreasing = TRUE), ], 50)
datj$JOURNAL <- factor(datj$JOURNAL, levels = rev(datj$JOURNAL))
ggplot(datj, aes(x = JOURNAL, y = CATEGORY)) + geom_bar(stat = "identity") + 
  ylab("Count") + xlab("Journal") + coord_flip()
```

![plot of chunk journal](http://i.imgur.com/oyTVXOq.png)

## Authors


```r
aut <- unlist(dat$AUTHOR)
topaut <- as.data.frame(head(sort(table(aut), decreasing = TRUE), 50))
topaut$aut <- factor(topaut$aut, levels = rev(topaut$aut))
ggplot(topaut, aes(x = aut, y = Freq)) + geom_bar(stat = "identity") + 
  ylab("Count") + xlab("Author Surname") + coord_flip()
```

![plot of chunk authors](http://i.imgur.com/DOjOflp.png)

## Publication Years


```r
ggplot(dat[dat$YEAR > 1900, ], aes(x = YEAR)) + geom_bar() +
  xlab("Publication Year") + ylab("Count")
```

```
## Warning: Removed 127 rows containing non-finite values (stat_count).
```

![plot of chunk year](http://i.imgur.com/Zub2XCo.png)


