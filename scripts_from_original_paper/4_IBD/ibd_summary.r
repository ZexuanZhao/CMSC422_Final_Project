args = commandArgs(trailingOnly=TRUE)

#table is 5 columns: genus species gene n p-value
results<-read.table(args[1], sep="\t")


#removes NAs
results<-na.omit(results)

p.value<-results$V4
x <- sum(p.value < 0.05)
x
n <- length(p.value)
n
o.proportion <- x/n
o.proportion
e.proportion <- n*0.05/n
e.proportion

binomial <- binom.test(x, n, 0.05, alternative = "g")
binomial
significance <- binomial$p.value
significance

l <- lm(V4 ~ V3, data = results)
l.p <- anova(l)$'Pr(>F)'[1]
l.r <- summary(l)$r.squared

group <- args[2]
file <- args[3]

df <- data.frame(group, n, x, o.proportion, significance, l.r, l.p)
df
###identify species with significant results and how many loci

write.table(df, file, append=TRUE, sep = "\t", row.names=FALSE, col.names=FALSE, quote=FALSE)