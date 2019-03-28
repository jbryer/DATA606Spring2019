#' Calculate the standard error of a proportion.
#' @param p probability of success
#' @param n sample size
se <- function(p, n) {
	sqrt(
		(p * (1-p)) / n
	) * 2
}

se(.5, 10)

df <- data.frame(
	p = seq(.01, 1, .01),
	se = sapply(seq(.01, 1, .01), function(x) { se(p = x, n = 10)})
)
df

plot(df$p, df$se)

se(0.5, 1068) # n for a 3% SE


df2 <- data.frame(
	n = seq(1000, 10000, 100),
	se = sapply(seq(1000, 10000, 100), function(x) { se(p = 0.5, n = x)})
)
head(df2)

plot(df2$n, df2$se)


