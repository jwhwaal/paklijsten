x <- c(1,2,3,6,7,8,11, 11, 12, 16, 17)
y <- c(1,3,11,16)
union(x, y)
intersect(x, y)
length(setdiff(x, y))
setdiff(y, x)

x
y




setequal(x, y)
length(unique(x))
length(unique(y))


## True for all possible x & y :
setequal( union(x, y),
          c(setdiff(x, y), intersect(x, y), setdiff(y, x)))

is.element(x, y) # length 10
is.element(y, x) # length  8
