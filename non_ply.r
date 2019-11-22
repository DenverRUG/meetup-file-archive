# This file is a brief exploration of the non-**ply functions in the 
# plyr package.  It was presented (poorly) for a lightning talk, so not all of
# the functions are as thoroughly explored and documented as I'd normally like -
# but this should at least get you started.

# The Incantation
options(stringsAsFactors = FALSE)

# Load plyr
library(plyr)





################################################################################
# Ordering a data.frame
################################################################################

# base way
mtcars.ord <- mtcars[order(mtcars$cyl, mtcars$disp), ]

# plyr way
mtcars.arr <- arrange(mtcars, cyl, disp)

# Key caveat: arrange() drops the rownames.
head(mtcars.ord)
head(mtcars.arr)

mtcars.ord == mtcars.arr
all(mtcars.ord == mtcars.arr)
identical(mtcars.ord, mtcars.arr)


# But nice advantage: saner descending order
mtcars.dro <- mtcars[order(mtcars$cyl, -xtfrm(mtcars$disp)), ]
mtcars.rra <- arrange(mtcars, cyl, desc(disp))

all(mtcars.dro == mtcars.rra)

# Let's take a closer look at desc()
data.frame(orig = 0:9, down = desc(0:9))
data.frame(orig = letters, down = desc(letters))


################################################################################
# Operating on columns
################################################################################

# base way
apply(mtcars, 2, mean)

# plyr way
cmean <- colwise(mean)
cmean(mtcars)

apply(mtcars, 2, mean) == cmean(mtcars)

# Not much difference in the basic application - but colwise takes a second
# argument that lets you specify criteria for the columns that should be
# included.  Let's add some character vectors to mtcars and see what happens
mpcars <- mtcars
mpcars$foo <- sample(LETTERS, size = nrow(mpcars), replace = TRUE)
mpcars$bar <- sample(letters, size = nrow(mpcars), replace = TRUE)
str(mpcars)

apply(mpcars, 2, mean)

cmean.num <- colwise(mean, .cols = is.numeric)
cmean.num(mpcars)

cmean.num(mpcars) == cmean(mtcars)

# There are actually two plyr functions for the common numeric- and
# discrete-column cases:
cmean.num2 <- numcolwise(mean)
cmean.num(mpcars) == cmean.num2(mpcars)

maxchar <- catcolwise(max)
maxchar(mpcars)


################################################################################
# Counting unique combinations
################################################################################

# ddply() ways
# The way I used to do it
ddply(mpcars, .var = .(cyl, gear), nrow)
# The way I do it now
ddply(mpcars, .var = .(cyl, gear), summarize, freq = length(mpg))

# count() way
count(mpcars, vars = .(cyl, gear))


# If the records are weighted - let's say by horsepower
ddply(mpcars, .var = .(cyl, gear), summarize, freq = sum(hp))
count(mpcars, vars = .(cyl, gear), wt_var = .(hp))

# Other notes I can't get into here:
# Competitive with table() for speed
# Only counts combinations that occur in the data 
#     (unlike ddply(..., .drop = FALSE)
# Preserves variable type of identifiers (i.e., cyl and gear)
# Need to test speed vs. ddply+summarize




################################################################################
# Compute new variables for a data.frame
################################################################################

# base way
transform(mtcars, 
          disp_per_cyl = disp / cyl, 
          hp_per_cyl = hp / cyl)

# plyr way
mutate(mtcars, 
       disp_per_cyl = disp / cyl, 
       hp_per_cyl = hp / cyl)

# But what about computations that depend on computations from the same
# transform/mutate?
transform(mtcars, 
          hp_per_cyl = hp / cyl, 
          hp_per_cyl_per_1klb = hp_per_cyl / wt)

mutate(mtcars, 
       hp_per_cyl = hp / cyl, 
       hp_per_cyl_per_1klb = hp_per_cyl / wt)



################################################################################
# Rename stuff
################################################################################

# base way
names(mpcars)[names(mpcars) %in% "disp"] <- "displ"
names(mpcars)[names(mpcars) %in% "vs"] <- "vroomvroom"
head(mpcars)

# plyr way (restoring names this time)
mpcars <- rename(mpcars, c("displ" = "disp", "vroomvroom" = "vs"))
head(mpcars)
