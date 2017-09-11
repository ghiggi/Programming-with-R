Window functions take nn inputs and give back nn outputs. Furthermore, the output depends on all the values


# min_rank	Windowed rank functions.
# percent_rank	Windowed rank functions.
# dense_rank	Windowed rank functions.
# ntile	Windowed rank functions.
# ranking	Windowed rank functions.
# row_number	Windowed rank functions.
filter(min_rank(desc(lifeExp)) < 2 | min_rank(lifeExp) < 2)  # to obtain extreme values 
y <- c(1, 2, 2, NA, 3, 4)
min_rank(y)
#> [1]  1  2  2 NA  4  5
min_rank(desc(y))



# cumall	Cumulativate versions of any, all, and mean
# cumany	Cumulativate versions of any, all, and mean
# cume_dist	Windowed rank functions.
# cummean	Cumulativate versions of any, all, and mean