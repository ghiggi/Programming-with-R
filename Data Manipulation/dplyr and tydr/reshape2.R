#################
## plyr join() ##
#################
join    	 #  Join two data frames together.
join_all   #  Recursively join a list of data frames.
match_df

########### 
# dyplr ##
##########
# combine  	Efficiently bind multiple data frames by row and column
# bind     	Efficiently bind multiple data frames by row and column
# bind_cols	Efficiently bind multiple data frames by row and column
# bind_rows	Efficiently bind multiple data frames by row and column
# rbind_all	Efficiently bind multiple data frames by row and column
# rbind_list	Efficiently bind multiple data frames by row and column


#############
## reshape2 #
#############
dcast
acast 
melt 
recast
colsplit
add_marging 

# Convert data.frame to array ...#
data<-data.frame(coord.name=rep(1:10, 2),
                 x=rnorm(20),
                 y=rnorm(20),
                 ID=rep(c("A","B"), each=10))

data.array<-array(dim=c(10, 2, length(unique(data$ID))))
for(i in 1:length(unique(data$ID))){
  data.array[,1,i]<-data[data$ID==unique(data$ID)[i],"x"]
  data.array[,2,i]<-data[data$ID==unique(data$ID)[i],"y"]
}

# Use this or some other method to add a column of row indices.
data$row <- with(data, ave(ID==ID, ID, FUN = cumsum))

m <- melt(data, id.vars = c("row", "ID"))
a <- acast(m, row ~ variable ~ ID)

###########
## aperm ##
###########
aperm 
abind 
