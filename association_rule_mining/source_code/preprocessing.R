library(dplyr)
library(plyr)


movie_df=read.csv("imdb_movie.csv", header=FALSE, sep=";", strip.white = TRUE)
actor_df=read.csv("imdb_actor.csv", header=FALSE, sep=";", strip.white = TRUE)


actor_df[] <- lapply(actor_df, type.convert)
actor_df[] <- lapply(actor_df, gsub, pattern="\\?", replacement='')

movie_df$V2 <- paste(movie_df$V3, movie_df$V2, sep=",")
movie_df$V3 <- NULL


imdb_df = bind_rows(movie_df, actor_df)


processed_df <- ddply(imdb_df,.(V1), function(df1)paste(df1$V2, collapse = ","))

write.csv(processed_df, "processed_itemlist.csv", quote = FALSE,row.names = FALSE, col.names = FALSE)





