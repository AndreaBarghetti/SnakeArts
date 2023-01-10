library(R6)
Space <- R6::R6Class(classname = "Space",
                     public = list(
                       nrows=NA,
                       ncols=NA,
                       size=NA,
                       freespace=NA,
                       occupied=c(),
                       occupied_values=c(),
                       posmap=NA,
                       head=NA,
                       tail=NA,
                       snake_value=0,

                       initialize = function(nrows, ncols) {
                         size = nrows*ncols
                         self$nrows <- nrows
                         self$ncols <- ncols
                         self$size <- size
                         self$freespace <- size
                         self$posmap <- matrix(1:size, nrow = nrows)
                       },

                       occupy = function(pos,value) {
                         if(pos %in% self$occupied) {stop("position already occupied")}
                         self$occupied <- c(self$occupied, pos)
                         self$occupied_values <- c(self$occupied_values, value)
                         self$freespace <- self$freespace -1
                         invisible(self)
                       },

                       get_free_pos = function() {

                         if(length(self$occupied) == self$size) {stop("Space full")}

                         options = setdiff(1:self$size,
                                           self$occupied)
                         if(length(options)==1){options}
                         sample(options,size = 1)
                       },

                       get_adj_pos = function(pos) {
                         get_adj_pos(pos, self)
                       },

                       pick_adj_pos = function(pos) {
                         options = self$get_adj_pos(pos)
                         if (all(options==0)) {stop("no options left")}
                         options <- options[options!=0]
                         if (length(options)==1) {return(options)}
                         sample(options, size = 1)
                       },

                       new_snake = function() {
                         self$snake_value <- self$snake_value+1
                         self$head = self$get_free_pos()
                         self$tail = self$head
                         self$occupy(self$head, self$snake_value)
                         invisible(self)
                       },

                       move_head = function() {
                         newhead <- self$pick_adj_pos(self$head)
                         self$occupy(newhead, self$snake_value)
                         self$head <- newhead
                         invisible(self)
                       },

                       grow_snake = function() {
                         while(any(space$get_adj_pos(space$head)!=0)) {
                           self$move_head()
                         }
                         invisible(self)
                       },

                       fill_space = function() {
                         while(self$freespace > 0) {
                           try(expr = self$new_snake(),silent = T)
                           try(expr = {self$grow_snake()},silent = T)
                         }
                         invisible(self)
                       },

                       show = function() {
                         plot_space(self)
                       }
                     ))

get_adj_pos <- function(pos, space) {
  current <- which(space$posmap==pos,arr.ind = TRUE)
  U =  c(current[1]-1, current[2])
  D =  c(current[1]+1, current[2])
  L =  c(current[1], current[2]-1)
  R =  c(current[1], current[2]+1)

  res <- map_dbl(list(U,D,L,R), function(p) {
    tryCatch(expr = {
      if(p[1]==0 | p[2]==0) {return(0)}
      space$posmap[p[1],p[2]]
    },
    error = function(cond) {return(0)}
    )})
  res[res %in% space$occupied] <- 0
  res
}

plot_space <- function(space) {

  df <- map(space$occupied, function(pos) {
    which(space$posmap==pos,arr.ind = TRUE)
  }) %>% purrr::reduce(rbind) %>%
    as_tibble() %>%
    mutate(value = space$occupied_values) %>%
    group_by(value) %>%
    mutate(length=n())

  head_and_tails <- df %>%
    group_by(value) %>%
    slice(c(1,n()))

  p <- ggplot(df, aes(x=col, y=row, group=value, col=(length))) +
    # ggplot(df, aes(x=col, y=row, col=as.factor(value))) +
    geom_point(data=head_and_tails,show.legend = F, size=3) +
    geom_path(show.legend = F, linewidth=3) +
    geom_path(show.legend = F, linewidth=2, col="black") +
    theme_void() +
    coord_fixed(xlim = c(1,space$ncols), ylim = c(1,space$nrows)) +
    theme(panel.background = element_rect(fill = "black")) +
    geom_polygon(data=tibble(row = c(0,0,space$nrows+1,space$nrows+1),
                          col = c(0,space$ncols+1,space$ncols+1,0),
                          value=0), col="white", linewidth=3, fill="transparent")

  p
}

get_space_stats <- function(space) {
  df <- map(space$occupied, function(pos) {
    which(space$posmap==pos,arr.ind = TRUE)
  }) %>% purrr::reduce(rbind) %>%
    as_tibble() %>%
    mutate(value = space$occupied_values) %>%
    group_by(value) %>%
    mutate(length=n()) %>%
    ungroup()

  snakes <- df %>%
    count(value,name = "length")

  length_count <- table(snakes$length)

  snakes %>%
    summarise(snakes = n(),
              longest = max(length),
              points=length_count[1])
}