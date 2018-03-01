

bibi.nest <- bibi.sub %>% 
  filter(period == "2004_2008") %>% 
  full_join(huc12.df, by = c("huc_12", "acres", "states", "total_acres")) %>% 
  select(huc_12, rating) %>% 
  group_by(huc_12) %>% 
  nest() %>% 
  mutate(empty = map_lgl(data, function(i) any(is.na(i))),
         length = purrr::map(data, function(i) length(unlist(i))),
         unique = purrr::map(data, function(i) length(unique(unlist(i)))),
        
         sel_rating = purrr::map(data, function(i) {
           if(length(unique(unlist(i))) == 1) {
             unique(unlist(i))
           } else {
             as.character(NA)
           }
         }),
         sel_rating = if_else(unique == 1 & is.na(sel_rating),
                              "insufficient",
                              as.character(sel_rating)))

#------------------------------------------------------------------------------
reps <- 10
boot.sample <- 800
nest.df <- bibi.nest %>% 
  filter(period == "2000_2008")
#------------------------------------------------------------------------------
sample_ratings <- function(nest.df, reps, boot.sample) {
 test1 <-  purrr::map(1:reps, function(i) {
    bibi.sub <- nest.df %>% 
      slice(sample(1:nrow(nest.df), boot.sample, replace = TRUE)) %>% 
      mutate(sel_rating = if_else(is.na(sel_rating),
                                  purrr::map_chr(data, function(i) {
                                    unlist(i)[sample(1:length(unlist(i)), 1)]
                                  }),
                                  sel_rating)) %>% 
      pull(sel_rating)
  }) 
}
#------------------------------------------------------------------------------
pct_ratings <- function(ratings.list, boot.sample) {
  purrr::map(1:length(ratings.list), function(list.i) {
    data.frame(rating = unlist(ratings.list[list.i]), stringsAsFactors = FALSE) %>% 
      mutate(rating = factor(rating, c("acceptable", "degraded", "insufficient"))) %>% 
      group_by(rating) %>% 
      summarize(pct = n() / boot.sample * 100) %>% 
      ungroup() %>% 
      complete(rating, fill =list(pct = 0)) %>% 
      pull(pct)
  })
}
#------------------------------------------------------------------------------
stat_ratings <- function(pct.ratings.list, boot.sample) {
  purrr::map(1:3, function(i) {
    purrr::map(pct.ratings.list, i) %>% 
      unlist() %>% 
      psych::describe()
  }) %>% 
    bind_rows() %>% 
    mutate(rating = c("acceptable", "degraded", "insufficient"),
           rating = factor(rating, c("acceptable", "degraded", "insufficient")),
           bootstrap_sample = boot.sample) %>% 
    select(rating, bootstrap_sample, everything(), -vars)
}

#------------------------------------------------------------------------------
bootstrap_ratings <- function(nest.df, reps, boot.sample) {
 sample_ratings(nest.df, reps, boot.sample) %>% 
    pct_ratings(boot.sample) %>% 
    stat_ratings(boot.sample)
}

test4 <- purrr::map(c(10, 50, 100, 500, 1000, 1500, 2000), function(i) {
  bootstrap_ratings(bibi.nest, 1000, i)
}) %>% 
  bind_rows() %>% 
  mutate(bootstrap_sample = factor(bootstrap_sample, levels = unique(bootstrap_sample)))

ggplot(test4, aes(bootstrap_sample, mean, fill = rating)) +
  geom_bar( position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("acceptable" = "#56B4E9",
                               "degraded" = "#E69F00",
                               "insufficient" = "#999999")) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.2,                    # Width of the error bars
                position = position_dodge(0.9))


ggplot(test4, aes( bootstrap_sample, kurtosis, color = rating)) +
  geom_point() +
  geom_line(aes(group = rating))


