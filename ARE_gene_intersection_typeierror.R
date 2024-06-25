comb <- function(x, ...) {
  lapply(seq_along(x),
         function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
}

n.cores = 4
cluster = parallel::makeCluster(n.cores, type = "PSOCK")
doParallel::registerDoParallel(cl = cluster)

p = NULL

pvals = foreach(i = 1:100, .combine = 'comb', .multicombine = TRUE,
                .init = list(p)) %dopar%{
                  a = sample(1:length(data$gene),373)
                  
                  sub = data[a,]
                  
                  randRE(data$gene, data$ARE, 373, 10000)
                  p = length(which(nullDist$RE > mean(sub$ARE)))/10000
                }
parallel::stopCluster(cl = cluster)

typeierrorrate = plyr::ldply(pvals[[1]])
write.csv(typeierrorrate, "file_name.csv")

data = read.csv("file_name.csv")
length(which(data$V1 < 0.025 | data$V1 > 0.975))/10000 #0.0499! Yay!