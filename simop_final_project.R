# Yu Tang
# Arden Cohen #
# Wheaton Jackoboice #
# Sully Noor #


library(rvest)
library(ROI)
library(dplyr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(magrittr)
library(simmer)

#q1

q1df <- data.frame(product = c("flower (3.5 grams)", "pre-rolls (1 gram)", 
                       "concentrates (1 gram)", "edibles (100 mg)"), 
           price = c(45, 12, 60, 15), 
           cost = c(25, 5, 50, 10), 
           demand = c(37, 15, 20, 28))

q1df$margin <- q1df$price - q1df$cost

cvec <- c(q1df$price,0)
bvec <- c(850000, 0,rep(50,4))
constdir <- c("<=","==",rep('<=', 4))
Amat <- rbind(c(25,5,50,10,0),
              c(1,1,1,1,-1),
              c(100,0,0,0,-37),
              c(0,100,0,0,-15),
              c(0,0,100,0,-20),
              c(0,0,0,100,-28))

constraint <- L_constraint(Amat, constdir, bvec)
model_1 <- OP(cvec, constraint, types = c(rep('I',length(cvec))),maximum = TRUE)
model_s1 <- ROI_solve(model_1)
q1_solution <- solution(model_s1)[1:4]
sum(q1_solution * q1df$margin) #profit
sum(q1_solution * q1df$price) #rev

sum(solution(model_s1)[1:4]*q1df$cost)



######################################################q2
data.frame(product = c("flower (3.5 grams)", "pre-rolls (1 gram)", 
                       "concentrates (1 gram)", "edibles (100 mg)"), 
           price = c(45, 12, 60, 15), 
           cost = c(25, 5, 50, 10), 
           demand = c(37, 15, 20, 28)) |>
  flextable::flextable(cwidth = 2.5, cheight = .15)

q2sol <- data.frame(0)
for (i in 1:1000) {
concendemand <- runif(1,25,50)
ediblesdemand <-  runif(1,25,50)
flowerdemand <- (15*100/(15+37))*(100-concendemand-ediblesdemand)/100
prerolldemand <- (37*100/(15+37))*(100-concendemand-ediblesdemand)/100
q2df <- data.frame(product = c("flower (3.5 grams)", "pre-rolls (1 gram)", 
                               "concentrates (1 gram)", "edibles (100 mg)"), 
                   price = c(45, 12, 60, 15), 
                   cost = c(25, 5, 50, 10), 
                   demand = c(flowerdemand, prerolldemand, 25, 50))


cvec <- c(q2df$price,0)
bvec <- c(850000, 0,rep(50,4))
constdir <- c("<=","==",rep('<=', 4))
Amat <- rbind(c(25,5,50,10,0),
              c(1,1,1,1,-1),
              c(100,0,0,0,-flowerdemand),
              c(0,100,0,0,-prerolldemand),
              c(0,0,100,0,-concendemand),
              c(0,0,0,100,-ediblesdemand))


constraint <- L_constraint(Amat, constdir, bvec)
model_2 <- OP(cvec, constraint, types = c(rep('I',length(cvec))),maximum = TRUE)
model_s2 <- ROI_solve(model_2)
q2_solution <- solution(model_s2)[1:4]#profit #rev
q2rev <- sum(q2_solution * q2df$price)
q2sol[i,] <- q2rev
}
mean(q2sol$X0)

sum(q2_solution*q2df$cost)



#q3
q3df1 <- data.frame(company = c(rep("Better Provisioning", 4), 
                         rep("Harbor Farmz", 4), 
                         rep("Hazy Farms", 4), 
                         rep("Rare Michigan Genetics", 4)), 
             store = rep(c("Ann Arbor", "Muskegon", "Utica", "Traverse City"), 4), 
             cost = c(10, 8, 12, 20, 
                      10, 8, 12, 20, 
                      5, 10, 5, 25,
                      10, 8, 12, 20))
q3df2 <- data.frame(company = c("Better Provisioning", "Harbor Farmz",  
                       "Hazy Farms", "Rare Michigan Genetics"),  
           supply = c(700, 300, 550, 420), 
           store = c("Ann Arbor", "Muskegon", "Utica", "Traverse City"), 
           demand = c(650, 200, 450, 250))


Amat1 <- matrix(0,ncol = length(q3df1$store),
               nrow = length(c(q3df2$store)),
               dimnames = list(q3df2$store, q3df1$store))
Amat2 <- matrix(0,ncol = length(q3df1$company),
                nrow = length(c(q3df2$company)),
                dimnames = list(q3df2$company, q3df1$company))

for(i in c(q3df2$company, q3df2$store)) {
  input <- c(q3df2$company, q3df2$store) == i
  Amat1[rownames(Amat1) == i,
       colnames(Amat1) == i] <- 1
  Amat2[rownames(Amat2) == i,
       colnames(Amat2) == i] <- 1
}
Amat <- rbind(Amat1,Amat2)

cvec <- q3df1$cost
bvec <- c(q3df2$demand,q3df2$supply)
constdir <- c(rep(">=",length(q3df2$demand)),
              rep("<=",length(q3df2$supply)))
constraint <- L_constraint(Amat, constdir, bvec)
model_3 <- OP(cvec, constraint, types = c(rep('I',length(cvec))),maximum = FALSE)
model_s3 <- ROI_solve(model_3)
solution(model_s3)
solution_s3 <- rbind(store = q3df1$store,
                     company = q3df1$company,
                     solution = solution(model_s3),
                     cost = q3df1$cost*solution(model_s3))





#q5
load("C:/Users/TF2020/Downloads/job_preference_list.RData")
q5df <- job_preference
print(q5df)
jobtype <-  q5df$jobType |> unique()
name <- q5df$personName |> unique()
Amat1 <- matrix(0,ncol = length(q5df$personName),
                nrow = length(name),
                dimnames = list(name, q5df$personName))
Amat2 <- matrix(0,ncol = length(q5df$jobType),
                nrow = length(jobtype),
                dimnames = list(jobtype, q5df$jobType))
for(i in c(jobtype, name)) {
  Amat1[rownames(Amat1) == i,
        colnames(Amat1) == i] <- 1
  Amat2[rownames(Amat2) == i,
        colnames(Amat2) == i] <- 1
}

Amat <- rbind(Amat1,Amat2)
print(Amat)

cvec <- q5df$preference
bvec <- c(rep(1,length(name)), 
          rep(5,length(jobtype)))
constdir <- c(rep('==',length(c(jobtype, name))))
length(name)/length(jobtype)
constraint <- L_constraint(Amat, constdir, bvec)
model_5 <- OP(cvec, constraint, types = c(rep('B',length(cvec))),maximum = TRUE)
model_s5 <- ROI_solve(model_5)
q5df$assignedjob <- solution(model_s5)
View(q5df %>% filter(assignedjob > 0))





#q6

mins <- 1
hrs <- 60 * mins
day <- 24 * hrs
week <- 7 * day
month <- 30.437 * day
min_time <- data.frame(0)






library(doParallel)
library(foreach)
library(furrr)
library(future)
plan(multisession, workers = parallel::detectCores()- 1)
q6df <- furrr::future_map_dfr(.x = 1:1000, ~{
  
  Plant <- trajectory("Plant") %>% 
    set_attribute("start_time", function() {now(grow)}) %>%
    seize("flowering_stage") %>% 
    timeout(function() {runif(1, 8*week, 11*week)}) %>% 
    release("flowering_stage") %>% 
    seize("drying_stage") %>% 
    timeout(function() {runif(1, 7*day, 18*day)}) %>% 
    release("drying_stage") %>% 
    branch(function() sample(1:2, 1, prob = c(0.55, 0.45)),
           continue = c(TRUE, TRUE), 
           trajectory() %>%  #concentrate branch
             seize("freezing_stage") %>% 
             timeout(20*mins) %>%
             release("freezing_stage") %>% 
             seize("extraction_stage") %>% 
             timeout(function() {rnorm(1, 15*mins, 2*mins)}) %>% 
             release("extraction_stage") %>% 
             seize("filtration_stage") %>% 
             timeout(function() {rnorm(1, 1*hrs, 8*mins)}) %>% 
             release("filtration_stage") %>% 
             seize("solvent_evaporation_stage") %>% 
             timeout(1*hrs) %>% 
             release("solvent_evaporation_stage") %>% 
             seize("Decarbonization_stage") %>% 
             timeout(function() {rnorm(1, 2*day, 8*hrs)}) %>%
             release("Decarbonization_stage") %>% 
             seize("Distillation_stage") %>% 
             timeout(function() {rnorm(1, 8*hrs, 2*hrs)}) %>% 
             release("Distillation_stage"),
           trajectory() %>% #flower branch
             seize("Trimming_stage") %>% 
             timeout(function() {rnorm(1, 1*hrs, 15*mins)}) %>% 
             release("Trimming_stage") %>% 
             seize("Curing_stage") %>% 
             timeout(function() {runif(1, 1*month, 4*month)}) %>% 
             release("Curing_stage")
    )
  
  grow <- simmer("grow") %>% 
    add_resource("flowering_stage", capacity = Inf, queue_size = Inf) %>%
    add_resource("drying_stage", capacity = Inf, queue_size = Inf) %>% 
    add_resource("freezing_stage", capacity = 1000, queue_size = Inf) %>% 
    add_resource("extraction_stage", capacity = 100, queue_size = Inf) %>% 
    add_resource("filtration_stage", capacity = 500, queue_size = Inf) %>% 
    add_resource("solvent_evaporation_stage", capacity = 500, queue_size = Inf) %>% 
    add_resource("Decarbonization_stage", capacity = 500, queue_size = Inf) %>% 
    add_resource("Distillation_stage", capacity = 100, queue_size = Inf) %>% 
    add_resource("Trimming_stage", capacity = 20, queue_size = Inf) %>% 
    add_resource("Curing_stage", capacity = Inf, queue_size = Inf) %>%
    add_generator("Plant", Plant, mon = 1, at(0))
  
  
  simmer::run(grow, until = 500000)
  q6df <- get_mon_arrivals(grow)
  #q6df <- q6df$end_time %>% min()
  q6df$run <- .x
  q6df
})
mean(q6df$end_time)
sampledf <- q6df[1:50,]
View(sampledf)

