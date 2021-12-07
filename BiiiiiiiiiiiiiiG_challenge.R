# MSBA class of 2022 decide to host an event at the local haunted house on Halloween
library(dplyr)
library(rvest)
library(ROI)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(magrittr)
library(tidyr)
library(simmer)
library(doParallel)
library(foreach)
library(furrr)
library(future)
#first thing first, we need to assign our class to different positions
# job opening are security"," actor"," janitor"," recipient"," technical_support",

dfstaff <- data.frame(NDID = c("abala","acohen1","aoconn24","asweet","avernaza",
                               "awerneck","bdeehan","bgong","bxu4","cblackmu",
                               "cfuente3","cgu2","cpallett","dkelly13","dlisa",
                               "ehou","ewilson6","gtrindad","ikaur","jdamaril",
                               "jhu9","jlin9","jliu28","jramir24","jwu25","jzheng5",
                               "kryan23","lbrothe1","lge2","mczabala","mfay3","mgalajda","mhowe","mmirer",
                               "mpascuto","mrziha","mterreri","ndong","pplisson","qsun2","qwu7",
                               "rbridges","rcole5","rdaryan2","rkim6","rnyajeka","sbruchha",
                               "sgupta4","sliu28","smyers5","snagabop","snoor","srodrig7",
                               "svettic2","tadeoshi","tzepf","vpenmets","wjackobo","wliu9","wwu8",
                               "xwu22","ychen47","ycui3","yguan2","yhe26","yhuang25","yji","yliao3",
                               "yliu38","yliu42","ymao2","ytang7","yxie4","yyao7","yyu6"))
dfstaff$sercurity <- as.integer(runif(75, 1,6))
dfstaff$actor <- as.integer(runif(75, 3,6)) 
dfstaff$janitor <- as.integer(runif(75, 1,4)) 
dfstaff$recipient <- as.integer(runif(75, 1,5))
dfstaff$technical_support <- as.integer(runif(75, 2,5))
# above are people's comforts level and skill level with the role, 1 means uncomfortable and incapable, 5 means very comfort and good
# for this situation, we need 3 shifts to keep the  operation system working
# for each shift we need 2 janitors, 3 recipients, 5 securities, 11 actors and 4 technical supports

name <- rep(dfstaff$NDID,5)
role <- c(rep('sercurity',75),rep('actor',75),rep('janitor',75),rep('recipient',75),rep('technical_support',75))

AmatX <- matrix(0,ncol = length(name),
                nrow = length(c(dfstaff$NDID)),
                dimnames = list(c(dfstaff$NDID), name))
AmatY <- matrix(0,ncol = length(role),
                nrow = n_distinct(role),
                dimnames = list(unique(role), role))  
for(i in c(role, name)) {
  AmatX[rownames(AmatX) == i,
        colnames(AmatX) == i] <- 1
  AmatY[rownames(AmatY) == i,
        colnames(AmatY) == i] <- 1
}
Amat <- rbind(AmatX, AmatY)



cvec <- c(dfstaff$sercurity,dfstaff$actor,dfstaff$janitor,dfstaff$recipient, dfstaff$technical_support)
bvec <- c(rep(1,n_distinct(name)), 15, 33, 6, 9, 12)
constdir <- c(rep("<=", length(rownames(Amat))))
constraint <- L_constraint(Amat, constdir, bvec)
staff_model <- OP(cvec, constraint, types = c(rep("B", length(cvec))), maximum = TRUE)
staff_solving <- ROI_solve(staff_model)
staff_solution <- solution(staff_solving) %>% print()


namelist <- data.frame(NDID = colnames(Amat), Role = role, pick = staff_solution, level = cvec)%>% filter(pick > 0)#organized the final shift
n_distinct(namelist$NDID)#make sure everyone get a position


#since number of visitors are different are vary between hours, we need to have some of our most skillful students to consist a team
AmatS <- matrix(0,ncol = length(namelist$Role),
                nrow = n_distinct(role),
                dimnames = list(unique(role), namelist$Role))  
for(i in namelist$Role) {
  AmatS[rownames(AmatS) == i,
        colnames(AmatS) == i] <- 1
}
AmatS = rbind(AmatS, shift = 0)

for (i in 1:3) {
  cvec <- namelist$level
  bvec <- c(5,11,2,3,4,0)
  constdir <- c(rep("<=", length(bvec)))
  constraint <- L_constraint(AmatS, constdir, bvec)
  shift_model <- OP(cvec, constraint, types = c(rep("B", length(cvec))), maximum = TRUE)
  shift_solving <- ROI_solve(shift_model)
  shift_solution <- solution(shift_solving)
  AmatS[6,] <- AmatS[6,] + shift_solution * i
}

namelist$shift <-  AmatS[6,]
shift_table <- subset(namelist, select = -c(pick, level)) #simplify the shift_table

find_your_shift <- function(i) {
  shift_table[shift_table$NDID == i
   , ]
}

find_your_shift("snoor") #Insert your "NDID" lower case pls in order to find your time schedule


#After we decide our staff, we need to start design our "horror house" event, which we 
#need to decide the adventure routes for the rest of Notre Dame student body and local community
mins <- 1
hrs <- 60 * mins



process_predic <- data.frame(0)



plan(multisession, workers = parallel::detectCores()- 1)
process_predic <- furrr::future_map_dfr(.x = 1:500, ~{ 
  
  Visitor <- trajectory("Visitor") %>% 
    set_attribute("start_time", function() {now(launch)}) %>% 
    seize("jigsaw_puzzle") %>% 
    timeout(function() {rnorm(1, 5*mins, 3*mins)}) %>%
    release("jigsaw_puzzle") %>% 
    seize("ghost_tunnel") %>% 
    timeout(function() {runif(1, 5*mins, 10*mins)}) %>% 
    release("ghost_tunnel") %>% 
    branch(function() sample(1:2, 1, prob = c(0.75, 0.25)),   #visitors can decide they want to skip next stage or not(base on our survey, we assume 25% of them will skip)
           continue = c(TRUE,TRUE),
           trajectory() %>% 
             seize("A_Quiet_Place") %>% 
             timeout(function() {runif(1, 3*mins, 5*mins)}) %>%
             release("A_Quiet_Place") %>% 
             seize("Psycho") %>% 
             timeout(function() {rnorm(1, 2.5*mins, 1*mins)}) %>% 
             release("Psycho") %>% 
             branch(function() {sample(1:2, 1, prob = c(0.57, 0.43))}, #visitor will have choice to pick the different track
                    continue = c(TRUE,TRUE),
                    trajectory() %>%
                      seize("Rosemary's_Baby") %>% 
                      timeout(function() {runif(1, 3*mins, 6*mins)}) %>%
                      release("Rosemary's_Baby"),
                    trajectory() %>%
                      seize("Eyes_Without_a_Face") %>% 
                      timeout(function() {runif(1, 2*mins, 5*mins)}) %>%
                      release("Eyes_Without_a_Face") %>% 
                      rollback(amount = 4, times = 1)), #will rollback if visitor pick this track
           trajectory() %>% 
             seize("The_Friends_of_Eddie_Coyle") %>% 
             timeout(function() {rnorm(1, 3*mins, 1.5*mins)}) %>%
             release("The_Friends_of_Eddie_Coyle")) %>% 
    branch(function() sample(1:2, 1, prob = c(0.5, 0.5)), # ultimate challenge, visitor could opt out
           continue = c(TRUE, TRUE),
           trajectory() %>%
             seize("insidious") %>% 
             timeout(function() {runif(1, 10* mins, 15 * mins)}) %>%
             release("insidious"),
           trajectory() %>%
             seize("snack") %>%  #people who opted out the challenge can grab some snack before they decide whether they gonna join the party
             timeout(function() {runif(1, 2* mins, 5 * mins)}) %>%
             release("snack")) %>% 
    branch(function() {sample(0:1, 1, prob = c(.6,.4)) == 1},#we are expecting 70% of visitors will stay for the party
           continue = TRUE,
           trajectory() %>%
             seize("After_game_party") %>% #during the party, we are going to play the movie Halloween
             timeout(60*mins) %>%
             release("After_game_party"))
  
  
  launch <- simmer("launch") %>% 
    add_resource("jigsaw_puzzle", capacity = 30, queue_size = 80) %>%
    add_resource("ghost_tunnel", capacity = 25, queue_size = 80) %>%
    add_resource("A_Quiet_Place", capacity = 25, queue_size = 70) %>%
    add_resource("Psycho", capacity = 25, queue_size = 70) %>%
    add_resource("Rosemary's_Baby", capacity = 20, queue_size = 60) %>%
    add_resource("Eyes_Without_a_Face", capacity = 15, queue_size = 60) %>%
    add_resource("The_Friends_of_Eddie_Coyle", capacity = 18, queue_size = 80) %>%
    add_resource("insidious", capacity = 25, queue_size = 120) %>%
    add_resource("snack", capacity = Inf, queue_size = Inf) %>% 
    add_resource("After_game_party", capacity = 200, queue_size = Inf) %>% 
    add_generator("Visitor", Visitor, mon = 1, function() {c(0,rexp(2100, 7),-1)}) # we are expecting 7 visitor per min at peak session
  
  # we are going to host 3 sessions during the day, above are the session that we are expecting the most of the visitors
  simmer::run(launch, until = 300)
  process_predic <- get_mon_arrivals(launch)
  process_predic$run <- .x
  process_predic

})
n_visitor <- as.integer(nrow(process_predic)/500) #average number of visitors shows up in each run of simulation session
n_visitor
p_finished <- (process_predic %>% filter(finished == 'TRUE') %>% nrow())/nrow(process_predic) #percentage of the visitor that finished our activity
p_finished



aggregate(process_predic,by = list(process_predic$run), FUN = "mean")

#sponsor by the Notre Dame, we are being able to provide souvenir for each visitor who finished the haunted house activity
#Assume we want to make sure at least 80% of the people can have souvenir


qbinom(.8, n_visitor, p_finsihed, FALSE) # number of the souvenir we need to prepare for the busiest session




