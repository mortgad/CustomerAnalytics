#------------------------------
# Choice-based conjoint analysis
#------------------------------
# Loading data and turning them into factors:
cbc.df<-read.csv("http://goo.gl/5xQObB",colClasses=c(seat="factor", 
                                                     cargo ="factor", 
                                                     price="factor",
                                                     choice="integer"))
cbc.df$eng<-factor(cbc.df$eng,levels=c("gas","hyb","elec"))
cbc.df$carpool<-factor(cbc.df$carpool,levels=c("yes","no"))
summary(cbc.df)

# inspecting data
summary(cbc.df) # Summary viser hvor mange gange de forskellige muligheder optræder. 
# Dette skulle gerne være balanced for at give os et fair samligningsgrundlag, med mindre vi har tidligere viden.

# Hvor ofte folk vælger en given pris, cargo, carpool, seat og engine i vores spørgeskema.
xtabs(choice~price,data=cbc.df)
xtabs(choice~cargo,data=cbc.df)
xtabs(choice~carpool,data=cbc.df)
xtabs(choice~seat,data=cbc.df)
xtabs(choice~eng,data=cbc.df)

# prepare the data (convert the data to an mlogit.data)
library(dfidx)
# add a column with unique question numbers, as needed in mlogit 1.1+
cbc.df$chid <- rep(1:(nrow(cbc.df)/3), each=3) # Skal ændres hvis der er flere muligheder
# shape the data for mlogit
cbc.mlogit <- dfidx(cbc.df, choice="choice", 
                    idx=list(c("chid", "resp.id"), "alt" ))

# fitting a nmlogit model
library(mlogit) #install.packages("mlogit") 
# without intercept
m1 <- mlogit(choice ~ 0 + 
               seat + 
               cargo + 
               eng + 
               price,
             data = cbc.mlogit)
summary(m1)
# Estimate lists the estimated parameter (part worth coefficient) for each level;
# these must be interpreted relative to the base levels of each attribute (if factor)
# Estimates that are larger in magnitude indicate stronger preferences, 
# so we can see that customers strongly disliked electric
# engines (relative to the base level, which is gas) and 
# disliked the $40K price (relative to the base level price of $30)
# all parameter estimates are on the logit scale
# and typically range between −2 and 2

# with intercept
m2 <- mlogit(choice ~ seat + cargo + eng + price,data = cbc.mlogit)
summary(m2)

# comparing the two models we see that their is no difference.
lrtest(m1,m2)

# intercept tjekker for bias om folk generelt altid svare B


# treating price as continuous (instead of a factor with levels)
m3 <- mlogit(choice ~ 0 + 
               seat + 
               cargo + 
               eng + 
               as.numeric(as.character(price)),
             data=cbc.mlogit)
summary(m3)
# output now shows a single parameter for price. The estimate is negative
# indicating that people prefer lower prices to higher prices
# As prices increase, people are less likely to chosse the model.
# If not sure about numeric or categorial or if there is only a set option of prices
# one should always choose categorial.

# comparing the two models
lrtest(m1,m3)


# reporting findings with more interpretable measures like WTP = Willingness to pay.
coef(m3)["cargo3ft"]/(-coef(m3)["as.numeric(as.character(price))"]/1000)
#$2,750.60 is the price at which customers become indifferent between the
# two cargo capacity options

# This same willingness to pay value can be computed for
# every attribute in the study and reported to decision makers to help them understand
# how much customers value various features. e.g. : 
coef(m3)["seat7"]/(coef(m3)["as.numeric(as.character(price))"]/1000) # 3084.38
coef(m3)["engelec"]/(coef(m3)["as.numeric(as.character(price))"]/1000) # 8823.302 

# Skal bare ses som den absolutte værdi at værdien imellem at vælge imellem 2 alternativer.
# Hvis coefficenten er negativ i model outputtet skal produktet være billigere
# er den positiv er det hvor meget mere de vil betale.


# Finally, predict predict how customers would choose among 
# those new alternatives (combinations) not included in the original data
  predict.mnl<-function(model,data){
  data.model<-model.matrix(update(model$formula,0~.),data=data)[,-1]
  utility<-data.model%*%model$coef
  share<-exp(utility)/sum(exp(utility))
  cbind(share,data)
  }

# create some new data
attrib<-list(seat=c("6","7","8"),
cargo=c("2ft","3ft"),
eng=c("gas","hyb","elec"),
price=c("30","35","40"))
new.data <- expand.grid(attrib)[c(8,1,3,41,49,26),] # extract only a few combinations
new.data # Her ser vi altså combination 8 combination 1 osv.

# pass these designs to predict.mnl() to determine what customers would
# choose if they had to pick among these 6 minivan alternatives
predict.mnl(m3,new.data)
# see in column share: respondents choose the 7 seat hybrid engine minivan 
# with 2 ft of cargo space at $30K a little more than 11%
# of the time


## Sensitivity plots show how the shares above would change if we changed one
## metric/factors and keept all else constant.
sensitivity.mnl <- function (model, attrib, base.data , competitor.data ) {
   # Function for creating data for a share - sensitivity chart
     # model: mlogit object returned by mlogit() function
     # attrib: list of vectors with attribute levels to be used in sensitivity
     # base.data: data frame containing baseline design of target product
     # competitor. data: data frame containing design of competitive set
     data <- rbind (base.data , competitor.data )
     base.share <- predict.mnl(model , data )[1,1]
     share <- NULL
     for (a in seq_along(attrib)) {
       for (i in attrib[[a]]) {
         data [1,] <- base.data
         data [1,a] <- i
         share <- c(share , predict.mnl(model , data )[1,1])
         }
       }
     data.frame (level= unlist (attrib), share=share , increase=share - base.share)
     }
## Then changing one variable to see how it affects the shares.
base.data <- expand.grid (attrib)[c(8), ]
competitor.data <- expand.grid (attrib)[c(1, 3, 41, 49, 26), ]
(tradeoff <- sensitivity.mnl(m1, attrib , base.data , competitor.data ))


# making the plot to se changes from choices 8.
barplot(tradeoff$increase,horiz=FALSE ,names.arg=tradeoff$level,
        ylab ="Change in Share for Baseline Product")
