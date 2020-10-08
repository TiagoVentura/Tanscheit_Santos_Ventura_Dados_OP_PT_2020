# ------------------------------------------------------------------------------- #
# Article: O Partido dos Trabalhadores e as Instituições Participativas: 
## a Influência da Dinâmica Intrapartidária na Adoção do Orçamento Participativo
# url: https://www.scielo.br/scielo.php?script=sci_abstract&pid=S0011-52582020000300202&lng=en&nrm=iso&tlng=pt
# Authors: Santos, Tanscheit, Ventura
# Update: June 15
# ------------------------------------------------------------------------------- #

# See session info for packages and dependencies

session <- sessionInfo()
session

# Pacotes -----------------------------------------------------------------
require(tidyverse)
require(dotwhisker)
library(stargazer)
library(lmtest)
library(broom)
library(tidyr)
library(lme4)
library(hrbrthemes)
library(arm)
library(here)
options(scipen=999)

# Acessar banco de dados e manipular variaveis ----------------------------

df <- read.csv(here("data.csv"))

# Making it a tbl
df<- tbl_df(df) 
df <- df[,-1]
df$pibpc <- (df$pib/df$poptot.y)
df$pib <- log(df$pib)
df$urbanpc <- 1 - df$ruralpc 
df$popdens <- df$popdens
df$urbanpc10 <- df$urbanpc *100



# Modelos Para Derminantes na Adoção do Orçamento Participativo ------------------


l <- glm(op ~ pib + urbanpc  + hdi.2000.100 + pref.pt + 
           organ + govpt, family=binomial(link = "logit"), data=df)

l.fe <- glm(op ~ pib + urbanpc + hdi.2000.100  + pref.pt + 
           organ + govpt+ as.factor(state.x), family=binomial(link = "logit"), data=df)

l.ml <- glmer(op ~ pib + urbanpc  + hdi.2000.100 + pref.pt + 
    organ + govpt+ (1+  pref.pt + pib + urbanpc  + hdi.2000.100+ organ + govpt|nome_uf), family=binomial(link = "logit"), data=df)
vignette("available-methods")

summary(l.ml)

# Tabela de Coeficientes

m1 <- tidy(l) %>% filter(term != "(Intercept)") %>% mutate(model = "Modelo Logit ")
m2 <- tidy(l.fe) %>% filter(term != "(Intercept)") %>%
  mutate(model = "Modelo Logit  com efeitos fixos") ; m2 <- m2[1:6,]

m3 <- broom.mixed::tidy(l.ml) %>% filter(term != "(Intercept)") %>%
  mutate(model = "Modelo Logit Hierarquico") %>% dplyr::select(-effect, -group) ; m3 <- m3[1:6,]

modelos<- rbind(m1, m2, m3) 

var= c( "Log Pib Municipal ",
        "IDH em 2000",
        "População Urbana (%)",
        "Prefeito PT", 
        "NGOs", 
        "Governador do PT")

var_en= c("Municipal GDP Log",
        "HDI in 2000",
        "Urban Population (%)",
        "PT Mayor", 
        "NGOs", 
        "PT Governor")


modelos_p <- modelos  %>% 
          mutate(term_rename= rep(var, 3), 
                 lw=estimate - 1.96*std.error, 
                 up=estimate + 1.96*std.error)



modelos_en <- modelos  %>% 
  mutate(term_rename= rep(var_en, 3), 
         lw=estimate - 1.96*std.error, 
         up=estimate + 1.96*std.error, 
         model=rep(c("Logit Model", "Logit + Fixed Effects", "Logit Multilevel"), each=6), 
         model=fct_relevel(model,"Logit Model", "Logit + Fixed Effects"))

ggplot(modelos_p, aes(y=estimate, x=term_rename, ymin=lw, group=model, shape=model, ymax=up)) + 
  geom_point(alpha=1, size=2, colour="black", fill="black",
             position=position_dodge(width = -.8)) +
  geom_errorbar(width=.2, colour="black", 
                fill="black", alpha=0.5, 
                position=position_dodge(width = -.8)) + 
  xlab("") +
  ylab("Coeficientes com 95% Intervalo de Confiança") +
  coord_flip() +
  geom_hline(yintercept=0, linetype="dotted") +
  theme_ipsum() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title.x=element_text(size=18), 
        legend.title = element_blank()) 

ggsave(here("resultado", "model1.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


ggplot(modelos_en, aes(y=estimate, x=term_rename, 
                       ymin=lw, group=model, shape=model, ymax=up)) + 
  geom_point(alpha=1, size=2, colour="black", fill="black",
             position=position_dodge(width = -.8)) +
  geom_errorbar(width=.2, colour="black", 
                fill="black", alpha=0.5, 
                position=position_dodge(width = -.8)) + 
  xlab("") +
  ylab("Coefficients with a 95% confidence intervals") +
  coord_flip() +
  geom_hline(yintercept=0, linetype="dotted") +
  theme_ipsum() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title.x=element_text(size=18), 
        legend.title = element_blank(), 
        legend.text = element_text(size=16)) 


ggsave(here("resultados", "model1_en.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")



# Figures 3 and 4 ---------------------------------------------------------


intercept <- fixef(l.ml)[1]
names <- rownames(coef(l.ml)$nome_uf)

r.uf <- coef(l.ml)$nome_uf[,1]

rse.uf <- se.coef(l.ml)$nome_uf[,1]

r.uf.df <- data_frame(names, r.uf, rse.uf) %>% 
            mutate(up=r.uf + 1.96*rse.uf, 
                   lb=r.uf -1.96*rse.uf, 
                   r.uf= r.uf-intercept,
                   up=up-intercept, 
                   lb=lb-intercept)


ggplot(r.uf.df, aes(y=r.uf, ymin=lb, ymax=up, x=names, label=names)) + 
  geom_point(size=3)+ 
  geom_errorbar(width=.2, colour="black", 
                fill="black", alpha=0.5, 
                position=position_dodge(width = -.8)) +
  coord_flip() + 
  geom_hline(yintercept =0, color="red") + xlab("") + 
  ylab("Interceptos Aleatórios por Estado") + 
  theme_ipsum( base_size = 18) +
  theme(text = element_text(size=18), 
        axis.title.x = element_text(size=18)) 

ggsave(here("resultados", "intercepts_pt.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


ggplot(r.uf.df, aes(y=r.uf, ymin=lb, ymax=up, x=names, label=names)) + 
  geom_point(size=3)+ 
  geom_errorbar(width=.2, colour="black", 
                fill="black", alpha=0.5, 
                position=position_dodge(width = -.8)) +
  coord_flip() + 
  geom_hline(yintercept =0, color="red") + xlab("") + 
  ylab("Random Intercepts by States") + 
  theme_ipsum( base_size = 18) +
  theme(text = element_text(size=18), 
        axis.title.x = element_text(size=18)) 

ggsave(here("resultados", "intercepts_en.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


names <- rownames(coef(l.ml)$nome_uf)

r.uf.pt <- ranef(l.ml)$nome_uf[,2]
rse.uf.pt <- se.coef(l.ml)$nome_uf[,2]

r.uf.pt <- data_frame(names, r.uf.pt, rse.uf.pt) %>% 
             mutate(up=r.uf.pt + 1.96*rse.uf.pt, 
                  lb=r.uf.pt -1.96*rse.uf.pt)
ggplot(r.uf.pt, aes(y=r.uf.pt, ymin=lb, ymax=up, x=names, label=names)) + 
  geom_point(size=3)+ 
  geom_errorbar(width=.2, colour="black", 
                fill="black", alpha=0.5, 
                position=position_dodge(width = -.8)) +
  coord_flip() + 
  geom_hline(yintercept = 0, color="red") + xlab("") + 
  ylab("Efeitos Aleatórios do PT por Estado") +
theme_ipsum( base_size = 18)+
  theme(text = element_text(size=18), 
        axis.title.x = element_text(size=18)) 


ggsave(here("resultados", "beta_pt.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


r.uf.pt <- data_frame(names, r.uf.pt, rse.uf.pt) %>% 
  mutate(up=r.uf.pt + 1.96*rse.uf.pt, 
         lb=r.uf.pt -1.96*rse.uf.pt)

ggplot(r.uf.pt, aes(y=r.uf.pt, ymin=lb, ymax=up, x=names, label=names)) + 
  geom_point(size=3)+ 
  geom_errorbar(width=.2, colour="black", 
                fill="black", alpha=0.5, 
                position=position_dodge(width = -.8)) +
  coord_flip() + 
  geom_hline(yintercept = 0, color="red") + xlab("") + 
  ylab("Random Slopes PT by States") +
  theme_ipsum( base_size = 18)+
  theme(text = element_text(size=18), 
        axis.title.x = element_text(size=18)) 

ggsave(here("resultados", "beta_pt.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")



# Figure 2: Hanmer Methods for Predicted Probability ----------------------


library(mvtnorm)  # for sampling from multivariate normal dist

l <- glm(op ~ pib + urbanpc  + hdi.2000.100 + pref.pt + 
    organ, family=binomial(link = "logit"), data=df) 


# Generating the Coeficients

coef <- coef(l)
vcov <- vcov(l)

# Matrix with the complete cases

d <- model.matrix(l)


# Simulating the coef

# set number of draws
n_draws <- 1000


# draw coefficients from posterior distribution
set.seed(17)
sim_coefs <- rmvnorm(n_draws, coef, vcov) 

# Generating the predictions 
sim.fit <- vector()
out.pt <- matrix(NA, 1000,1)

d[, "pref.pt"] <- 1

d.pt <- d
for( i in 1:1000){
  sim.fit <- pnorm(sim_coefs[i,] %*% t(d.pt))
  out.pt[i,] <- c(mean(sim.fit))
}

out.npt <- matrix(NA, 1000,1)

d[, "pref.pt"] <- 0

d.npt <- d
for( i in 1:1000){
  sim.fit <- pnorm(sim_coefs[i,] %*% t(d.npt))
  out.npt[i,] <- c(mean(sim.fit))
}

out.npt <- as.data.frame(out.npt) %>%
  mutate(index = "Sem prefeito do PT")

out.pt <- as.data.frame(out.pt) %>%
  mutate(index = "Com prefeito do PT")

d <- rbind(out.npt, out.pt)

d  <- d %>% group_by(index) %>% 
  summarise(Mean = mean(V1), 
          upper = quantile(V1, 0.975), 
    lower = quantile(V1, 0.025)
    
)


# ggplot(df, aes(fill=index, x=V1)) + geom_density(alpha=0.5)

ggplot(d, aes(y=Mean, x= index, fill= index)) + geom_point(size=2) + 
  geom_errorbar(aes(ymax=upper, ymin=lower, width=0.2), alpha=.5, size=1,  width=.1) + 
  scale_color_manual(labels=c("Sem Prefeito do PT", "Com prefeito do PT"), 
    values=c("tomato3", "blue")  ) +
  ylab("Probabilidade Predita") + 
  theme(legend.position="None") + xlab("") + ylim(0,1) + 
  theme_ipsum(base_size = 16) +
  theme(axis.title.y = element_text(size=18)) +
   guides(fill=FALSE, color=FALSE) 
  
ggsave(here("resultados", "figure2_pt.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


d$index <- c("With a PT Mayor", "Without a PT Mayor")

ggplot(d, aes(y=Mean, x= index, fill= index)) + geom_point(size=2) + 
  geom_errorbar(aes(ymax=upper, ymin=lower, width=0.2), alpha=.5, size=1,  width=.1) + 
  scale_color_manual(labels=c("With a PT Mayor", "Without a PT Mayor"), 
                     values=c("tomato3", "blue")  ) +
  ylab("Predicted Probabilities") + 
  theme(legend.position="None") + xlab("") + ylim(0,1) + 
  theme_ipsum(base_size = 16) +
  theme(axis.title.y = element_text(size=18)) +
  guides(fill=FALSE, color=FALSE) 

ggsave(here("resultados", "figure_2_en.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")



# Models for PT's Factions ------------------------------------------------

df <- read.csv("data.csv")

# Making it a tbl
df<- tbl_df(df) 
df <- df[,-1]
df$pibpc <- (df$pib/df$poptot.y)
df$pib <- log(df$pib)*100
df$urbanpc <- 1 - df$ruralpc 
df$popdens <- df$popdens
df$urbanpc <- df$urbanpc*100 
df$hdi.2000.100 <- df$hdi.2000.100*100

df %>% filter(pref.pt==1) %>% nrow()
df$esq100 <- df$esq*100



l4 <- glm(op ~ pib + urbanpc +  hdi.2000.100 + esq100 + 
            organ + govpt, family=binomial(link = "logit"), data=df[df$pref.pt==1,])

summary(l4)
# Graph

# Tabela de Coeficientes

m1 <- tidy(l4) %>% filter(term != "(Intercept)") %>% mutate(model = "Modelo Facções do PT")




m1[6, 2:3] <- m1[6, 2:3]/10 # reescalinting the dummy for the governor

var= c( "Log Pib Municipal ",
        "População Urbana (%)",
        "IDH em 2000",
        "Facções do PT",
        "NGOs", 
        "Governador do PT")


var_en= c("Municipal GDP Log",
          "Urban Population (%)",
          "HDI in 2000",
          "Size of PT Majority", 
          "NGOs", 
          "PT Governor")


modelos <- m1  %>% 
  mutate(term_rename= var,
         lw=estimate - 1.96*std.error, 
         up=estimate + 1.96*std.error)



ggplot(modelos, aes(y=estimate, x=term_rename, ymin=lw, ymax=up)) + 
  geom_point(alpha=1, size=2, colour="black", fill="black",
             position=position_dodge(width = -.8)) +
  geom_errorbar(width=.2, colour="black", 
                fill="black", alpha=0.5) + 
  xlab("") +
  ylab("Coeficientes com 95% Intervalo de Confiança") +
  coord_flip() +
  geom_hline(yintercept=0, linetype="dotted") +
  theme_ipsum() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title.x=element_text(size=18), 
        legend.title = element_blank()) 



ggsave(here("resultados", "figure_4_pt.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


modelos_en <- m1  %>% 
  mutate(term_rename= var_en,
         lw=estimate - 1.96*std.error, 
         up=estimate + 1.96*std.error)


ggplot(modelos_en, aes(y=estimate, x=term_rename, ymin=lw, ymax=up)) + 
  geom_point(alpha=1, size=2, colour="black", fill="black",
             position=position_dodge(width = -.8)) +
  geom_errorbar(width=.2, colour="black", 
                fill="black", alpha=0.5) + 
  xlab("") +
  ylab("Coefficients with a 95% confidence intervals") +
  coord_flip() +
  geom_hline(yintercept=0, linetype="dotted") +
  theme_ipsum() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title.x=element_text(size=18), 
        legend.title = element_blank()) 



ggsave(here("resultados", "figure_4_en.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


# Hanmer method

library(mvtnorm)  # for sampling from multivariate normal dist

l4 <- glm(op ~ pib + urbanpc + hdi.2000.100 + esq100 + 
    organ, family=binomial(link = "logit"), data=df[df$pref.pt==1,])


# Generating the Coeficients

coef <- coef(l4)
vcov <- vcov(l4)

# Matrix with the complete cases

d <- model.matrix(l4)

# Simulating the coef

# set number of draws
n_draws <- 1000


# draw coefficients from posterior distribution
set.seed(17)
sim_coefs <- rmvnorm(n_draws, coef, vcov) 

# Generating the predictions

# Repository
sim.fit <- vector()
m <- matrix(NA, 1000,1)
out<- list(m, m, m ,m , m ,m ,m , m, m, m,m)
esq <-c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)

for(j in 1:11){
d[,"esq100"] <- esq[j]

print(j)

for( i in 1:1000){
  sim.fit <- pnorm(sim_coefs[i,] %*% t(d))
  out[[j]][i,] <- c(mean(sim.fit))
}}

fun <- function(x){
  m <- mean(x[,1])
  u <- quantile(x[,1], 0.975)
  l <- quantile(x[,1], 0.025)
  z <- data_frame(m, u, l)
  }

d <- out %>% purrr::map(as.data.frame)

res <- d%>% purrr::map_df(fun)

res$esq <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)


ggplot(res, aes(y=m, x = esq)) + 
  geom_ribbon(aes(ymin = l,
    ymax = u), alpha=0.1) + 
  geom_line(size=1, color="black", alpha=.5) + ylab("Probabilidade Predita") +
  xlab ("Votação da maioria do PT") + theme_ipsum(base_size=18) +
  theme(axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18))


ggsave(here("resultados", "figure_5_pt.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


ggplot(res, aes(y=m, x = esq)) + 
  geom_ribbon(aes(ymin = l,
                  ymax = u), alpha=0.1) + 
  geom_line(size=1, color="black", alpha=.5) + ylab("Predicted Probabilities") +
  xlab ("Vote of the PT majority") + theme_ipsum(base_size=18) +
  theme(axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18))

ggsave(here("resultados", "figure_5_pt.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


save.image(file="models.Rdata")
