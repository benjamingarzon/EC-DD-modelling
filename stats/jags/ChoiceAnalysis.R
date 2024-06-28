# Choice probabilities

myplot.choiceprob = ggplot(
  data = choicedata.group,
  aes(
    x = ev,
    y = prob_late.mean,
    ymin = prob_late.mean - prob_late.sem,
    ymax = prob_late.mean + prob_late.sem,
    color = Context
  )
) +
  geom_line() +
  geom_point() +
  geom_errorbar(width = 0.5) +
  xlab('Later amount ($)') +
  ylab('Frequency of \nlater option') + theme(legend.position = 'bottom', legend.title = element_blank())

myplot.choiceprob.order = myplot.choiceprob + facet_grid(. ~ Context_order)
myplot.choiceprob.group = myplot.choiceprob + facet_grid(. ~ Group)
print(myplot.choiceprob.order)
print(myplot.choiceprob.group)

choicedata.mean = choicedata %>%
  group_by(subjID,
           Context,
           context,
           Context_order,
           context_order,
           Group,
           ev.cut) %>%
  dplyr::summarise(ev = mean(ev),
                   prob_late = mean(choice))

choicedata.mean.high = subset(choicedata.mean, Context == 'High volatility')
choicedata.mean.low = subset(choicedata.mean, Context == 'Low volatility')
choicedata.diff = merge(
  choicedata.mean.high,
  choicedata.mean.low,
  by = c('subjID', 'Group', 'ev.cut', 'ev'),
  suffixes = c('.high', '.low')
)

choicedata.diff$prob_late.diff = choicedata.diff$prob_late.high - choicedata.diff$prob_late.low

choicedata.diff.group = choicedata.diff %>%
  group_by(ev.cut, Group) %>%
  dplyr::summarise(
    prob_late.mean = mean(prob_late.diff),
    prob_late.sem = sem(prob_late.diff),
    ev = mean(ev)
  )

myplot.choiceprob.diff = ggplot(
  data = choicedata.diff.group,
  aes(
    x = ev,
    y = prob_late.mean,
    ymin = prob_late.mean - prob_late.sem,
    ymax = prob_late.mean + prob_late.sem
  )
) +
  geom_line() +
  geom_point() +
  geom_errorbar(width = 0.5) +
  xlab('Later amount ($)') +
  ylab('Difference in frequency of \nlater option (high-low vol.)') + theme(legend.position = 'bottom', legend.title = element_blank())

myplot.choiceprob.diff.group = myplot.choiceprob.diff + facet_grid(. ~ Group)
print(myplot.choiceprob.diff.group)

model = fitmodel(
  "choice ~ Group*Context*amount_later_centered + (1|subjID)", # background_col
  choicedata,
  c(
    "_choice_context_" = "ContextLow volatility",
    "_choice_groupxcontext_" = "GroupLow vol. first:ContextLow volatility",
    "_choice_groupxamount_" = "GroupLow vol. first:amount_later_centered",
    "_choice_contextxamount_" = "ContextLow volatility:amount_later_centered",
    "_choice_groupxcontextxamount_" = "GroupLow vol. first:ContextLow volatility:amount_later_centered"
  ),
  family = 'binomial'
)

# model = fitmodel(
#   "choice ~ Group + Context + amount_later_centered + Group:Context + Context:amount_later_centered  + background_col + (1|subjID)",
#   choicedata,
#   c(
#     "_choice_context_" = "ContextLow volatility",
#     "_choice_groupxcontext_" = "GroupLow vol. first:ContextLow volatility",
#     "_choice_contextxamount_" = "ContextLow volatility:amount_later_centered"
#   ),
#   family = 'binomial'
# )

print(summary(model))

mm = model.matrix(model)

inds <- c(1, 2, 3, 4, 6, 7)
#inds <- c(1, 3, 4, 7)

ffx <- fixef(model)
rfx <- ranef(model)$subjID
fefs <- names(ffx)[inds]
print(fefs)
eta <- mm[, fefs] %*% ffx[fefs] + rfx[choicedata$subjID, 1]

choice.pred <- (plogis(eta) + residuals(model, type='response'))

library(caret)
confusionMatrix(as.factor((choice.pred>0.5)*1), as.factor(choicedata$choice))
choicedata$choice.pred <- choice.pred 
cor.test(choice.pred, choicedata$choice)
boxplot(choice.pred~ choicedata$choice)
choice.fixef = coef(summary(model))

choicedata.mean.agg <- choicedata %>%
  group_by(subjID,
           Context,
           context,
           ev.cut) %>%
  dplyr::summarise(ev = mean(ev),
                   prob_late = mean(choice.pred))

choicedata.group.agg = choicedata.mean.agg %>%
  group_by(Context, ev.cut) %>%
  dplyr::summarise(
    prob_late.mean = mean(prob_late),
    prob_late.sem = sem(prob_late),
    ev = mean(ev)
  )

myplot.choiceprob.agg = ggplot(
  data = choicedata.group.agg,
  aes(
    x = ev,
    y = prob_late.mean,
    ymin = prob_late.mean - prob_late.sem,
    ymax = prob_late.mean + prob_late.sem,
    color = Context
  )
) +
  geom_line() +
  geom_point() +
  geom_errorbar(width = 0.5) +
  xlab('Later amount ($)') +
  ylab('Frequency of \nlater option') + theme(legend.position = 'bottom', legend.title = element_blank())
print(myplot.choiceprob.agg)

choicedata.mean.agg = choicedata %>%
  group_by(subjID,
           Context,
           context,
           ev.cut) %>%
  dplyr::summarise(ev = mean(ev),
                   prob_late = mean(choice.pred))

choicedata.mean.high = subset(choicedata.mean.agg, Context == 'High volatility')
choicedata.mean.low = subset(choicedata.mean.agg, Context == 'Low volatility')

choicedata.diff = merge(
  choicedata.mean.high,
  choicedata.mean.low,
  by = c('subjID', 'ev.cut', 'ev'),
  suffixes = c('.high', '.low')
)

choicedata.diff$prob_late.diff = choicedata.diff$prob_late.high - choicedata.diff$prob_late.low

choicedata.diff.group = choicedata.diff %>%
  group_by(ev.cut) %>%
  dplyr::summarise(
    prob_late.mean = mean(prob_late.diff),
    prob_late.sem = sem(prob_late.diff),
    ev = mean(ev)
  )

myplot.choiceprob.diff.agg = ggplot(
  data = choicedata.diff.group,
  aes(
    x = ev,
    y = prob_late.mean,
    ymin = prob_late.mean - prob_late.sem,
    ymax = prob_late.mean + prob_late.sem
  )
) +
  geom_line() +
  geom_point() +
  geom_errorbar(width = 0.5) +
  xlab('Later amount ($)') +
  ylab('Difference in frequency of \nlater option (high-low vol.)') + theme(legend.position = 'bottom', legend.title = element_blank())

print(myplot.choiceprob.diff.agg)


#  x-y

choicedata.group.wide = dcast(choicedata.group,
                              ev.cut + Context_order ~ Context,
                              value.var = c("prob_late.mean"))
choicedata.group.wide = choicedata.group.wide %>% mutate(highprob = `High volatility`,
                                                         lowprob = `Low volatility`)
choicedata.group.wide.sem = dcast(choicedata.group,
                                  ev.cut + Context_order ~ Context,
                                  value.var = c("prob_late.sem"))

choicedata.group.wide.sem = choicedata.group.wide.sem %>% mutate(highsem = `High volatility`,
                                                                 lowsem = `Low volatility`)

choicedata.group.wide = merge(
  choicedata.group.wide,
  choicedata.group.wide.sem,
  by = c("ev.cut", "Context_order"),
  suffix = c(".x", ".y")
)

myplot.choiceprob.wide = ggplot(
  data = choicedata.group.wide,
  aes(
    x = lowprob,
    y = highprob,
    ymin = highprob - highsem,
    ymax = highprob + highsem,
    xmin = lowprob - lowsem,
    xmax = lowprob + lowsem,
    color = Context_order,
    group = Context_order
  )
) +
  geom_line() +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  geom_errorbar() +
  geom_errorbarh() +
  xlab("Probability of later option\n(high volatility context)") +
  ylab("Probability of later option\n(low volatility context)")
#xlab('Later amount ($)') +
#ylab('Frequency of later option') + theme(legend.position = 'bottom', legend.title = element_blank())
print(myplot.choiceprob.wide)
