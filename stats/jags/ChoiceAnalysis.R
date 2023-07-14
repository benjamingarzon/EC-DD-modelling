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
  ylab('Frequency of later option') + theme(legend.position = 'bottom', legend.title = element_blank())

myplot.choiceprob.order = myplot.choiceprob + facet_grid(. ~ Context_order)
myplot.choiceprob.group = myplot.choiceprob + facet_grid(. ~ Group)
print(myplot.choiceprob.order)
print(myplot.choiceprob.group)

model = fitmodel(
  "choice ~ Group*Context*amount_later_centered + (1|subjID)",
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
print(summary(model))

cc.main = fixef(model)["GroupLow vol. first:ContextLow volatility"]
cc.inter = fixef(model)["GroupLow vol. first:ContextLow volatility:amount_later_centered"]
mm = model.matrix(model)
z = cc.main*mm[,"GroupLow vol. first:ContextLow volatility"]
+ cc.inter*mm[,"GroupLow vol. first:ContextLow volatility:amount_later_centered"]
choicedata$choice.pred = choicedata$choice - sigmoid(z)

choicedata.median.agg = choicedata %>%
  group_by(subjID,
           Context,
           context,
           ev.cut) %>%
  dplyr::summarise(
    ev = mean(ev),
    prob_late = mean(choice.pred)
  )

choicedata.group.agg = choicedata.median.agg %>%
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
  ylab('Frequency of later option') + theme(legend.position = 'bottom', legend.title = element_blank())
print(myplot.choiceprob.agg)

choicedata.group.wide = dcast(choicedata.group, ev.cut + Context_order ~ Context,  value.var = c("prob_late.mean"))
choicedata.group.wide = choicedata.group.wide %>% mutate(highprob = `High volatility`,
                                                         lowprob = `Low volatility`) 
choicedata.group.wide.sem = dcast(choicedata.group, ev.cut + Context_order ~ Context,  value.var = c("prob_late.sem"))

choicedata.group.wide.sem = choicedata.group.wide.sem %>% mutate(highsem = `High volatility`,
                                                         lowsem = `Low volatility`) 

choicedata.group.wide = merge(choicedata.group.wide, choicedata.group.wide.sem, by = c("ev.cut", "Context_order"), suffix = c(".x", ".y"))

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
