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
