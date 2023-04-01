

mypars.rt = c('key.rt') #, 'bid.rt.max', 'bid.rt.min',  'bid.rt.second', 'bid.rt.first')

ratingdata.median = ratingdata %>%
  group_by(subjID, Context, context_order, Context_order, Group) %>%
  summarise(key.rt.median = median(key.rt) / 1000)

myplot = ggplot(data = ratingdata, aes(x = Context, y = key.rt / 1000, fill = Context)) +
  geom_violin() +
  xlab('Context') +
  ylab('Response time') + facet_grid(. ~ Group)
print(myplot)
# do intermediate step


ratingdata.median.ev = ratingdata %>%
  group_by(subjID,
           Context,
           context_order,
           Context_order,
           Group,
           amount_later,
           ev) %>%
  summarise(key.rt = median(key.rt) / 1000)

ratingdata.group = ratingdata.median.ev %>%
  group_by(Context, context_order, Context_order, Group, amount_later, ev) %>%
  summarise(
    key.rt.mean = mean(key.rt),
    key.rt.sem = sem(key.rt),
    ev = mean(ev)
  )

myplot.key.rt = ggplot(
  data = ratingdata.group,
  aes(
    x = ev,
    y = key.rt.mean,
    ymin = key.rt.mean - key.rt.sem,
    ymax = key.rt.mean + key.rt.sem,
    color = Context
  )
) +
  geom_line() +
  geom_point() +
  geom_errorbar() +
  xlab('Later amount ($)') +
  ylab('Response time (s)') + facet_grid(. ~ Group)

print(myplot.key.rt)


# myplot = ggplot(data = ratingdata, aes(x = as.factor( amount_later_centered), y = bid.rt.second/1000, fill = Context)) +
#   geom_boxplot() +
#   xlab('Amount') +
#   ylab('Response time') + facet_grid( Group ~ . )
# print(myplot)

myplot = ggplot(data = ratingdata.median, aes(x = Context, y = key.rt.median, group = subjID)) +
  geom_line() +
  geom_point() +
  xlab('Amount') +
  ylab('Response time') + facet_grid(. ~ Group)
print(myplot)

model.rt = fitmodel(
  "key.rt ~ Group*Context + (1 | subjID)",
  ratingdata,
  c(
    "_RatingRT_context_" = "ContextLow volatility",
    "_RatingRT_groupxcontext_" = "GroupLow first:ContextLow volatility"
  )
)
print(summary(model.rt))
