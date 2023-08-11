

mypars.rt = c('key.rt') #, 'bid.rt.max', 'bid.rt.min',  'bid.rt.second', 'bid.rt.first')myplot = ggplot(data = ratingdata, aes(x = bid.rt.max, y = key.rt, fill = Context)) +

myplot = ggplot(data = ratingdata, aes(x = bid.high.rt, y = bid.low.rt, col = Context)) +
  geom_point() +
  xlab('Bid time') +
  ylab('Response time') + facet_grid(Context ~ Group)
print(myplot)

ratingdata$key.rt.orig = ratingdata$key.rt
ratingdata$key.rt = ratingdata$bid.rt.max 

ratingdata.median = ratingdata %>%
  group_by(subjID, Context, context_order, Context_order, Group) %>%
  summarise(key.rt.median = median(key.rt) )

myplot = ggplot(data = ratingdata, aes(x = Context, y = key.rt, fill = Context)) +
  geom_violin() +
  xlab('Context') +
  ylab('Response time') + facet_grid(. ~ Group)
print(myplot)

myplot = ggplot(data = ratingdata, aes(x = amount_later, y = key.rt , col = Context, group = subjID)) +
  geom_point() +
  geom_line() +
  xlab('Later amount') +
  ylab('Response time') + facet_grid(Context ~ Group)
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
  summarise(key.rt = median(key.rt))

ratingdata.group = ratingdata.median.ev %>%
  group_by(Context, context_order, Context_order, Group, amount_later, ev) %>%
  summarise(
    key.rt.mean = mean(key.rt, na.rm=T),
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
  ylab('Response time (s)') 

myplot.key.rt.group = myplot.key.rt + facet_grid(. ~ Group)
myplot.key.rt.context = myplot.key.rt + facet_grid(. ~ Context)
print(myplot.key.rt.group)


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
  "log(key.rt) ~ Group*Context + (1 | subjID)",
  ratingdata,
  c(
    "_RatingRT_context_" = "ContextLow volatility",
    "_RatingRT_groupxcontext_" = "GroupLow vol. first:ContextLow volatility"
  )
)
print(summary(model.rt))

cc = fixef(model.rt)["GroupLow vol. first:ContextLow volatility"]
mm = model.matrix(model.rt)

ratingdata$key.rt.pred = exp(log(ratingdata$key.rt) - cc*mm[,"GroupLow vol. first:ContextLow volatility"])

ratingdata.median.ev = ratingdata %>%
  group_by(subjID,
           Context,
           amount_later,
           ev) %>%
  summarise(key.rt = median(key.rt.pred, na.rm=T))

ratingdata.group.agg = ratingdata.median.ev %>%
  group_by(Context, amount_later, ev) %>%
  summarise(
    key.rt.mean = mean(key.rt, na.rm=T),
    key.rt.sem = sem(key.rt),
    ev = mean(ev)
  )

myplot.key.rt.agg = ggplot(
  data = ratingdata.group.agg,
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
  ylab('Response time (s)') 

print(myplot.key.rt.agg)
