ratingdata.sub$bid.diff = ratingdata.sub$bid.high - ratingdata.sub$bid.low

ratingdata.sub$bid.rt.max = apply(ratingdata.sub[c('bid.high.rt', 'bid.low.rt')], 1, max)
ratingdata.sub$bid.rt.min = apply(ratingdata.sub[c('bid.high.rt', 'bid.low.rt')], 1, min)

ratingdata.sub = ratingdata.sub %>%
  mutate(bid.rt.second = bid.rt.max - bid.rt.min,
         bid.rt.first = bid.rt.min)

ratingdata.sub = ratingdata.sub %>% group_by(subjID) %>% mutate(bid.diff.residuals = lm(bid.diff ~ amount_later)$residuals)

#remove outliers
#ratingdata.sub = ratingdata.sub %>% group_by(amount_later) %>% mutate(bid.diff.outlier = markoutliersIQR(log(bid.diff)),
#                                                                      bid.rt.outlier = markoutliersIQR(log(bid.rt.max)))


#remove outliers
ratingdata.sub = ratingdata.sub %>% group_by(amount_later) %>% mutate(
  bid.diff.outlier = markoutliersIQR(bid.diff.residuals),# markoutliersIQR(log(bid.diff)),
  bid.rt.outlier = markoutliersIQR(log(bid.rt.max)))


ratingdata.sub.clean = subset(ratingdata.sub, bid.rt.outlier == F) #& bid.diff.outlier == F 
                                


ratingdata = merge(
  ratingdata.sub.clean,
  allIndPars %>% filter(subjID %in% include_subjects),
  by = c(
    'subjID',
    'context',
    'context_num',
    'Context_order',
    'context_order',
    'background_col',
    'Context',
    'Group'
  )
)
ratingdata = subset(ratingdata, subjID %in% include_subjects)
ratingdata = ratingdata %>% mutate(ev = amount_later, bid.diff = (bid.high - bid.low))
# b.drift.intercept.p + b.drift.amount.p*amount_later_centered

myplot.range.all = ggplot(data = ratingdata, aes(
  x = ev,
  y = bid.diff,
  color = Context,
  group = subjID
)) +
  geom_line(linewidth = 0.5, alpha = 0.4) +
  geom_point(size = 0.5, alpha = 0.4) +
  xlab('Subjective value of later amount ($)') +
  ylab('Willingness-to-pay range ($)') + facet_grid(Group ~ Context)
print(myplot.range.all)

ratingdata.mean = ratingdata %>%
  group_by(subjID, Context, Context_order, amount_later, Group) %>%
  summarise(bid.diff = mean(bid.diff), ev = mean(ev))

ratingdata.group = ratingdata.mean %>%
  group_by(Context, Context_order, amount_later, Group) %>%
  summarise(
    bid.diff.mean = mean(bid.diff),
    bid.diff.sem = sem(bid.diff),
    ev = mean(ev)
  )

myplot.range = ggplot(
  data = ratingdata.group,
  aes(
    x = ev,
    y = bid.diff.mean,
    ymin = bid.diff.mean - bid.diff.sem,
    ymax = bid.diff.mean + bid.diff.sem,
    group = Context,
    col = Context
  )
) +
  geom_line() +
  geom_point() +
  geom_errorbar(width = 0.5) +
  xlab('Later amount ($)') +
  ylab('Willingness-to-pay range ($)')

myplot.range.group = myplot.range + facet_grid(. ~ Group)
myplot.range.order = myplot.range + facet_grid(. ~ Context_order)

print(myplot.range.group)
print(myplot.range.order)

# Equation 4
model = fitmodel(
#  "bid.diff ~ Group*amount_later_centered + (1|subjID)",
  "bid.diff ~ Group*amount_later_centered + (1 + amount_later_centered|subjID)",
  ratingdata,
  c(
    "GroupLow var. first:amount_later_centered",
    "GroupLow var. first"
  )
)
print(summary(model))

## ev
#model = fitmodel(
#  "bid.diff ~ Group*Context*ev + (1|subjID)",
#  ratingdata,
#  c(
#    "_Rating_context_" = "ContextLow variance",
#    "_Rating_groupxcontext_" = "GroupLow var. first:ContextLow variance",
#    "_Rating_groupxev_" = "GroupLow var. first:ev",
#    "_Rating_contextxev_" = "ContextLow variance:ev",
#    "_Rating_groupxcontextxev_" = "GroupLow var. first:ContextLow variance:ev"
#  )
#)

#cc.main = get_fixef(model)["GroupLowvar.first:ContextLowvariance", "Estimate"]
#cc.inter = get_fixef(model)["GroupLowvar.first:ContextLowvariance:ev", "Estimate"]

#mm <- model.matrix(as.formula("bid.diff ~ Group*Context*ev"), data = ratingdata)

#ratingdata$bid.diff.pred = ratingdata$bid.diff - cc.main * mm[, "GroupLowvar.first:ContextLowvariance"]  -
#  cc.inter * mm[, "GroupLowvar.first:ContextLowvariance:amount_later_centered"]
#colnames(mm) <- gsub(" ", "", colnames(mm))

model = fitmodel(
  "bid.diff ~ Group*Context*amount_later_centered + (1 + Group*Context*amount_later_centered|subjID)",
  ratingdata,
  c(
    "_Rating_context_" = "ContextLow variance",
    "_Rating_groupxcontext_" = "GroupLow var. first:ContextLow variance",
    "_Rating_groupxamount_later_centered_" = "GroupLow var. first:amount_later_centered",
    "_Rating_contextxamount_later_centered_" = "ContextLow variance:amount_later_centered",
    "_Rating_groupxcontextxamount_later_centered_" = "GroupLow var. first:ContextLow variance:amount_later_centered"
  )
)

print(summary(model))

model.rating = model

#cc.main = fixef(model)["GroupLow var. first:ContextLow variance"]
#cc.inter = fixef(model)["GroupLow var. first:ContextLow variance:ev"]
#mm = model.matrix(model)
#ratingdata$bid.diff.pred = ratingdata$bid.diff - cc.main * mm[, "GroupLow var. first:ContextLow variance"]  -
#  cc.inter * mm[, "GroupLow var. first:ContextLow variance:ev"]

cc.main = get_fixef(model)["GroupLowvar.first:ContextLowvariance", "Estimate"]
cc.inter = get_fixef(model)["GroupLowvar.first:ContextLowvariance:amount_later_centered", "Estimate"]

mm <- model.matrix(as.formula("bid.diff ~ Group*Context*amount_later_centered"), data = ratingdata)
colnames(mm) <- gsub(" ", "", colnames(mm))

ratingdata$bid.diff.pred = ratingdata$bid.diff - cc.main * mm[, "GroupLowvar.first:ContextLowvariance"]  -
  cc.inter * mm[, "GroupLowvar.first:ContextLowvariance:amount_later_centered"]

ratingdata.mean.agg = ratingdata %>%
  group_by(subjID, Context, amount_later) %>%
  summarise(bid.diff = mean(bid.diff.pred), ev = mean(ev))

ratingdata.group.agg = ratingdata.mean.agg %>%
  group_by(Context, amount_later) %>%
  summarise(
    bid.diff.mean = mean(bid.diff),
    bid.diff.sem = sem(bid.diff),
    ev = mean(ev)
  )

myplot.range.agg = ggplot(
  data = ratingdata.group.agg,
  aes(
    x = ev,
    y = bid.diff.mean,
    ymin = bid.diff.mean - bid.diff.sem,
    ymax = bid.diff.mean + bid.diff.sem,
    group = Context,
    col = Context
  )
) +
  geom_line() +
  geom_point() +
  geom_errorbar(width = 0.5) +
  xlab('Later amount ($)') +
  ylab('Willingness-to-pay range ($)')

print(myplot.range.agg)