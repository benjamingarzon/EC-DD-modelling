

breaks = seq(0.5, 8.5, 1)
choicedata = merge(
  choicedata.common,
  allIndPars %>% filter(subjID %in% include_subjects),
  by = c(
    'subjID',
    'Context',
    'context',
    'Context_order',
    'background_col',
    'context_order',
    'Group'
  )
)
choicedata = choicedata %>% mutate(ev = amount_later, context_num = as.numeric(substr(context, 2, 2))) %>% mutate(ev.cut = cut(ev, breaks))

myplot.RT = ggplot(data = choicedata, aes(
  x = jitter(ev),
  y = log_rt,
  color = Context,
  group = subjID
)) + geom_line(linewidth = 0.2) +
  geom_point(size = 0.2) +
  xlab('Later amount ($)') +
  ylab('Response time (s)') + facet_grid(Group ~ Choice)

print(myplot.RT)

choicedata.median = choicedata %>%
  group_by(subjID,
           Context,
           context,
           Context_order,
           context_order,
           Group,
           ev.cut) %>%
  dplyr::summarise(
    rt.median = median(rt),
    ev = mean(ev),
    prob_late = mean(choice)
  )

myplot.RT = ggplot(data = choicedata.median, aes(
  x = ev,
  y = log(rt.median),
  color = Context,
  group = subjID
)) + geom_line(linewidth = 0.2) +
  geom_point(size = 0.2) +
  xlab('Later amount ($)') +
  ylab('Response time (s)') + facet_grid(Group ~ Context_order)

print(myplot.RT)

choicedata.median.sub = choicedata %>%
  group_by(subjID, Context, context, Context_order, context_order, Group) %>%
  dplyr::summarise(rt.median = median(rt))

myplot.RT = ggplot(data = choicedata.median.sub, aes(x = Context,
                                                     y = rt.median,
                                                     fill = Context)) +
  geom_violin() +
  xlab('Context') +
  ylab('Response time (s)') + facet_grid(Group ~ .)

print(myplot.RT)

myplot.RT = ggplot(data = choicedata.median.sub, aes(x = Context,
                                                     y = rt.median,
                                                     group = subjID)) +
  geom_point() +
  geom_line() +
  xlab('Context') +
  ylab('Response time (s)') + facet_grid(. ~ Group)
print(myplot.RT)

choicedata.group = choicedata.median %>%
  group_by(Context, Context_order, ev.cut, Group) %>%
  dplyr::summarise(
    rt.mean = mean(rt.median),
    rt.sem = sem(rt.median),
    prob_late.mean = mean(prob_late),
    prob_late.sem = sem(prob_late),
    ev = mean(ev)
  )

myplot.RT = ggplot(
  data = choicedata.group,
  aes(
    x = ev,
    y = rt.mean,
    ymin = rt.mean - rt.sem,
    ymax = rt.mean + rt.sem,
    color = Context,
    group = Context
  )
) +
  geom_line() +
  geom_point() +
  geom_errorbar(width = 0.5) +
  xlab('Later amount ($)') +
  ylab('Response time (s)') + facet_grid(. ~ Context_order)

print(myplot.RT)

choicedata.group.choice = choicedata %>%
  group_by(subjID, Context, Context_order, ev.cut, Choice, Group) %>%
  dplyr::summarise(
    rt.median = median(rt),
    ev = mean(ev),
    prob_late = mean(choice)
  )

choicedata.group.choice = choicedata.group.choice %>%
  group_by(Context, ev.cut, Context_order, Choice, Group) %>%
  dplyr::summarise(
    rt.mean = mean(rt.median),
    rt.sem = sem(rt.median),
    prob_late.mean = mean(prob_late),
    prob_late.sem = sem(prob_late),
    ev = mean(ev)
  )

# separate by choice
myplot.RT.choice = ggplot(
  data = choicedata.group.choice,
  aes(
    x = ev,
    y = rt.mean,
    ymin = rt.mean - rt.sem,
    ymax = rt.mean + rt.sem,
    color = Context,
    group = Context
  )
) +
  geom_line() +
  geom_point() +
  geom_errorbar(width = 0.5) +
  xlab('Later amount ($)') +
  ylab('Response time (s)') + theme(legend.position = 'bottom', legend.title = element_blank())

myplot.RT.choice.order = myplot.RT.choice + facet_grid(Context_order ~ Choice)
myplot.RT.choice.group = myplot.RT.choice + facet_grid(Group ~ Choice)
print(myplot.RT.choice.order)
print(myplot.RT.choice.group)

# myplot.RT.choice = ggplot(data = choicedata, aes(
#   x = cut(ev, breaks),
#   y = log_rt,
#   fill = Context
# )) +
#   geom_violin() +
#   xlab('Later amount ($)') +
#   ylab('Response time (s)') +  theme(legend.position = 'bottom', legend.title = element_blank()) #facet_grid(  . ~ Context_order)
#
# print(myplot.RT.choice)


myplot.RT.choice.hist = ggplot(
  data = subset(choicedata, amount_later > 2.5 &
                  amount_later < 7.5),
  aes(x = Group, y = rt, fill = Context)
) +
  geom_violin() +
  xlab('Later amount ($)') +
  ylab('Response time (s)') +  theme(legend.position = 'bottom', legend.title = element_blank()) #facet_grid(  . ~ Context_order)

print(myplot.RT.choice.hist)

myplot = ggplot(data = choicedata, aes(
  x = cut(ev, breaks),
  y = log_rt,
  fill = context
)) +
  geom_violin() +
  xlab('Subjective value of later amount ($)') +
  ylab('Response time (s)') + ylim(-5, 5)

#print(myplot)
choicedata$amount_later_centered.2 = choicedata$amount_later_centered ^ 2

# without order correction
model = fitmodel(
  #  "log_rt ~ Group*Context + (1|subjID)", 
  "log_rt ~ Context + (1 + Context|subjID)", 
  choicedata,
  c("_RT_contextNOgroup_" = "ContextLow volatility")
)


# with order correction
#source("../analysis_funcs.R")
model = fitmodel(
#  "log_rt ~ Group*Context + (1|subjID)", 
  "log_rt ~ Group*Context + (1 + Group*Context|subjID)", 
  choicedata,
  c("_RT_context_" = "ContextLow volatility",
    "_RT_groupxcontext_" = "GroupLow vol. first:ContextLow volatility")
)

#cc = fixef(model)["GroupLow vol. first:ContextLow volatility"]
cc = fixef(model)["GroupLowvol.first:ContextLowvolatility", "Estimate"]
mm <- model.matrix(as.formula("log_rt ~ Group*Context"), data = choicedata)

#rt.fixef = coef(summary(model))
rt.fixef = fixef(model) 
choicedata$rt.pred = exp(log(choicedata$rt) - cc * mm[, "GroupLow vol. first:ContextLow volatility"])



model = fitmodel(
  "log_rt ~ Group*Context + Choice*(amount_later_centered + amount_later_centered.2) + (1+Group*Context + Choice*(amount_later_centered + amount_later_centered.2)|subjID)",
  choicedata,
  c(
    "_RT_contextCORchoicexamount_" = "ContextLow volatility",
    "_RT_groupxcontextCORchoicexamount_" = "GroupLow vol. first:ContextLow volatility"
  ),
  
)

rt.cor.fixef = fixef(model) #coef(summary(model))

choicedata.median = choicedata %>%
  group_by(subjID,
           Context,
           context,
           Choice,
           ev.cut) %>%
  dplyr::summarise(
    rt.median = median(rt.pred),
    ev = mean(ev),
    prob_late = mean(choice)
  )

choicedata.group.agg = choicedata.median %>%
  group_by(Context, ev.cut, Choice) %>%
  dplyr::summarise(
    rt.mean = mean(rt.median),
    rt.sem = sem(rt.median),
    prob_late.mean = mean(prob_late),
    prob_late.sem = sem(prob_late),
    ev = mean(ev)
  )

myplot.RT.choice.agg = ggplot(
  data = choicedata.group.agg,
  aes(
    x = ev,
    y = rt.mean,
    ymin = rt.mean - rt.sem,
    ymax = rt.mean + rt.sem,
    color = Context,
    group = Context
  )
) +
  geom_line() +
  geom_point() +
  geom_errorbar(width = 0.5) +
  facet_grid(. ~ Choice) +
  xlab('Later amount ($)') +
  ylab('Response time (s)')

print(myplot.RT.choice.agg)

choicedata.trial.all = merge(
  choicedata.all,
  allIndPars %>% filter(subjID %in% include_subjects),
  by = c('subjID',
         'context',
         'background_col',
         'context_order'),
  suffixes = c('', '.pars')
)

#load(file.path(WDD, "processed_data.RData"))

choicedata.trial.all = choicedata.trial.all %>% mutate(ev = amount_later, context_num = as.numeric(substr(context, 2, 2))) %>% mutate(ev.cut = cut(ev, breaks))

choicedata.trial = choicedata.trial.all %>% filter(iscommon == T) %>%
  group_by(index,
           Context,
           context,
           Context_order,
           context_order,
           #         ev.cut,
           Group) %>%
  dplyr::summarise(
    rt.median = median(rt),
    rt.sem = sem(rt),
    #  ev = mean(ev),
    prob_late = mean(choice)
  )


myplot.trial.RT = ggplot(
  data = choicedata.trial,
  aes(
    x = index,
    y = rt.median,
    color = Group,
    group = Group,
    ymin = rt.median - rt.sem,
    ymax = rt.median + rt.sem
  )
) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 0.5) +
  geom_errorbar() +
  xlim(420, 460) +
  xlab('Trial') +
  ylab('Response time (s)') # + facet_grid(Group ~ .)

print(myplot.trial.RT)

myplot.trial.RT = ggplot(
  data = choicedata.trial,
  aes(
    x = index,
    y = rt.median,
    color = Group,
    group = Group,
    ymin = rt.median - rt.sem,
    ymax = rt.median + rt.sem
  )
) +
  geom_line(linewidth = 0.3, alpha = 0.5) +
  geom_point(size = 0.3, alpha = 0.5) +
  geom_errorbar(size = 0.3, alpha = 0.5) +
  xlab('Trial') +
  scale_color_manual(values = c("green", "brown")) +
  ylab('Response time (s)')
print(myplot.trial.RT)

choicedata.trans = choicedata.trial.all %>% filter(index == 441) # & iscommon == T)
t.test(log_rt ~ Group, choicedata.trans)
