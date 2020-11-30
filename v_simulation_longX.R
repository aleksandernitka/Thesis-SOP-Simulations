input = list()
input$sorQp1 = .8
input$sorXp1 = .6

#input$sor2Qp1 = .1
#input$sor2Xp1 = .05

input$sorQon1 = c(10,40)
input$sorXon1 = c(10,40)

input$sor2Qon1 = c(10,40)
input$sor2Xon1 = c(10,100)

input$sordur = 100
input$sorpd1 = 0.1
input$sorpd2 = 0.02

input$Lp = .5
input$Lm = input$Lp/5

sor = matrix(nrow = input$sordur, ncol = 32)

colnames(sor) = c('p1Q', 'p1X',
                  'pd1', 'pd2', 
                  'pIQ', 'pA1Q', 'pA2Q',
                  'pA1Qgain', 'pA1Qloss','pA2Qgain',
                  'pA2Qloss','pIQgain','pIQloss',
                  'pA1Qdelta', 'pA2Qdelta', 'pIQdelta',

                  'pIX', 'pA1X', 'pA2X',
                  'pA1Xgain', 'pA1Xloss','pA2Xgain',
                  'pA2Xloss','pIXgain','pIXloss',
                  'pA1Xdelta', 'pA2Xdelta', 'pIXdelta',
                  'Ve', 'Vi', 'Vdelta', 'V'
                  )

sor[,'p1Q'] = 0 # set as 0 to init
sor[,'p1X'] = 0


sor[input$sorQon1[1]:input$sorQon[2],'p1Q'] = input$sorQp1
sor[input$sorXon1[1]:input$sorXon[2],'p1X'] = input$sorXp1

sor[,'pd1'] = input$sorpd1     # set decay rate
sor[,'pd2'] = input$sorpd2     # set decay rate

sor[,'pIQ'] = 1                 # Make I numeric
sor[,'pIX'] = 1   

sor2 = sor

sor2[input$sor2Qon1[1]:input$sor2Qon1[2],'p1Q'] = input$sorQp1
sor2[input$sor2Xon1[1]:input$sor2Xon1[2],'p1X'] = input$sorXp1

# run moment by moment simulation
for (i in 1:nrow(sor)){
    if (i == 1){
        
        #SOR
        sor[i,'pIQ'] = 1 - sor[i,'p1Q'] # This will be 1 as long as p1 = 0
        
        # Deltas calculate
        sor[i,'pA1Qgain'] = sor[i,'p1Q']
        sor[i,'pA1Qloss'] = sor[i,'pA1Qgain'] * sor[i,'pd1']
        sor[i,'pA1Qdelta'] = sor[i,'pA1Qgain'] - sor[i,'pA1Qloss']
        
        sor[i,'pA2Qgain'] = sor[i,'pA1Qloss']
        sor[i,'pA2Qloss'] = sor[i,'pA2Qgain'] * sor[i,'pd2']
        sor[i,'pA2Qdelta'] = sor[i,'pA2Qgain'] - sor[i,'pA2Qloss']

        sor[i,'pIQgain'] = sor[i,'pA2Qloss']
        sor[i,'pIQloss'] = sor[i,'p1Q']
        sor[i,'pIQdelta'] = sor[i,'pIQgain'] - sor[i,'pIQloss']

        # Calculate proportions in each state
        sor[i,'pA1Q'] = sor[i,'pA1Qdelta']
        sor[i,'pA2Q'] = sor[i,'pA2Qdelta']
        sor[i,'pIQ'] = sor[i,'pIQ'] + sor[i,'pIQgain']

        # X
        sor[i,'pIX'] = 1 - sor[i,'p1X'] # This will be 1 as long as p1 = 0
        
        # Deltas calculate
        sor[i,'pA1Xgain'] = sor[i,'p1X']
        sor[i,'pA1Xloss'] = sor[i,'pA1Xgain'] * sor[i,'pd1']
        sor[i,'pA1Xdelta'] = sor[i,'pA1Xgain'] - sor[i,'pA1Xloss']
        
        sor[i,'pA2Xgain'] = sor[i,'pA1Xloss']
        sor[i,'pA2Xloss'] = sor[i,'pA2Xgain'] * sor[i,'pd2']
        sor[i,'pA2Xdelta'] = sor[i,'pA2Xgain'] - sor[i,'pA2Xloss']
        
        sor[i,'pIXgain'] = sor[i,'pA2Xloss']
        sor[i,'pIXloss'] = sor[i,'p1X']
        sor[i,'pIXdelta'] = sor[i,'pIXgain'] - sor[i,'pIXloss']
        
        # Calculate proportions in each state
        sor[i,'pA1X'] = sor[i,'pA1Xdelta']
        sor[i,'pA2X'] = sor[i,'pA2Xdelta']
        sor[i,'pIX'] = sor[i,'pIX'] + sor[i,'pIXgain']
        
        # Calculate V
        sor[i, 'Ve'] = sor[i, 'pA1X'] * sor[i, 'pA1Q'] * input$Lp
        sor[i, 'Vi'] = sor[i, 'pA1X'] * sor[i, 'pA2Q'] * input$Lm
        sor[i, 'Vdelta'] = sor[i, 'Ve'] - sor[i, 'Vi']
        sor[i, 'V'] = sum(sor[1:i, 'Vdelta'])
        
        # SOR 2
        sor2[i,'pIQ'] = 1 - sor2[i,'p1Q'] # This will be 1 as long as p1 = 0
        
        # Deltas calculate
        sor2[i,'pA1Qgain'] = sor2[i,'p1Q']
        sor2[i,'pA1Qloss'] = sor2[i,'pA1Qgain'] * sor2[i,'pd1']
        sor2[i,'pA1Qdelta'] = sor2[i,'pA1Qgain'] - sor2[i,'pA1Qloss']
        
        sor2[i,'pA2Qgain'] = sor2[i,'pA1Qloss']
        sor2[i,'pA2Qloss'] = sor2[i,'pA2Qgain'] * sor2[i,'pd2']
        sor2[i,'pA2Qdelta'] = sor2[i,'pA2Qgain'] - sor2[i,'pA2Qloss']
        
        sor2[i,'pIQgain'] = sor2[i,'pA2Qloss']
        sor2[i,'pIQloss'] = sor2[i,'p1Q']
        sor2[i,'pIQdelta'] = sor2[i,'pIQgain'] - sor2[i,'pIQloss']
        
        # Calculate proportions in each state
        sor2[i,'pA1Q'] = sor2[i,'pA1Qdelta']
        sor2[i,'pA2Q'] = sor2[i,'pA2Qdelta']
        sor2[i,'pIQ'] = sor2[i,'pIQ'] + sor2[i,'pIQgain']
        
        # X
        sor2[i,'pIX'] = 1 - sor2[i,'p1X'] # This will be 1 as long as p1 = 0
        
        # Deltas calculate
        sor2[i,'pA1Xgain'] = sor2[i,'p1X']
        sor2[i,'pA1Xloss'] = sor2[i,'pA1Xgain'] * sor2[i,'pd1']
        sor2[i,'pA1Xdelta'] = sor2[i,'pA1Xgain'] - sor2[i,'pA1Xloss']
        
        sor2[i,'pA2Xgain'] = sor2[i,'pA1Xloss']
        sor2[i,'pA2Xloss'] = sor2[i,'pA2Xgain'] * sor2[i,'pd2']
        sor2[i,'pA2Xdelta'] = sor2[i,'pA2Xgain'] - sor2[i,'pA2Xloss']
        
        sor2[i,'pIXgain'] = sor2[i,'pA2Xloss']
        sor2[i,'pIXloss'] = sor2[i,'p1X']
        sor2[i,'pIXdelta'] = sor2[i,'pIXgain'] - sor2[i,'pIXloss']
        
        # Calculate proportions in each state
        sor2[i,'pA1X'] = sor2[i,'pA1Xdelta']
        sor2[i,'pA2X'] = sor2[i,'pA2Xdelta']
        sor2[i,'pIX'] = sor2[i,'pIX'] + sor2[i,'pIXgain']
        
        # Calculate V
        sor2[i, 'Ve'] = sor2[i, 'pA1X'] * sor2[i, 'pA1Q'] * input$Lp
        sor2[i, 'Vi'] = sor2[i, 'pA1X'] * sor2[i, 'pA2Q'] * input$Lm
        sor2[i, 'Vdelta'] = sor2[i, 'Ve'] - sor2[i, 'Vi']
        sor2[i, 'V'] = sum(sor2[1:i, 'Vdelta'])

        
    } else {
        
        #SOR
        # Q
        
        sor[i,'pIQ'] = sor[i-1,'pIQ']
        
        sor[i,'pA1Qgain'] = sor[i,'pIQ'] * sor[i,'p1Q'] # how mauch is activated from I
        
        sor[i,'pA1Q'] = sor[i-1,'pA1Q'] + sor[i,'pA1Qgain'] # how much is there in total with carried over
        
        sor[i,'pA1Qloss'] = sor[i,'pA1Q'] * sor[i,'pd1'] # how much decays to A2
        
        sor[i,'pA1Q'] = sor[i,'pA1Q'] - sor[i,'pA1Qloss'] # how much is left after the decay
        
        sor[i,'pA1Qdelta'] = sor[i,'pA1Qgain'] - sor[i,'pA1Qloss']
        
        sor[i,'pA2Qgain'] = sor[i,'pA1Qloss'] # how much is gained from A1->A2 decay
        
        sor[i,'pA2Q'] = sor[i-1,'pA2Q'] + sor[i,'pA2Qgain'] # how much is there with carried over
        
        sor[i,'pA2Qloss'] = sor[i,'pA2Q'] * sor[i,'pd2'] # how much decays A2->I
        
        sor[i,'pA2Q'] = sor[i,'pA2Q'] - sor[i,'pA2Qloss'] # how much is left after A2->I decay
        
        sor[i,'pA2Qdelta'] = sor[i,'pA2Qgain'] - sor[i,'pA2Qloss']
        
        sor[i,'pIQgain'] = sor[i,'pA2Qloss'] # how much decayed from A2
        
        sor[i,'pIQloss'] = sor[i,'pA1Qgain']
        
        sor[i,'pIQ'] = sor[i,'pIQ'] + sor[i,'pIQgain'] - sor[i,'pIQloss']
        
        sor[i,'pIQdelta'] = sor[i,'pIQgain'] - sor[i,'pIQloss']
        
        # X
        
        sor[i,'pIX'] = sor[i-1,'pIX']
        
        sor[i,'pA1Xgain'] = sor[i,'pIX'] * sor[i,'p1X'] # how mauch is activated from I
        
        sor[i,'pA1X'] = sor[i-1,'pA1X'] + sor[i,'pA1Xgain'] # how much is there in total with carried over
        
        sor[i,'pA1Xloss'] = sor[i,'pA1X'] * sor[i,'pd1'] # how much decays to A2
        
        sor[i,'pA1X'] = sor[i,'pA1X'] - sor[i,'pA1Xloss'] # how much is left after the decay
        
        sor[i,'pA1Xdelta'] = sor[i,'pA1Xgain'] - sor[i,'pA1Xloss']
        
        sor[i,'pA2Xgain'] = sor[i,'pA1Xloss'] # how much is gained from A1->A2 decay
        
        sor[i,'pA2X'] = sor[i-1,'pA2X'] + sor[i,'pA2Xgain'] # how much is there with carried over
        
        sor[i,'pA2Xloss'] = sor[i,'pA2X'] * sor[i,'pd2'] # how much decays A2->I
        
        sor[i,'pA2X'] = sor[i,'pA2X'] - sor[i,'pA2Xloss'] # how much is left after A2->I decay
        
        sor[i,'pA2Xdelta'] = sor[i,'pA2Xgain'] - sor[i,'pA2Xloss']
        
        sor[i,'pIXgain'] = sor[i,'pA2Xloss'] # how much decayed from A2
        
        sor[i,'pIXloss'] = sor[i,'pA1Xgain']
        
        sor[i,'pIX'] = sor[i,'pIX'] + sor[i,'pIXgain'] - sor[i,'pIXloss']
        
        sor[i,'pIXdelta'] = sor[i,'pIXgain'] - sor[i,'pIXloss']
        
        # Calculate V
        sor[i, 'Ve'] = sor[i, 'pA1X'] * sor[i, 'pA1Q'] * input$Lp
        sor[i, 'Vi'] = sor[i, 'pA1X'] * sor[i, 'pA2Q'] * input$Lm
        sor[i, 'Vdelta'] = sor[i, 'Ve'] - sor[i, 'Vi']
        sor[i, 'V'] = sum(sor[1:i, 'Vdelta'])
        
        
        #SOR 2
        # Q
        
        sor2[i,'pIQ'] = sor2[i-1,'pIQ']
        
        sor2[i,'pA1Qgain'] = sor2[i,'pIQ'] * sor2[i,'p1Q'] # how mauch is activated from I
        
        sor2[i,'pA1Q'] = sor2[i-1,'pA1Q'] + sor2[i,'pA1Qgain'] # how much is there in total with carried over
        
        sor2[i,'pA1Qloss'] = sor2[i,'pA1Q'] * sor2[i,'pd1'] # how much decays to A2
        
        sor2[i,'pA1Q'] = sor2[i,'pA1Q'] - sor2[i,'pA1Qloss'] # how much is left after the decay
        
        sor2[i,'pA1Qdelta'] = sor2[i,'pA1Qgain'] - sor2[i,'pA1Qloss']
        
        sor2[i,'pA2Qgain'] = sor2[i,'pA1Qloss'] # how much is gained from A1->A2 decay
        
        sor2[i,'pA2Q'] = sor2[i-1,'pA2Q'] + sor2[i,'pA2Qgain'] # how much is there with carried over
        
        sor2[i,'pA2Qloss'] = sor2[i,'pA2Q'] * sor2[i,'pd2'] # how much decays A2->I
        
        sor2[i,'pA2Q'] = sor2[i,'pA2Q'] - sor2[i,'pA2Qloss'] # how much is left after A2->I decay
        
        sor2[i,'pA2Qdelta'] = sor2[i,'pA2Qgain'] - sor2[i,'pA2Qloss']
        
        sor2[i,'pIQgain'] = sor2[i,'pA2Qloss'] # how much decayed from A2
        
        sor2[i,'pIQloss'] = sor2[i,'pA1Qgain']
        
        sor2[i,'pIQ'] = sor2[i,'pIQ'] + sor2[i,'pIQgain'] - sor2[i,'pIQloss']
        
        sor2[i,'pIQdelta'] = sor2[i,'pIQgain'] - sor2[i,'pIQloss']
        
        # X
        
        sor2[i,'pIX'] = sor2[i-1,'pIX']
        
        sor2[i,'pA1Xgain'] = sor2[i,'pIX'] * sor2[i,'p1X'] # how mauch is activated from I
        
        sor2[i,'pA1X'] = sor2[i-1,'pA1X'] + sor2[i,'pA1Xgain'] # how much is there in total with carried over
        
        sor2[i,'pA1Xloss'] = sor2[i,'pA1X'] * sor2[i,'pd1'] # how much decays to A2
        
        sor2[i,'pA1X'] = sor2[i,'pA1X'] - sor2[i,'pA1Xloss'] # how much is left after the decay
        
        sor2[i,'pA1Xdelta'] = sor2[i,'pA1Xgain'] - sor2[i,'pA1Xloss']
        
        sor2[i,'pA2Xgain'] = sor2[i,'pA1Xloss'] # how much is gained from A1->A2 decay
        
        sor2[i,'pA2X'] = sor2[i-1,'pA2X'] + sor2[i,'pA2Xgain'] # how much is there with carried over
        
        sor2[i,'pA2Xloss'] = sor2[i,'pA2X'] * sor2[i,'pd2'] # how much decays A2->I
        
        sor2[i,'pA2X'] = sor2[i,'pA2X'] - sor2[i,'pA2Xloss'] # how much is left after A2->I decay
        
        sor2[i,'pA2Xdelta'] = sor2[i,'pA2Xgain'] - sor2[i,'pA2Xloss']
        
        sor2[i,'pIXgain'] = sor2[i,'pA2Xloss'] # how much decayed from A2
        
        sor2[i,'pIXloss'] = sor2[i,'pA1Xgain']
        
        sor2[i,'pIX'] = sor2[i,'pIX'] + sor2[i,'pIXgain'] - sor2[i,'pIXloss']
        
        sor2[i,'pIXdelta'] = sor2[i,'pIXgain'] - sor2[i,'pIXloss']
        
        # Calculate V
        sor2[i, 'Ve'] = sor2[i, 'pA1X'] * sor2[i, 'pA1Q'] * input$Lp
        sor2[i, 'Vi'] = sor2[i, 'pA1X'] * sor2[i, 'pA2Q'] * input$Lm
        sor2[i, 'Vdelta'] = sor2[i, 'Ve'] - sor2[i, 'Vi']
        sor2[i, 'V'] = sum(sor2[1:i, 'Vdelta'])
        

    }
}



library(ggplot2)
#library(jtools)

sor = as.data.frame(sor)
sor$time = c(1:nrow(sor))
sor = sor[1:input$sordur,]

sor2 = as.data.frame(sor2)
sor2$time = c(1:nrow(sor2))
sor2 = sor2[1:input$sordur,]

text1 = 20
text2 = 18

pv = as.data.frame(cbind(sor$time, sor$V, sor2$V))
names(pv) = c('time', 'Vh', 'Vl')

png("vplh.png", width = 800, height = 400)
pltit = "SOP V Simulation, Cumulative Value of V"
ggplot(data=pv, aes(x = time, y=Vh, color="VXs")) +
    geom_line(size = 1) +
    geom_line(aes(y=Vl, colour = 'VXl'), size = 1) +
    theme_classic() + 
    geom_vline(xintercept = input$sorQon1[1], linetype = "dashed", colour = 'grey') + 
    geom_vline(xintercept = input$sorQon1[2], linetype = "dashed", colour = 'grey') +
    theme(legend.text = element_text(size = text2, face = "bold"), legend.title = element_text(size=text2, face="bold")) + 
    scale_color_discrete(name = "V type") + theme(axis.text=element_text(size=text1),
                                                  axis.title=element_text(size=text1,face="bold"), axis.text.x = element_text(size = text1),
                                                  axis.text.y = element_text(size = text1)) + 
    scale_x_continuous(name="Time", limits=c(0, input$sordur)) + scale_y_continuous(name="Value") + 
    ggtitle(pltit) + 
    theme(plot.title = element_text(size = text2, face = "bold", hjust = 0.5)) 
dev.off()

# png("vplot_1.png", width = 800, height = 400)
# 
# text1 = 20
# text2 = 18
# 
# pltit = "A. SOP V Simulation, Stimulus Activation"
# ggplot(data=as.data.frame(sor), aes(x = time, y=pA1X, group=1, color="X" )) +
#     geom_line(size = 1) + geom_line(aes(y=pA2X, colour = 'X'), size = 1, linetype = 'longdash') +
#     geom_line(aes(y=pA1Q, colour = 'Q'), size = 1) +
#     geom_line(aes(y=pA2Q, colour = 'Q'), size = 1, linetype = 'longdash') +
#     theme_classic() + 
#     geom_vline(xintercept = input$sorQon1[1], linetype = "dashed", colour = 'grey') + 
#     geom_vline(xintercept = input$sorQon1[2], linetype = "dashed", colour = 'grey') +
#     theme(legend.text = element_text(size = text2, face = "bold"), legend.title = element_text(size=text2, face="bold")) + 
#     scale_color_discrete(name = "Stimulus") + theme(axis.text=element_text(size=text1),
#                                                axis.title=element_text(size=text1,face="bold"), axis.text.x = element_text(size = text1),
#                                                axis.text.y = element_text(size = text1)) + 
#     scale_x_continuous(name="Time", limits=c(0, input$sordur)) + scale_y_continuous(name="Activation") + 
#     ggtitle(pltit) + 
#     theme(plot.title = element_text(size = text2, face = "bold", hjust = 0.5)) 
# dev.off()
# 
# png("vplot_2.png", width = 800, height = 400)
# 
# pltit = "B. SOP V Simulation, Cumulative Value of V"
# ggplot(data=as.data.frame(sor), aes(x = time, y=V, color="Cum V")) +
#     geom_line(size = 1) +
#     theme_classic() + 
#     geom_vline(xintercept = input$sorQon1[1], linetype = "dashed", colour = 'grey') + 
#     geom_vline(xintercept = input$sorQon1[2], linetype = "dashed", colour = 'grey') +
#     theme(legend.text = element_text(size = text2, face = "bold"), legend.title = element_text(size=text2, face="bold")) + 
#     scale_color_discrete(name = "V type") + theme(axis.text=element_text(size=text1),
#                                                     axis.title=element_text(size=text1,face="bold"), axis.text.x = element_text(size = text1),
#                                                     axis.text.y = element_text(size = text1)) + 
#     scale_x_continuous(name="Time", limits=c(0, input$sordur)) + scale_y_continuous(name="Value") + 
#     ggtitle(pltit) + 
#     theme(plot.title = element_text(size = text2, face = "bold", hjust = 0.5)) 
# dev.off()
# 
# png("vplot_3.png", width = 800, height = 400)
# 
# pltit = "C. SOP V Simulation, Delta V"
# ggplot(data=as.data.frame(sor), aes(x = time, y=V, color="Cumulative V")) +
#     geom_line(aes(y=Vdelta, colour = 'dV'), size = 1, linetype = 'longdash') +
#     geom_line(aes(y=Ve, colour = 'dV+'), size = 1, linetype = 'longdash') +
#     geom_line(aes(y=Vi, colour = 'dV-'), size = 1, linetype = 'longdash') +
#     theme_classic() + 
#     geom_vline(xintercept = input$sorQon1[1], linetype = "dashed", colour = 'grey') + 
#     geom_vline(xintercept = input$sorQon1[2], linetype = "dashed", colour = 'grey') +
#     theme(legend.text = element_text(size = text2, face = "bold"), legend.title = element_text(size=text2, face="bold")) + 
#     scale_color_discrete(name = "V type") + theme(axis.text=element_text(size=text1),
#                                                   axis.title=element_text(size=text1,face="bold"), axis.text.x = element_text(size = text1),
#                                                   axis.text.y = element_text(size = text1)) + 
#     scale_x_continuous(name="Time", limits=c(0, input$sordur)) + scale_y_continuous(name="Value") + 
#     ggtitle(pltit) + 
#     theme(plot.title = element_text(size = text2, face = "bold", hjust = 0.5)) 
# dev.off()
