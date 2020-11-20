input = list()
input$sorQp1 = .5
input$sorPp1 = .5
input$sorXp1 = .5

# s1 = c(1,30)
# s2 = c(40,70)
# tt = c(160,190)
# input$sordur = 220

s1 = c(1,30)
s2 = c(100,130)
tt = c(350,380)
input$sordur = 400

input$sorQon1 = s1
input$sorXon1 = s1

input$sorXon2 = s2
input$sorPon2 = s2

input$sorXon3 = tt
input$sorQon3 = tt
input$sorPon3 = tt

input$sorpd1 = 0.1
input$sorpd2 = input$sorpd1/5

input$Lp = .25
input$Lm = input$Lp/5

input$w1 = 1
input$w2 = input$w1/5

sor = matrix(nrow = input$sordur, ncol = 53)

colnames(sor) = c('p1Q', 'p1X', 'p1P',
                  'pd1', 'pd2', 
                  
                  'pIQ', 'pA1Q', 'pA2Q',
                  'pA1Qgain', 'pA1Qloss','pA2Qgain',
                  'pA2Qloss','pIQgain','pIQloss',
                  'pA1Qdelta', 'pA2Qdelta', 'pIQdelta',
                  
                  'pIP', 'pA1P', 'pA2P',
                  'pA1Pgain', 'pA1Ploss','pA2Pgain',
                  'pA2Ploss','pIPgain','pIPloss',
                  'pA1Pdelta', 'pA2Pdelta', 'pIPdelta',

                  'pIX', 'pA1X', 'pA2X',
                  'pA1Xgain', 'pA1Xloss','pA2Xgain',
                  'pA2Xloss','pIXgain','pIXloss',
                  'pA1Xdelta', 'pA2Xdelta', 'pIXdelta',
                  
                  'Ve_XQ', 'Vi_XQ', 'Vdelta_XQ', 'V_XQ', 'p2_XQ', 'p2_XQ_primed',
                  'Ve_XP', 'Vi_XP', 'Vdelta_XP', 'V_XP', 'p2_XP', 'p2_XP_primed'
                  )

sor[,'p1Q'] = 0 # set as 0 to init
sor[,'p1X'] = 0
sor[,'p1P'] = 0


sor[input$sorQon1[1]:input$sorQon1[2],'p1Q'] = input$sorQp1
sor[input$sorQon3[1]:input$sorQon3[2],'p1Q'] = input$sorQp1

sor[input$sorPon2[1]:input$sorPon2[2],'p1P'] = input$sorPp1
sor[input$sorPon3[1]:input$sorPon3[2],'p1P'] = input$sorPp1

sor[input$sorXon1[1]:input$sorXon1[2],'p1X'] = input$sorXp1
sor[input$sorXon2[1]:input$sorXon2[2],'p1X'] = input$sorXp1
sor[input$sorXon3[1]:input$sorXon3[2],'p1X'] = input$sorXp1


sor[,'pd1'] = input$sorpd1     # set decay rate
sor[,'pd2'] = input$sorpd2     # set decay rate

sor[,'pIQ'] = 1                 # Make I numeric
sor[,'pIX'] = 1   
sor[,'pIP'] = 1

# run moment by moment simulation
for (i in 1:nrow(sor)){
    if (i == 1){

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
        
        # Q
        sor[i,'pIQ'] = 1 - sor[i,'p1Q'] # This will be 1 as long as p1 = 0
        sor[i, 'p2_XQ'] = 0
        # NO P2 priming on t=1 as we need V for that, later use t-1 p2 (from previous moment)
        
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
        
        # Calculate V XQ
        sor[i, 'Ve_XQ'] = sor[i, 'pA1X'] * sor[i, 'pA1Q'] * input$Lp
        sor[i, 'Vi_XQ'] = sor[i, 'pA1X'] * sor[i, 'pA2Q'] * input$Lm
        sor[i, 'Vdelta_XQ'] = sor[i, 'Ve_XQ'] - sor[i, 'Vi_XQ']
        sor[i, 'V_XQ'] = sum(sor[1:i, 'Vdelta_XQ'])
        
        # Calculate p2 from VXQ, priming of Q
        sor[i,'p2_XQ'] = sor[i, 'V_XQ'] * (input$w1 * sor[i,'pA1X'] + input$w2 * sor[i,'pA2X'])
        
        # P
        sor[i,'pIP'] = 1 - sor[i,'p1P'] # This will be 1 as long as p1 = 0
        sor[i, 'p2_XP'] = 0
        # Deltas calculate
        # NO P2 priming on t=1 as we need V for that, later use t-1 p2 (from previous moment)
        sor[i,'pA1Pgain'] = sor[i,'p1P']
        sor[i,'pA1Ploss'] = sor[i,'pA1Pgain'] * sor[i,'pd1']
        sor[i,'pA1Pdelta'] = sor[i,'pA1Pgain'] - sor[i,'pA1Ploss']
        
        sor[i,'pA2Pgain'] = sor[i,'pA1Ploss']
        sor[i,'pA2Ploss'] = sor[i,'pA2Pgain'] * sor[i,'pd2']
        sor[i,'pA2Pdelta'] = sor[i,'pA2Pgain'] - sor[i,'pA2Ploss']
        
        sor[i,'pIPgain'] = sor[i,'pA2Ploss']
        sor[i,'pIPloss'] = sor[i,'p1P']
        sor[i,'pIPdelta'] = sor[i,'pIPgain'] - sor[i,'pIPloss']
        
        # Calculate proportions in each state
        sor[i,'pA1P'] = sor[i,'pA1Pdelta']
        sor[i,'pA2P'] = sor[i,'pA2Pdelta']
        sor[i,'pIP'] = sor[i,'pIP'] + sor[i,'pIPgain']
        
        # Calculate V XP
        sor[i, 'Ve_XP'] = sor[i, 'pA1X'] * sor[i, 'pA1P'] * input$Lp
        sor[i, 'Vi_XP'] = sor[i, 'pA1X'] * sor[i, 'pA2P'] * input$Lm
        sor[i, 'Vdelta_XP'] = sor[i, 'Ve_XP'] - sor[i, 'Vi_XP']
        sor[i, 'V_XP'] = sum(sor[1:i, 'Vdelta_XP'])
        
        # Calculate p2 from VXQ, priming of Q
        sor[i,'p2_XP'] = sor[i, 'V_XP'] * (input$w1 * sor[i,'pA1X'] + input$w2 * sor[i,'pA2X'])

        
    } else {
        
        
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
        
        # Q
        
        sor[i,'pIQ'] = sor[i-1,'pIQ']
        
        # PRIMING
        sor[i, 'p2_XQ_primed'] = sor[i, 'pIQ'] * sor[i-1, 'p2_XQ'] # how much is primed
        sor[i, 'pIQ'] = sor[i, 'pIQ'] - sor[i, 'p2_XQ_primed'] # how much is take from I
        
        sor[i,'pA1Qgain'] = sor[i,'pIQ'] * sor[i,'p1Q'] # how much is activated from I
        
        sor[i,'pA1Q'] = sor[i-1,'pA1Q'] + sor[i,'pA1Qgain'] # how much is there in total with carried over
        
        sor[i,'pA1Qloss'] = sor[i,'pA1Q'] * sor[i,'pd1'] # how much decays to A2
        
        sor[i,'pA1Q'] = sor[i,'pA1Q'] - sor[i,'pA1Qloss'] # how much is left after the decay
        
        sor[i,'pA1Qdelta'] = sor[i,'pA1Qgain'] - sor[i,'pA1Qloss']
        
        sor[i,'pA2Qgain'] = sor[i,'pA1Qloss'] + sor[i, 'p2_XQ_primed'] # how much is gained from A1->A2 decay + P2
        
        sor[i,'pA2Q'] = sor[i-1,'pA2Q'] + sor[i,'pA2Qgain'] # how much is there with carried over
        
        sor[i,'pA2Qloss'] = sor[i,'pA2Q'] * sor[i,'pd2'] # how much decays A2->I
        
        sor[i,'pA2Q'] = sor[i,'pA2Q'] - sor[i,'pA2Qloss'] # how much is left after A2->I decay
        
        sor[i,'pA2Qdelta'] = sor[i,'pA2Qgain'] - sor[i,'pA2Qloss']
        
        sor[i,'pIQgain'] = sor[i,'pA2Qloss'] # how much decayed from A2
        
        sor[i,'pIQloss'] = sor[i,'pA1Qgain']
        
        sor[i,'pIQ'] = sor[i,'pIQ'] + sor[i,'pIQgain'] - sor[i,'pIQloss']
        
        sor[i,'pIQdelta'] = sor[i,'pIQgain'] - sor[i,'pIQloss']
        
        # Calculate V XQ
        sor[i, 'Ve_XQ'] = sor[i, 'pA1X'] * sor[i, 'pA1Q'] * input$Lp
        sor[i, 'Vi_XQ'] = sor[i, 'pA1X'] * sor[i, 'pA2Q'] * input$Lm
        sor[i, 'Vdelta_XQ'] = sor[i, 'Ve_XQ'] - sor[i, 'Vi_XQ']
        sor[i, 'V_XQ'] = sum(sor[1:i, 'Vdelta_XQ'])
        
        # Calculate p2 from VXQ, priming of Q
        sor[i,'p2_XQ'] = sor[i, 'V_XQ'] * (input$w1 * sor[i,'pA1X'] + input$w2 * sor[i,'pA2X'])
        
        # P
        
        sor[i,'pIP'] = sor[i-1,'pIP']
        
        # PRIMING
        sor[i, 'p2_XP_primed'] = sor[i, 'pIP'] * sor[i-1, 'p2_XP'] # how much is primed
        sor[i, 'pIP'] = sor[i, 'pIP'] - sor[i, 'p2_XP_primed'] # how much is take from I
        
        sor[i,'pA1Pgain'] = sor[i,'pIP'] * sor[i,'p1P'] # how mauch is activated from I
        
        sor[i,'pA1P'] = sor[i-1,'pA1P'] + sor[i,'pA1Pgain'] # how much is there in total with carried over
        
        sor[i,'pA1Ploss'] = sor[i,'pA1P'] * sor[i,'pd1'] # how much decays to A2
        
        sor[i,'pA1P'] = sor[i,'pA1P'] - sor[i,'pA1Ploss'] # how much is left after the decay
        
        sor[i,'pA1Pdelta'] = sor[i,'pA1Pgain'] - sor[i,'pA1Ploss']
        
        sor[i,'pA2Pgain'] = sor[i,'pA1Ploss'] # how much is gained from A1->A2 decay
        
        sor[i,'pA2P'] = sor[i-1,'pA2P'] + sor[i,'pA2Pgain'] + sor[i, 'p2_XP_primed']# how much is there with carried over + P2
        
        sor[i,'pA2Ploss'] = sor[i,'pA2P'] * sor[i,'pd2'] # how much decays A2->I
        
        sor[i,'pA2P'] = sor[i,'pA2P'] - sor[i,'pA2Ploss'] # how much is left after A2->I decay
        
        sor[i,'pA2Pdelta'] = sor[i,'pA2Pgain'] - sor[i,'pA2Ploss']
        
        sor[i,'pIPgain'] = sor[i,'pA2Ploss'] # how much decayed from A2
        
        sor[i,'pIPloss'] = sor[i,'pA1Pgain']
        
        sor[i,'pIP'] = sor[i,'pIP'] + sor[i,'pIPgain'] - sor[i,'pIPloss']
        
        sor[i,'pIPdelta'] = sor[i,'pIPgain'] - sor[i,'pIPloss']
        
        # Calculate V XP
        sor[i, 'Ve_XP'] = sor[i, 'pA1X'] * sor[i, 'pA1P'] * input$Lp
        sor[i, 'Vi_XP'] = sor[i, 'pA1X'] * sor[i, 'pA2P'] * input$Lm
        sor[i, 'Vdelta_XP'] = sor[i, 'Ve_XP'] - sor[i, 'Vi_XP']
        sor[i, 'V_XP'] = sum(sor[1:i, 'Vdelta_XP'])
        
        # Calculate p2 from VXQ, priming of Q
        sor[i,'p2_XP'] = sor[i, 'V_XP'] * (input$w1 * sor[i,'pA1X'] + input$w2 * sor[i,'pA2X'])
    }
}



library(ggplot2)


sor = as.data.frame(sor)
sor$time = c(1:nrow(sor))
sor = sor[1:input$sordur,]

png("rrmclaren_1.png", width = 800, height = 400)

text1 = 20
text2 = 18

pltit = "SOP V Simulation, Stimulus Activation"
ggplot(data=as.data.frame(sor), aes(x = time, y=pA1Q, group=1, color="Q" )) +
    geom_line(size = 1) + geom_line(aes(y=pA2Q, colour = 'Q'), size = 1, linetype = 'longdash') +
    geom_line(aes(y=pA1P, colour = 'P'), size = 1) +
    geom_line(aes(y=pA2P, colour = 'P'), size = 1, linetype = 'longdash') +
    geom_line(aes(y=pA1X, colour = 'X'), size = 1) +
    geom_line(aes(y=pA2X, colour = 'X'), size = 1, linetype = 'longdash') +
    theme_classic() + 
    geom_vline(xintercept = s1[1], linetype = "dashed", colour = 'grey') + 
    geom_vline(xintercept = s1[2], linetype = "dashed", colour = 'grey') +
    geom_vline(xintercept = s2[1], linetype = "dashed", colour = 'grey') + 
    geom_vline(xintercept = s2[2], linetype = "dashed", colour = 'grey') +
    geom_vline(xintercept = tt[1], linetype = "dashed", colour = 'grey') + 
    geom_vline(xintercept = tt[2], linetype = "dashed", colour = 'grey') +
    theme(legend.text = element_text(size = text2, face = "bold"), legend.title = element_text(size=text2, face="bold")) + 
    scale_color_discrete(name = "Stimulus") + theme(axis.text=element_text(size=text1),
                                               axis.title=element_text(size=text1,face="bold"), axis.text.x = element_text(size = text1),
                                               axis.text.y = element_text(size = text1)) + 
    scale_x_continuous(name="Time", limits=c(0, input$sordur)) + scale_y_continuous(name="Activation") + 
    ggtitle(pltit) + 
    theme(plot.title = element_text(size = text2, face = "bold", hjust = 0.5)) 
dev.off()

png("rrmclaren_2.png", width = 800, height = 400)

pltit = "SOP V Simulation, Cumulative Value of V"
ggplot(data=as.data.frame(sor), aes(x = time, y=V_XQ, color="V XQ")) +
    geom_line(aes(y=V_XP, colour = 'V XP'), size = 1) +
    geom_line(size = 1) +
    theme_classic() +
    geom_vline(xintercept = s1[1], linetype = "dashed", colour = 'grey') + 
    geom_vline(xintercept = s1[2], linetype = "dashed", colour = 'grey') +
    geom_vline(xintercept = s2[1], linetype = "dashed", colour = 'grey') + 
    geom_vline(xintercept = s2[2], linetype = "dashed", colour = 'grey') +
    geom_vline(xintercept = tt[1], linetype = "dashed", colour = 'grey') + 
    geom_vline(xintercept = tt[2], linetype = "dashed", colour = 'grey') +
    theme(legend.text = element_text(size = text2, face = "bold"), legend.title = element_text(size=text2, face="bold")) +
    scale_color_discrete(name = "V type") + theme(axis.text=element_text(size=text1),
                                                    axis.title=element_text(size=text1,face="bold"), axis.text.x = element_text(size = text1),
                                                    axis.text.y = element_text(size = text1)) +
    scale_x_continuous(name="Time", limits=c(0, input$sordur)) + scale_y_continuous(name="Value") +
    ggtitle(pltit) +
    theme(plot.title = element_text(size = text2, face = "bold", hjust = 0.5))
dev.off()

png("rrmclaren_3.png", width = 800, height = 400)

pltit = "SOP V Simulation, p2 Values"
ggplot(data=as.data.frame(sor), aes(x = time, y=p2_XQ, color="p2 Q")) +
    geom_line(aes(y=p2_XP, colour = 'p2 P'), size = 1) +
    geom_line(size = 1) +
    theme_classic() +
    geom_vline(xintercept = s1[1], linetype = "dashed", colour = 'grey') + 
    geom_vline(xintercept = s1[2], linetype = "dashed", colour = 'grey') +
    geom_vline(xintercept = s2[1], linetype = "dashed", colour = 'grey') + 
    geom_vline(xintercept = s2[2], linetype = "dashed", colour = 'grey') +
    geom_vline(xintercept = tt[1], linetype = "dashed", colour = 'grey') + 
    geom_vline(xintercept = tt[2], linetype = "dashed", colour = 'grey') +
    theme(legend.text = element_text(size = text2, face = "bold"), legend.title = element_text(size=text2, face="bold")) +
    scale_color_discrete(name = "p2 Type") + theme(axis.text=element_text(size=text1),
                                                  axis.title=element_text(size=text1,face="bold"), axis.text.x = element_text(size = text1),
                                                  axis.text.y = element_text(size = text1)) +
    scale_x_continuous(name="Time", limits=c(0, input$sordur)) + scale_y_continuous(name="Value") +
    ggtitle(pltit) +
    theme(plot.title = element_text(size = text2, face = "bold", hjust = 0.5))
dev.off()
