input = list()
input$sorQp1 = .8
input$sorPp1 = .8
input$sorPon1 = c(1,30)
input$sorQon2 = c(40,70)
input$sorPon2 = c(40,70)
input$sordur = 100
input$sorpd1 = 0.1
input$sorpd2 = 0.02

input$V = 0.2
input$w1 = 1
input$w2 = input$w1/5

sor = matrix(nrow = input$sordur, ncol = 28)

colnames(sor) = c('p1Q', 'p1P',
                  'pd1', 'pd2', 
                  'pIQ', 'pA1Q', 'pA2Q',
                  'pIP', 'pA1P', 'pA2P',
                  
                  'pA1Qgain', 'pA1Qloss','pA2Qgain',
                  'pA2Qloss','pIQgain','pIQloss',
                  'pA1Qdelta', 'pA2Qdelta', 'pIQdelta',
                  
                  'pA1Pgain', 'pA1Ploss','pA2Pgain',
                  'pA2Ploss','pIPgain','pIPloss',
                  'pA1Pdelta', 'pA2Pdelta', 'pIPdelta'
                  )

sor[,'p1Q'] = 0 # set as 0 to init
sor[,'p1P'] = 0


sor[input$sorPon1[1]:input$sorPon1[2],'p1P'] = input$sorQp1
sor[input$sorPon2[1]:input$sorPon2[2],'p1P'] = input$sorQp1
sor[input$sorQon2[1]:input$sorQon2[2],'p1Q'] = input$sorPp1

sor[,'pd1'] = input$sorpd1     # set decay rate
sor[,'pd2'] = input$sorpd2     # set decay rate

sor[,'pIQ'] = 0                 # Make I numeric
sor[,'pIP'] = 0

# run moment by moment simulation
for (i in 1:nrow(sor)){
    if (i == 1){
        
        sor[i,'pIQ'] = 1 - sor[i,'p1Q'] # This will be 1 as long as p1 = 0
        
        # Q
        
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
        
        # P
        
        # Deltas calculate
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


        
    } else {
        
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
        
        # P
        
        sor[i,'pIP'] = sor[i-1,'pIP']
        
        sor[i,'pA1Pgain'] = sor[i,'pIP'] * sor[i,'p1P'] # how mauch is activated from I
        
        sor[i,'pA1P'] = sor[i-1,'pA1P'] + sor[i,'pA1Pgain'] # how much is there in total with carried over
        
        sor[i,'pA1Ploss'] = sor[i,'pA1P'] * sor[i,'pd1'] # how much decays to A2
        
        sor[i,'pA1P'] = sor[i,'pA1P'] - sor[i,'pA1Ploss'] # how much is left after the decay
        
        sor[i,'pA1Pdelta'] = sor[i,'pA1Pgain'] - sor[i,'pA1Ploss']
        
        sor[i,'pA2Pgain'] = sor[i,'pA1Ploss'] # how much is gained from A1->A2 decay
        
        sor[i,'pA2P'] = sor[i-1,'pA2P'] + sor[i,'pA2Pgain'] # how much is there with carried over
        
        sor[i,'pA2Ploss'] = sor[i,'pA2P'] * sor[i,'pd2'] # how much decays A2->I
        
        sor[i,'pA2P'] = sor[i,'pA2P'] - sor[i,'pA2Ploss'] # how much is left after A2->I decay
        
        sor[i,'pA2Pdelta'] = sor[i,'pA2Pgain'] - sor[i,'pA2Ploss']
        
        sor[i,'pIPgain'] = sor[i,'pA2Ploss'] # how much decayed from A2
        
        sor[i,'pIPloss'] = sor[i,'pA1Pgain']
        
        sor[i,'pIP'] = sor[i,'pIP'] + sor[i,'pIPgain'] - sor[i,'pIPloss']
        
        sor[i,'pIPdelta'] = sor[i,'pIPgain'] - sor[i,'pIPloss']

    }
}




png("plot.png", width = 1400, height = 1000)

library(ggplot2)
#library(jtools)

sor = as.data.frame(sor)
sor$time = c(1:nrow(sor))
sor = sor[1:input$sordur,]

png("sorplot.png", width = 800, height = 400)

text1 = 20
text2 = 18

ggplot(data=as.data.frame(sor), aes(x = time, y=pA1P, group=1, colour = 'P')) +
    geom_line(size = 1) + geom_line(aes(y=pA2P, colour = 'P'), size =1, linetype = 'longdash') + theme_classic() + 
    geom_line(aes(y=pA1Q, colour = 'Q'), size =1) +
    geom_line(aes(y=pA2Q, colour = 'Q'), size =1, linetype = 'longdash') +
    geom_vline(xintercept = 30, linetype = "dashed", colour = 'grey') + 
    geom_vline(xintercept = 1, linetype = "dashed", colour = 'grey') +
    geom_vline(xintercept = 40, linetype = "dashed", colour = 'grey') + 
    geom_vline(xintercept = 70, linetype = "dashed", colour = 'grey') +
    theme(legend.text = element_text(size = text2, face = "bold"), legend.title = element_text(size=text2, face="bold")) + 
    scale_color_discrete(name = "Stimulus") + theme(axis.text=element_text(size=text1),
                                               axis.title=element_text(size=text1,face="bold"), axis.text.x = element_text(size = text1),
                                               axis.text.y = element_text(size = text1)) + 
    scale_x_continuous(name="Time", limits=c(0, input$sordur)) + scale_y_continuous(name="Activation") + 
    ggtitle("SOR Simulation") + 
    theme(plot.title = element_text(size = text2, face = "bold", hjust = 0.5)) 
    


dev.off()


