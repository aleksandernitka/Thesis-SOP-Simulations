input = list()
input$sorQp1 = .8

input$sorQon1 = c(10,40)
input$sorQon2 = c(50,80)

input$sordur = 100
input$sorpd1 = 0.1
input$sorpd2 = 0.02


sor = matrix(nrow = input$sordur, ncol = 15)

colnames(sor) = c('p1Q',
                  'pd1', 'pd2', 
                  'pIQ', 'pA1Q', 'pA2Q',
                  'pA1Qgain', 'pA1Qloss','pA2Qgain',
                  'pA2Qloss','pIQgain','pIQloss',
                  'pA1Qdelta', 'pA2Qdelta', 'pIQdelta'
                  )

sor[,'p1Q'] = 0 # set as 0 to init


sor[input$sorQon1[1]:input$sorQon1[2],'p1Q'] = input$sorQp1
sor[input$sorQon2[1]:input$sorQon2[2],'p1Q'] = input$sorQp1

sor[,'pd1'] = input$sorpd1     # set decay rate
sor[,'pd2'] = input$sorpd2     # set decay rate

sor[,'pIQ'] = 1                 # Make I numeric


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
        
        

    }
}



library(ggplot2)
#library(jtools)

sor = as.data.frame(sor)
sor$time = c(1:nrow(sor))
sor = sor[1:input$sordur,]

png("sgpplot_2.png", width = 800, height = 400)

text1 = 20
text2 = 18

ggplot(data=as.data.frame(sor), aes(x = time, y=pA1Q, group=1, color="Q" )) +
    geom_line(size = 1) + geom_line(aes(y=pA2Q, colour = 'Q'), size = 1, linetype = 'longdash') +
    geom_line(aes(y=pIQ, colour = 'Q'), size = 1, linetype = 'dotted') +
    theme_classic() + 
    geom_vline(xintercept = input$sorQon1[1], linetype = "dashed", colour = 'grey') + 
    geom_vline(xintercept = input$sorQon1[2], linetype = "dashed", colour = 'grey') +
    geom_vline(xintercept = input$sorQon2[1], linetype = "dashed", colour = 'grey') + 
    geom_vline(xintercept = input$sorQon2[2], linetype = "dashed", colour = 'grey') +
    theme(legend.text = element_text(size = text2, face = "bold"), legend.title = element_text(size=text2, face="bold")) + 
    scale_color_discrete(name = "Stimulus") + theme(axis.text=element_text(size=text1),
                                               axis.title=element_text(size=text1,face="bold"), axis.text.x = element_text(size = text1),
                                               axis.text.y = element_text(size = text1)) + 
    scale_x_continuous(name="Time", limits=c(0, input$sordur)) + scale_y_continuous(name="Activation") + 
    ggtitle("SGP Simulation") + 
    theme(plot.title = element_text(size = text2, face = "bold", hjust = 0.5)) 
    


dev.off()


