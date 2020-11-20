input = list()
input$sorQp1 = .8
input$sorQon1 = c(1,50)
input$sordur = 700
input$sorpd1 = 0.1
input$sorpd2 = 0.02

input$V = 0.2
input$w1 = 1
input$w2 = input$w1/5

# p2 details
# a - model with A1 unscaled and A2 scaled as 0.2 (1/5th), A1 as in Uribe-Bahamonde et al., 2019 but A2 still contribues
# b - model shown in Uribe-Bahamonde et al., 2019

sor = matrix(nrow = input$sordur, ncol = 18)
colnames(sor) = c('p1Q', 'pd1Q', 'pd2Q', 
                  'pIQ', 'pA1Q', 'pA2Q', 'pTotalQ', 
                  'pA1Qgain', 'pA1Qloss','pA2Qgain','pA2Qloss','pIQgain','pIQloss',
                  'pA1Qdelta', 'pA2Qdelta', 'pIQdelta',
                  'p2a','p2b')

sor[,'p1Q'] = 0 # set as 0 to init


sor[input$sorQon1[1]:input$sorQon1[2],'p1Q'] = input$sorQp1

sor[,'pd1Q'] = input$sorpd1     # set decay rate for Q
sor[,'pd2Q'] = input$sorpd2     # set decay rate for Q
sor[,'pIQ'] = 0                 # Make I numeric

# run moment by moment simulation
for (i in 1:nrow(sor)){
    if (i == 1){
        
        sor[i,'pIQ'] = 1 - sor[i,'p1Q'] # This will be 1 as long as p1 = 0
        
        # Deltas calculate
        sor[i,'pA1Qgain'] = sor[i,'p1Q']
        sor[i,'pA1Qloss'] = sor[i,'pA1Qgain'] * sor[i,'pd1Q']
        sor[i,'pA1Qdelta'] = sor[i,'pA1Qgain'] - sor[i,'pA1Qloss']
        
        sor[i,'pA2Qgain'] = sor[i,'pA1Qloss']
        sor[i,'pA2Qloss'] = sor[i,'pA2Qgain'] * sor[i,'pd2Q']
        sor[i,'pA2Qdelta'] = sor[i,'pA2Qgain'] - sor[i,'pA2Qloss']

        sor[i,'pIQgain'] = sor[i,'pA2Qloss']
        sor[i,'pIQloss'] = sor[i,'p1Q']
        sor[i,'pIQdelta'] = sor[i,'pIQgain'] - sor[i,'pIQloss']

        # Calculate proportions in each state
        sor[i,'pA1Q'] = sor[i,'pA1Qdelta']
        sor[i,'pA2Q'] = sor[i,'pA2Qdelta']
        sor[i,'pIQ'] = sor[i,'pIQ'] + sor[i,'pIQgain']
        sor[i,'pTotalQ'] = sor[i,'pIQ'] + sor[i,'pA1Q'] + sor[i,'pA2Q']  # total in all states
        
        # calc p2 a and b
        sor[i,'p2a'] = input$V * ( input$w1 * sor[i,'pA1Q'] + ( input$w2 * sor[i,'pA2Q']) )
        sor[i,'p2b'] = input$V * sor[i,'pA1Q']

        
    } else {
        
        sor[i,'pIQ'] = sor[i-1,'pIQ']
        
        sor[i,'pA1Qgain'] = sor[i,'pIQ'] * sor[i,'p1Q'] # how mauch is activated from I
        
        sor[i,'pA1Q'] = sor[i-1,'pA1Q'] + sor[i,'pA1Qgain'] # how much is there in total with carried over
        
        sor[i,'pA1Qloss'] = sor[i,'pA1Q'] * sor[i,'pd1Q'] # how much decays to A2
        
        sor[i,'pA1Q'] = sor[i,'pA1Q'] - sor[i,'pA1Qloss'] # how much is left after the decay
        
        sor[i,'pA1Qdelta'] = sor[i,'pA1Qgain'] - sor[i,'pA1Qloss']
        
        sor[i,'pA2Qgain'] = sor[i,'pA1Qloss'] # how much is gained from A1->A2 decay
        
        sor[i,'pA2Q'] = sor[i-1,'pA2Q'] + sor[i,'pA2Qgain'] # how much is there with carried over
        
        sor[i,'pA2Qloss'] = sor[i,'pA2Q'] * sor[i,'pd2Q'] # how much decays A2->I
        
        sor[i,'pA2Q'] = sor[i,'pA2Q'] - sor[i,'pA2Qloss'] # how much is left after A2->I decay
        
        sor[i,'pA2Qdelta'] = sor[i,'pA2Qgain'] - sor[i,'pA2Qloss']
        
        sor[i,'pIQgain'] = sor[i,'pA2Qloss'] # how much decayed from A2
        
        sor[i,'pIQloss'] = sor[i,'pA1Qgain']
        
        sor[i,'pIQ'] = sor[i,'pIQ'] + sor[i,'pIQgain'] - sor[i,'pIQloss']
        
        sor[i,'pIQdelta'] = sor[i,'pIQgain'] - sor[i,'pIQloss']
        
        sor[i,'pTotalQ'] = sor[i,'pIQ'] + sor[i,'pA1Q'] + sor[i,'pA2Q']
        
        sor[i,'p2a'] = input$V * ( input$w1 * sor[i,'pA1Q'] + ( input$w2 * sor[i,'pA2Q']) )
        sor[i,'p2b'] = input$V * sor[i,'pA1Q']
    }
}


# Round p2 vals to 3th decimal
sor[,'p2a'] = round(sor[,'p2a'], 3)
sor[,'p2b'] = round(sor[,'p2b'], 3)

# find when p2 == 0
p2a0 = min(which(sor[,'p2a'] == 0))
p2b0 = min(which(sor[,'p2b'] == 0))

ymax = max( c(sor[,'p2a'], sor[,'p2b']))

png("plot.png", width = 1400, height = 1000)

library(ggplot2)
#library(jtools)

sor = as.data.frame(sor)
sor$time = c(1:nrow(sor))
sor = sor[1:300,]

png("plot.png", width = 800, height = 400)

text1 = 20
text2 = 18

ggplot(data=as.data.frame(sor), aes(x = time, y=p2a, group=1, colour = 'A')) +
    geom_line(size = 1) + geom_line(aes(y=p2b, colour = 'B'), size =1, linetype = 'longdash') + theme_classic() + 
    geom_vline(xintercept = 50, linetype = "dashed", colour = 'grey') + 
    geom_vline(xintercept = 0, linetype = "dashed", colour = 'grey') +
    theme(legend.text = element_text(size = text2, face = "bold"), legend.title = element_text(size=text2, face="bold")) + 
    scale_color_discrete(name = "Model") + theme(axis.text=element_text(size=text1),
                                               axis.title=element_text(size=text1,face="bold"), axis.text.x = element_text(size = text1),
                                               axis.text.y = element_text(size = text1)) + 
    scale_x_continuous(name="Time", limits=c(0, 300)) + scale_y_continuous(name="Intensity of p2") + 
    geom_point(aes(x=90, y=0), colour='blue', size = 4, shape = 21, stroke = 2) +
    geom_point(aes(x=268, y=0), colour='red', size = 4, shape = 21, stroke = 2) +
    ggtitle("Parameter p2 simulations") + 
    theme(plot.title = element_text(size = text2, face = "bold", hjust = 0.5))


dev.off()


