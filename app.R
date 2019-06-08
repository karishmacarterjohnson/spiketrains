#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    selectInput("region", "Choose a brain region",
                choices = c("Caudate Putamen (CP)","Frontal Motor Cortex (FrMoCtx)","Hippocampal Formation (HPF)",
                            "Lateral Septum (LS)", "Midbrain (MB)", "Superior Colliculus (SC)", "Somatomotor Cortex (SoMoCtxx)","Thalamus (TH)","Primary Visual Cortex (V1)" )),
    
    
    mainPanel(
        plotOutput("spikePlot")
    )
    
)


server <- function(input, output){

    i_chosen <- reactive({
        switch(input$region,
               "Caudate Putamen (CP)" = 1,
               "Frontal Motor Cortex (FrMoCtx)" = 2,
               "Hippocampal Formation (HPF)" = 3,
               "Lateral Septum (LS)" = 4,
               "Midbrain (MB)" = 5,
               "Superior Colliculus (SC)" = 6,
               "Somatomotor Cortex (SoMoCtxx)" = 7,
               "Thalamus (TH)" = 8,
               "Primary Visual Cortex (V1)" = 9
        )
    })
     library("R.matlab") # Install 'R.matlab' if this is the first time you call it.
    # # Below VV Will vary depending on the local pathway for each computer
     ephysroot = ''; # path of the data set 
     mstr = 'Krebs'; # mouse names
    # ### Load saved data:
     exp_data<- readMat(paste(ephysroot,mstr,'_reduced.mat',sep=''));
    #setwd("C:/Users/karis/Desktop/spiketrainchoose/")
    
    
    brain_regions= ls(exp_data)[-c(1:3)];
    n_time= dim(exp_data$faceSVD)[2];
    n_sv = dim(exp_data$faceSVD)[1];
    
    spiketrain.chosen <- function(i_region){
        # plot_list = list()
        # for(i_region in 1:9) {
        time_range = c(0, n_time); # indices of time frame
        
        ### Extract the spike train for this region
        spikes_this_region = exp_data[[brain_regions[i_region]]]
        
        ### Visualize the spike train
        n_neurons = dim(spikes_this_region)[1];
        p = plot(x=0,y=0,pch=16,col="white",type="l",lwd=3,ylim=c(0,n_neurons),
                 xlim=time_range,cex.lab=1,cex.main=1,ylab='Neuron',xlab='Time frame',
                 main=paste0("Spike Train Region ", i_region, " Time ", time_range[1],
                             " to ", time_range[2]))
        #yaxt='n',xaxt='n',
        
        for(i_neuron in 1:n_neurons){
            spk_times =which(spikes_this_region[i_neuron,time_range[1]:time_range[2]]>0);
            if (length(spk_times)>0){
                points(y=rep(i_neuron,length(spk_times)),x=spk_times+time_range[1]-1,col="#000000",pch='.',cex=2)
            }
        }
        
        
    }
    
    
    output$spikePlot <- renderPlot({
        spiketrain.chosen(i_region = i_chosen())
    })
    
}
    
# Run the application 
shinyApp(ui = ui, server = server)
