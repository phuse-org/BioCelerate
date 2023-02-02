#Output MI Histopathy plot for Cross-Study Analysis for organSystem or all defined 
#in CrossStudyGraph_Combined.R or CrossStudyVisualizationApp.R

#Gender must be restricted for each plot to either M or F or Combined

#Dose must be specified; Vehicle/Control will be added for comparisson

#Current organSystem options: REPRODUCTIVE, LIVER, KIDNEY, HEMATOPOIETIC, and
#ENDOCRINE

makeMIplot <- function(MIresults, OrganSystem, Dose, Gender){

  #Check that Vehicle is included in Dose
  if ( grepl("Vehicle", Dose) == "FALSE"){
    DOSE <- c(Dose, "Vehicle")
  }
  
    #Limit to Desired OrganSystem and Gender
    MIplotData <- MIresults[[OrganSystem]][[Gender]]
    
    #Limit to Proper Dose
    MIplotData <- MIplotData[which(MIplotData$Dose %in% DOSE),]
    
    Heatmap <- ggplot(MIplotData) +
      geom_tile_pattern(aes(x = Finding, fill = Count, y = Treatment,
                            pattern_fill = MISEV, pattern_density = MISEV),
                        pattern= 'stripe', pattern_color="black") +
      scale_pattern_density_manual(values = c("0" = 0, "1" = 0.1, "2" = 0.2, "3"=0.3, "4" = 0.4, "5"= 0.5))+
      scale_pattern_fill_brewer(palette = 3)+ 
      ylab('Study (Species): Treatment') + 
      ggtitle(paste0(OrganSystem, " - ",Dose, " ",Gender)) + scale_y_discrete( name = 'Findings' ) + 
      xlab("Study Code and Treatment") + 
      scale_fill_gradient2(low = muted('blue'), high = muted('red'), name = 'Incidence') + theme_classic() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    Legend <- get_legend(Heatmap)
    Heatmap <- Heatmap + theme(legend.position = "none")
    return(print(ggdraw(plot_grid(Heatmap,Legend, align='h', rel_widths = c(4/5,1/5))))) 

}