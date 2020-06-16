#'Get colors for a discrete variable
#'@description Get different colors for a vector, for example a Treatment variable
#'@param labels The input variable
#'@param grayscale If TRUE returns grayscale colors
#'@author BDA Team \email{matias.thayer@owlstone..co.uk}
#'BDA Team
#'License: GNU GPL (>= 2)
#'@export
#'
get_color_schema <- function(labels, grayscale=F){
  # Based on a similar function in Metaboanalyst
  lvs <- levels(as.factor(unlist(labels)))
  grp.num <- length(lvs);

  if(grayscale){
    dist.cols <- colorRampPalette(c("grey90", "grey30"))(grp.num);
  }else if(exists("colVec") && !any(colVec =="#NA")){
    dist.cols <- colVec;
  }else{
    pal18 <- c("#e6194B", "#3cb44b", "#4363d8", "#42d4f4", "#f032e6", "#ffe119", "#911eb4", "#f58231", "#bfef45",
               "#fabebe", "#469990", "#e6beff", "#9A6324", "#800000", "#aaffc3", "#808000", "#ffd8b1", "#000075");

    if(grp.num <= 18){ # update color and respect default
      dist.cols <- pal18[1:grp.num];
    }else{
      dist.cols <- colorRampPalette(pal18)(grp.num);
    }
  }
  colors <- vector(mode="character", length=length(labels));
  for(i in 1:length(lvs)){
    colors[labels == lvs[i]] <- dist.cols[i];
  }
  return (colors);
}

