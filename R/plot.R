#
# library(rgl)
# library(mapproj)
# library(maps)
# #library(OpenStreetMap)
# # to plot multiplex object we need the packages : rgl, mapproj, maps and OpenStreetMap
#
#
# plot.multiplex <- function(self, DIRECTED = F, WEIGHTED = F ){
#
#
#   #==== Choose the layout
#   LAYOUT_FRUCHTERMAN_REINGOLD <- F #for networks with < 1000 nodes
#   LAYOUT_LGL <- F                                              #for networks with > 1000 nodes
#   LAYOUT_DRL <- F                                              #for networks with > 1000 nodes
#   LAYOUT_SPRING <- T                                       #for networks with < 100 nodes
#   LAYOUT_KAMADA_KAWAI <- F                        #for networks with < 100 nodes
#   LAYOUT_REINGOLD_TILFORD <- F                #for networks with < 1000 nodes
#   LAYOUT_COMBINED <- F                                #for large and complicated networks...
#   LAYOUT_MAXITER <- 1000
#   LAYOUT_BY_LAYER_ID <- 0                            #0: use the aggregated, >0 use that layer ID
#   LAYOUT_INDEPENDENT <- F                           #if each layer can be layouted separately
#   #If external layout is provided for each layer, it will overwrite
#   #the above alternatives
#
#
#   #==== Output/Export options
#   FILE_RGL_SNAPSHOT <- "muxViz.png"
#   FILE_RGL_MOVIE <- "muxViz_movie.png"
#   EXPORT_MOVIE <- F
#
#   #==== Community detection algorithm
#   COMMUNITY_EDGE_BETWEENNESS <- F
#   COMMUNITY_RANDOM_WALK_TRAP <- F
#   COMMUNITY_INFOMAP <- T
#
#   #==== Graphic options
#   LAYER_SHOW <- T
#   NODE_LABELS_SHOW <- F
#   GEOGRAPHIC_BOUNDARIES_SHOW <- T
#   GEOGRAPHIC_BOUNDARIES_AGGREGATE_SHOW <- F
#   OSMType <- "stamen-watercolor"               #this fix the type of map to be used in the background
#   #osm-bbike-german
#   #bing
#
#   INTERLINK_SHOW <- F
#   INTERLINK_SHOW_FRACTION <- 0.2 #this allows to show only a few (randomly chosen) interlinks
#   #0: no interlinks 1: show all
#   RESCALE_WEIGHT <- F
#
#   NODE_TRANSP <- 0.1
#   EDGE_TRANSP <- 0.2
#   INTERLINK_TRANSP <- 0.2
#
#   PLOT_TITLE <- ""
#   PLOT_SUBTITLE <- ""
#   PLOT_FOV <- 20  #deg, can be changed with mouse
#   LAYER_COLOR <- "gray"
#
#
#   LAYER_TRANSP <- 0.1
#   LAYER_SHIFT <- 1                                #this shift the layers along x-axis to improve the perspective
#   LAYER_ARROW_SIZE <- 1
#   LAYER_ARROW_WIDTH <- 1
#   LAYER_ID_SHOW_TOPLEFT <- T
#   LAYER_ID_SHOW_BOTTOMLEFT<- F
#   LAYER_ID_SHOW_TOPRIGHT <- F
#   LAYER_ID_SHOW_BOTTOMRIGHT<- F
#   LAYER_ID_FONTSIZE <- 0.5
#   NODE_DEFAULT_SIZE <- 4
#   NODE_SIZE_PROPORTIONAL_TO_LOGPAGERANK <- F
#   NODE_SIZE_PROPORTIONAL_TO_STRENGTH <- F
#   NODE_SIZE_PROPORTIONAL_TO_LOGSTRENGTH <- F
#   NODE_SIZE_PROPORTIONAL_TO_LOGLOGSTRENGTH <- F
#   NODE_COLOR_BY_COMMUNITY <- F
#   COMMUNITY_MIN_SIZE <- 0     #will color with same RGB all nodes in communities smaller than this size
#   EDGE_DEFAULT_SIZE <- 0.5
#   EDGE_SIZE_PROPORTIONAL_TO_WEIGHT <- F
#   EDGE_SIZE_PROPORTIONAL_TO_LOGWEIGHT <- F
#   EDGE_SIZE_PROPORTIONAL_TO_LOGLOGWEIGHT <- F
#   EDGE_BENDING <- 0.2
#
#   INTERLINK_COLOR <- "black"
#   INTERLINK_TYPE <- "dotted"
#   INTERLINK_WIDTH <- 5
#
#   BACKGROUND_COLOR <- "#FFFFFF"
#
#   ###############
#   #End Parameters
#   ###############
#
#
#
#   LAYERS <- get_number_of_layers.multiplex(self)
#
#   layerLabel <- vector("list",LAYERS)
#   layerLayout <- vector("list",LAYERS)
#   nodesLabel <- vector("list",LAYERS)
#
#
#   for(l in 1:LAYERS){
#      #layerLabel[[l]] <- ""
#      #layerLabel[[l]] <- paste("Layer#",l,sep="")
#      layerLabel[[l]] <-get_layer_name.multiplex(self,l)
#     }
#
#  # layerLabel <-list("Conseil","Travailler-avec","Amitie")
#
# if(is.character(self@nodes))
#   Nodes = seq(length(self@nodes))
#   else
#     Nodes = self@nodes
# print(Nodes)
# print(layerLabel)
#   XMAX <- -1e10
#   YMAX <- -1e10
#   XMIN <- 1e10
#   YMIN <- 1e10
#   LONGMAX <- -1e10
#   LATMAX <- -1e10
#   LONGMIN <- 1e10
#   LATMIN <- 1e10
#
#
#
#   #If each layout is specified correctly
#   for(l in 1:LAYERS){
#
#     print(paste("Layout for layer",l,"not specified correctly. Proceeding with automatic layouting."))
#     LAYOUT_EXTERNAL <- F
#     GEOGRAPHIC_LAYOUT <- F
#
#     print(paste("Nodes' labels for layer",l,"not specified correctly. Proceeding with automatic labeling."))
#     #Assign labels to nodes
#     nodesLabel[[l]] <- Nodes
#
#   }
#
#
#   #giving the layout of the aggregate from external file makes no sense if it is different from the other layers
#   #and it is also annoying to be constrained to specify the aggregate, if one does not want to show it.
#   #Therefore, here I prefer to assign manually the layout of the first layer to the aggregate.
#   #So far, I accept this possibility just for sake of completeness, but a correct use of muxViz should avoid
#   #situations like this..
#
#
#   #Create the graph objects
#   g <- vector("list",LAYERS)
#
#   g <- self@layers
#   layouts <- vector("list",LAYERS)
#
#   #Check if the layouts are specified by external files, otherwise proceed with the automatic ones
#   if(!LAYOUT_EXTERNAL){
#
#       print("Independent layout option.")
#       for(l in 1:(LAYERS)){
#         layouts[[l]] <- matrix(c(1),ncol=3,nrow=Nodes)
#
#         print(paste("  Layout for layer",l,"..."))
#         #Each layout is calculated separatelya
#         if(LAYOUT_FRUCHTERMAN_REINGOLD){
#           layouts[[l]] <- layout.fruchterman.reingold.grid(g[[l]],weights=E(g[[l]])$weight,niter=LAYOUT_MAXITER,area=vcount(g[[l]])^1.,repulserad=vcount(g[[l]])^1.3,dim=2)
#         }
#         if(LAYOUT_LGL){
#           layouts[[l]] <- layout.lgl(g[[l]],maxiter=LAYOUT_MAXITER)
#         }
#         if(LAYOUT_DRL){
#           layouts[[l]] <- layout.drl(g[[l]],options=list(simmer.attraction=0,simmer.iterations=floor(LAYOUT_MAXITER*0.15),crunch.iterations=floor(LAYOUT_MAXITER*0.1),cooldown.iterations=floor(LAYOUT_MAXITER*0.25),expansion.iterations=floor(LAYOUT_MAXITER*0.25),liquid.iterations=floor(LAYOUT_MAXITER*0.25)))
#         }
#
#         if(LAYOUT_REINGOLD_TILFORD){
#           layouts[[l]] <- layout.reingold.tilford(g[[l]])
#         }
#
#         if(LAYOUT_KAMADA_KAWAI){
#           layouts[[l]] <- layout.kamada.kawai(g[[l]], niter=LAYOUT_MAXITER)
#         }
#         if(LAYOUT_SPRING){
#           layouts[[l]] <- layout.spring(g[[l]],repulse=T)
#         }
#
#         if(LAYOUT_COMBINED){
#           #We try to use the DRL to scale and we use it as seed for a Kamada-Kawai with few iterations
#           ltmp <- layout.drl(g[[l]],options=list(simmer.attraction=0,simmer.iterations=floor(LAYOUT_MAXITER*0.15),crunch.iterations=floor(LAYOUT_MAXITER*0.1),cooldown.iterations=floor(LAYOUT_MAXITER*0.25),expansion.iterations=floor(LAYOUT_MAXITER*0.25),liquid.iterations=floor(LAYOUT_MAXITER*0.25)))
#
#           layouts[[l]] <- layout.kamada.kawai(g[[l]], niter=LAYOUT_MAXITER,start=ltmp)
#         }
#
#     }
#   }else{
#     print("Layouting: external files.")
#     for(l in 1:LAYERS){
#       layouts[[l]] <- matrix(c(1),nrow=Nodes,ncol=2)
#       layouts[[l]] <- layerLayout[[l]]
#     }
#
#     #giving the layout of the aggregate from external file makes no sense if it is different from the other layers
#     #and it is also annoying to be constrained to specify the aggregate, if one does not want to show it.
#     #Therefore, here I prefer to assign manually the layout of the first layer to the aggregate.
#     #So far, I accept this possibility just for sake of completeness, but a correct use of muxViz should avoid
#     #situations like this..
#
#
#   }
#
#   for(l in 1:LAYERS){
#     layouts[[l]] <- layouts[[1]]
#   }
#
#
#   for(l in 1:LAYERS){
#     # layerLabel[[l]] <- paste("Layer#",l,sep="")
#     print("--------------")
#     print(l)
#     print(layerLabel[[l]])
#   }
#
#
#
#   #Make it a 3-columns object
#   for(l in 1:(LAYERS)){
#     layouts[[l]] <- cbind(layouts[[l]][,1:2],1)
#   }
#
#   if(!LAYOUT_EXTERNAL && !GEOGRAPHIC_LAYOUT){
#     for(l in 1:(LAYERS)){
#       if(min(layouts[[l]][,1],na.rm=T) < XMIN) XMIN = min(layouts[[l]][,1],na.rm=T)
#       if(min(layouts[[l]][,2],na.rm=T) < YMIN) YMIN = min(layouts[[l]][,2],na.rm=T)
#       if(max(layouts[[l]][,1],na.rm=T) > XMAX) XMAX = max(layouts[[l]][,1],na.rm=T)
#       if(max(layouts[[l]][,2],na.rm=T) > YMAX) YMAX = max(layouts[[l]][,2],na.rm=T)
#     }
#   }
#
#   print("Layouting finished. Proceeding with openGL plot of each layer.")
#
#
#   rgl.clear()
#
#   for(l in 1:(LAYERS)){
#     print(paste("Layer: ",l))
#
#     V(g[[l]])$vertex.label.color <- rgb(47,47,47,0,maxColorValue = 255)
#
#     #this set the transparency level of edges and nodes.. it can be customized
#     E(g[[l]])$alpha <- floor(EDGE_TRANSP*255)
#     V(g[[l]])$alpha <- floor(NODE_TRANSP*255)
#
#     # #generate a random color for this layer
#     # Rcolor <- sample(0:255, 1, replace=T)
#     # Gcolor <- sample(0:255, 1, replace=T)
#     # Bcolor <- sample(0:255, 1, replace=T)
#     #
#     # #assign the color to the layer
#     # E(g[[l]])$red <- Rcolor
#     # E(g[[l]])$green <- Gcolor
#     # E(g[[l]])$blue <- Bcolor
#     # V(g[[l]])$red <- Rcolor
#     # V(g[[l]])$green <- Gcolor
#     # V(g[[l]])$blue <- Bcolor
#     #
#     # E(g[[l]])$color<-rgb(E(g[[l]])$red, E(g[[l]])$green, E(g[[l]])$blue, E(g[[l]])$alpha, maxColorValue=255)
#     # V(g[[l]])$color <- rgb(V(g[[l]])$red, V(g[[l]])$green, V(g[[l]])$blue, V(g[[l]])$alpha, maxColorValue=255)
#
#
#     #lazega color
#     V(g[[l]])$color <- "grey"
#     if(l==1)
#       E(g[[l]])$color<-"lightgreen"
#     if(l==3)
#       E(g[[l]])$color<-"royalblue"
#     if(l==2)
#       E(g[[l]])$color<-"salmon"
#
#
#
#
#
#     #other assignments
#     E(g[[l]])$curve<- EDGE_BENDING
#
#     if(!NODE_LABELS_SHOW){
#       V(g[[l]])$label <- ""
#     }else{
#       V(g[[l]])$label <- nodesLabel[[l]]
#     }
#
#     V(g[[l]])$size <- NODE_DEFAULT_SIZE
#     if(NODE_SIZE_PROPORTIONAL_TO_STRENGTH) V(g[[l]])$size = graph.strength(g[[l]]);
#     if(NODE_SIZE_PROPORTIONAL_TO_LOGSTRENGTH) V(g[[l]])$size = 1+2*log(1+graph.strength(g[[l]]));
#     if(NODE_SIZE_PROPORTIONAL_TO_LOGLOGSTRENGTH) V(g[[l]])$size = NODE_DEFAULT_SIZE*log(1+log(1+graph.strength(g[[l]])));
#     if(NODE_SIZE_PROPORTIONAL_TO_LOGPAGERANK) V(g[[l]])$size = 1+1.2*log(1+Nodes*page.rank.old(g[[l]]));
#
#     E(g[[l]])$size <- EDGE_DEFAULT_SIZE;
#     if(WEIGHTED){
#       if(EDGE_SIZE_PROPORTIONAL_TO_WEIGHT) E(g[[l]])$size <- E(g[[l]])$weight
#       if(EDGE_SIZE_PROPORTIONAL_TO_LOGWEIGHT) E(g[[l]])$size <- log(1+E(g[[l]])$weight)
#       if(EDGE_SIZE_PROPORTIONAL_TO_LOGLOGWEIGHT) E(g[[l]])$size <- EDGE_DEFAULT_SIZE*log(1+log(1+E(g[[l]])$weight))
#     }
#
#     #rescale the layout to allow superposition with shift along z-axis
#     print("  Normalizing coordinates...")
#     layouts[[l]][,1] <- 2*(layouts[[l]][,1] - XMIN)/(XMAX-XMIN) - 1 + (l-1)*LAYER_SHIFT
#     layouts[[l]][,2] <- 2*(layouts[[l]][,2] - YMIN)/(YMAX-YMIN) - 1
#
#     layouts[[l]][,3] <- -1 + 2*l/LAYERS
#
#
#     if(NODE_COLOR_BY_COMMUNITY){
#       print("  Detecting communities for node coloring")
#
#       if(COMMUNITY_EDGE_BETWEENNESS){
#         wt <- edge.betweenness.community(g[[l]],modularity=TRUE)
#         wmemb <- community.to.membership(g[[l]], wt$merges,steps=which.max(wt$modularity)-1)
#       }
#       if(COMMUNITY_RANDOM_WALK_TRAP){
#         wt <- walktrap.community(g[[l]],modularity=TRUE)
#         wmemb <- community.to.membership(g[[l]], wt$merges,steps=which.max(wt$modularity)-1)
#       }
#       if(COMMUNITY_INFOMAP){
#         wt <- walktrap.community(g[[l]],modularity=TRUE)
#         wmemb <- community.to.membership(g[[l]], wt$merges,steps=which.max(wt$modularity)-1)
#
#         wt <- infomap.community(g[[l]],modularity=TRUE)
#         wmemb$membership <- membership(wt) - 1
#         comList <- communities(wt)
#         wmemb$csize <- numeric(length(comList))
#         for(com in 1:length(wmemb$csize)){
#           wmemb$csize[com] <- length(comList[[com]])
#         }
#       }
#       print(paste("  Modularity: ",modularity(wt)))
#       maxCom <- max(wmemb$membership) + 1
#
#       if(COMMUNITY_MIN_SIZE>0){
#         #Merge community smaller than chosen resolution to a unique community
#         #This will improve the coloring scheme when there are many isoloted nodes/components
#         mergedNodes <- 0
#         for(n in 1:length(wmemb$membership)){
#           if( wmemb$csize[ wmemb$membership[n]+1 ] <= COMMUNITY_MIN_SIZE ){
#             wmemb$membership[n]  <- -1
#             mergedNodes <- mergedNodes + 1
#           }
#         }
#
#         print(paste("  There are", mergedNodes, "nodes in communities smaller than",COMMUNITY_MIN_SIZE))
#         print("  Merging...")
#
#         maxCom <- max(wmemb$membership) + 1
#         mergeComID <-  maxCom + 1
#         wmemb$membership[wmemb$membership==-1] <- mergeComID
#         wmemb$csize[mergeComID] <- mergedNodes
#         wmemb$csize <- wmemb$csize[1:mergeComID]
#       }
#
#       print(paste("  Communities with >",COMMUNITY_MIN_SIZE,"nodes:",maxCom-1))
#
#       tmpColor <- rainbow( max(wmemb$membership) + 2, alpha=NODE_TRANSP, start=runif(1) )[ wmemb$membership + 1 ]
#
#       #setting a random start should avoid coloring nodes in the same way on different layers
#       V(g[[l]])$color <- tmpColor
#     }
#
#     print("  openGL phase...")
#     #plot the graph with openGL
#     rglplot(g[[l]], layout=layouts[[l]],
#                    vertex.size=V(g[[l]])$size,
#                    vertex.color=V(g[[l]])$color,
#                    vertex.label=V(g[[l]])$label,
#                    vertex.label.dist=0.4 + 0.01*graph.strength(g[[l]]),
#                    vertex.label.font=2,
#                    vertex.label.cex=2,
#                    vertex.label.color=V(g[[l]])$vertex.label.color,
#                    edge.width=E(g[[l]])$size,
#                    edge.color=E(g[[l]])$color,
#                    edge.arrow.size=LAYER_ARROW_SIZE,
#                    edge.arrow.width=LAYER_ARROW_WIDTH,
#                    edge.curved=E(g[[l]])$curve,
#                    rescale=F)
#
#     print(paste("  Layout of layer: finished."))
#   }
#
#
#   fileNamePNG <- paste("Toberename","_",OSMType,".png",sep="")
#   if(GEOGRAPHIC_LAYOUT && (GEOGRAPHIC_BOUNDARIES_SHOW || GEOGRAPHIC_BOUNDARIES_AGGREGATE_SHOW)){
#     print(paste("  Downloading geographic area..."))
#     #create a map with openstreetmap and save to a file for later use
#     rescaleFactor <- (YMAX-YMIN)/(XMAX-XMIN)
#     #H0/W0 = H/W  --> H = W/W0 * H0 = W*rescaleFactor in terms of Cartesian coords
#     pngWidth = 720
#     pngHeight = pngWidth*rescaleFactor
#     png(filename=fileNamePNG,width=pngWidth,height=pngHeight)
#     map = openmap(c(lat=LATMAX,   lon=LONGMIN), c(lat= LATMIN,   lon=LONGMAX), minNumTiles=18,type=OSMType)
#     plot(map)
#     dev.off()
#   }
#
#
#   if(LAYER_SHOW){
#     for( l in 1:LAYERS){
#       #This draws a plan to be used as layer
#       d <- -1 + 2*l/LAYERS
#
#
#       x <- c(-1,-1,1,1) + (l-1)*LAYER_SHIFT
#       y <- c(1,-1,-1,1)
#       z <- c(d,d,d,d)
#
#       if(l<=LAYERS){
#         #planes3d(0,0,1, -d , alpha=LAYER_TRANSP, col=LAYER_COLOR)
#         if(GEOGRAPHIC_LAYOUT && GEOGRAPHIC_BOUNDARIES_SHOW){
#           quads3d(x,y,z, alpha=LAYER_TRANSP, col=LAYER_COLOR,texcoords=cbind(c(0,0,1,1), -c(0,1,1,0)), texture=fileNamePNG)
#         }else{
#           quads3d(x,y,z, alpha=LAYER_TRANSP, col=LAYER_COLOR)
#         }
#       }
#
#       if(LAYER_ID_SHOW_BOTTOMLEFT){
#         text3d(-1+(l-1)*LAYER_SHIFT, -1, d+0.1,text=layerLabel[[l]],adj = 0.2, color="black", family="sans", cex=LAYER_ID_FONTSIZE)
#       }
#       if(LAYER_ID_SHOW_TOPLEFT){
#         text3d(-1+(l-1)*LAYER_SHIFT, 1, d+0.1,text=layerLabel[[l]],adj = 0.2, color="black", family="sans", cex=LAYER_ID_FONTSIZE)
#       }
#       if(LAYER_ID_SHOW_BOTTOMRIGHT){
#         text3d(1+(l-1)*LAYER_SHIFT, -1, d+0.1,text=layerLabel[[l]],adj = 0.2, color="black", family="sans", cex=LAYER_ID_FONTSIZE)
#       }
#       if(LAYER_ID_SHOW_TOPRIGHT){
#         text3d(1+(l-1)*LAYER_SHIFT, 1, d+0.1,text=layerLabel[[l]],adj = 0.2, color="black", family="sans", cex=LAYER_ID_FONTSIZE)
#       }
#     }
#
#   }
#
#   if(!LAYOUT_INDEPENDENT){
#     if(INTERLINK_SHOW & INTERLINK_SHOW_FRACTION>0){
#       print("Adding interlayer links.")
#       #to be generalized to allow cross-interlink and absence of interlinks for some nodes
#       for( l in 1:(LAYERS-1) ){
#         layerLinesX <- matrix(c(0),nrow=Nodes,ncol=2)
#         layerLinesY <- matrix(c(0),nrow=Nodes,ncol=2)
#         layerLinesZ <- matrix(c(0),nrow=Nodes,ncol=2)
#
#         layerLinesX <- cbind(layouts[[l]][,1] + (l-1)*LAYER_SHIFT,layouts[[l]][,1] + l*LAYER_SHIFT)
#         layerLinesY <- cbind(layouts[[l]][,2],layouts[[l]][,2])
#         layerLinesZ <- cbind(layouts[[l]][,3],layouts[[l]][,3])
#
#         for(i in 1:Nodes){
#           if(runif(1)>1-INTERLINK_SHOW_FRACTION){
#             segments3d(
#               layerLinesX[i,],
#               layerLinesY[i,],
#               layerLinesZ[i,],
#               lwd=INTERLINK_WIDTH,
#               col=INTERLINK_COLOR,
#               lty=INTERLINK_TYPE,
#               alpha=INTERLINK_TRANSP)
#           }
#         }
#       }
#     }
#   }
#
#   M <- matrix(0, ncol=4,nrow=4)
#   M[1,] <- c(0.54,0,0.84,0)
#   M[2,] <- c(0.33,0.92,-0.22,0)
#   M[3,] <- c(-0.77,0.39,0.5,0)
#   M[4,] <- c(0,0,0,1)
#
#   par3d(FOV=PLOT_FOV, userMatrix=M)
#   bg3d(BACKGROUND_COLOR)
#   title3d(PLOT_TITLE,PLOT_SUBTITLE,'','','')
#   #rgl.snapshot(FILE_RGL_SNAPSHOT)
#   print("Finalizing rendering...")
#
#   #  dev.off()
#
#
#
#
#
# }
