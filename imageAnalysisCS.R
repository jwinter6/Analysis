##################################################################
## Image Analysis Script                                        ##
##      Version 7                                               ##
##      raw images are supposed to be placed 'plateDir'         ##
##      saves compressed imges after segmentation in 'resDir'   ##
##      pushes single cell data in 'dbDir'                      ##
##          expects three raw images per unique id              ##
##              ATTENETION:                                     ##
##                  the name of the channels are hard coded     ##
##                  e.g. DAPI, Cy3 and FITC                     ##
##      procedure:                                              ##
##          a list with all identifiers in 'plateDir' is created##
##          image analysis by applying a function over this list##
##          image analysis details are commented in function    ##
##          an empty table is created if image analysis fails   ##
##              this requires 'mockNames'                       ##   
##################################################################


rm(list=setdiff(ls(), "x"))
cat("\014")


library(stringr)
library(dplyr)
library(EBImage)
library(parallel)
library(FNN)

plateDir <- c("/Users/user/Desktop/test")
resDir <- c("/Users/user/Desktop/results")
dbDir <- c("/Users/user/Desktop/results")
sc_db <- src_sqlite(paste(dbDir,"test.sqlite3",sep="/"),create=T)


files <- list.files(plateDir)
images <- files[grep("tif",files)]

# mockNames =get(
#     load(
#         ''
#     )
# )

#create vector with file names and sub for search-identifier (well and field info) 
fileNames <- sapply(images,function(x){
                    idi <- sub("wv.*","",x,perl=TRUE)
                    return(idi)
                    }
                )

#create list with all file names matching one serach-identifer 
fileList <- as.list(unique(fileNames))
names(fileList) <- unique(fileNames)
fileList <- lapply(fileList,function(x){
                x <- names(fileNames[which(fileNames %in% x)])
                    }
                )

#function
anlyzeImages <- function(x) {
    
    ## grab image names     
    DAPI_image_name=x[grep("DAPI",x)]
    tub_image_name=x[grep("FITC",x)]
    actin_image_name=x[grep("dsRed",x)]
    
    
    if(file.exists(paste(plateDir,DAPI_image_name,sep="/")) && 
       file.exists(paste(plateDir,actin_image_name,sep="/")) && 
       file.exists(paste(plateDir,tub_image_name,sep="/")) ){
        
        ## load images        
        identifier <- str_replace_all(DAPI_image_name, fixed(" "), "")
        identifier=sub("wv.*","",identifier,perl=TRUE)
        identifier <- str_replace_all(identifier, fixed("("), "")
        
        img1_DAPI <- readImage(paste(plateDir,DAPI_image_name,sep="/"))
        img1_Cy3 <- readImage(paste(plateDir,actin_image_name,sep="/"))
        img1_FITC <- readImage(paste(plateDir,tub_image_name,sep="/"))
        
        ## test images for artifacts 
        strange_behavior <-  c("dark"=0,"fussel"=0)
        medi=median(img1_DAPI)
        midi=mean(img1_DAPI) 
        stat_diff <- abs(abs(medi)-abs(midi))
        
        if(medi<0.15){
            strange_behavior["dark"]=1}
        
        if(stat_diff > medi){
            strange_behavior["fussel"]=1}
        
        ## blur images with gaussian filter 
        Filter1 = makeBrush(size=51,shape = "gaussian",sigma=1)/12.91571
        Filter3 = makeBrush(size=51,shape = "gaussian",sigma=4)/12.91571
        img1_Cy3smooth = filter2(img1_Cy3, filter=Filter3)
        img1_FITCsmooth = filter2(img1_FITC, filter=Filter3)
        img1_DAPI_blur = filter2(img1_DAPI, filter=Filter1)
        
        ## segment nuclei using an adaptive threshold 
        nucleusFill_blur = thresh(img1_DAPI_blur, w = 16, h = 16, offset = 0.0006)
        nucleusFill_blur = opening(nucleusFill_blur, kern=makeBrush(7, shape="disc"))
        nucleusFill_blur2 = fillHull(thresh(img1_DAPI_blur, w = 28, h = 28, offset = 0.00005))
        nucleusFill_blur = propagate(img1_DAPI_blur, seed=bwlabel(nucleusFill_blur), mask=nucleusFill_blur2)
        
        
        ## segment cell bodies by adaptive and global thersholding 
        ##      the actin and tubulin channel are used   
        cytoplasmThresh = thresh(img1_Cy3smooth, w = 40, h = 40, offset =  0.00005)
        cytoplasmOpening = opening(cytoplasmThresh,kern=makeBrush(7,shape="disc"))
        
        cytoplasmOpening2 = opening(img1_Cy3smooth > 0.0025)
        tubOpening = opening(img1_FITCsmooth > 0.001)
        
        
        cytoplasmCombined = cytoplasmOpening
        cytoplasmCombined[cytoplasmOpening2 > cytoplasmCombined] = cytoplasmOpening2[cytoplasmOpening2 > cytoplasmCombined]
        cytoplasmCombined[tubOpening > cytoplasmCombined] = tubOpening[tubOpening > cytoplasmCombined]
        cytoplasmCombined[nucleusFill_blur2 > cytoplasmCombined] = nucleusFill_blur2[nucleusFill_blur2 > cytoplasmCombined]
        
        cytoplasmRegions = propagate(x = img1_Cy3smooth, seeds = nucleusFill_blur, lambda=5e-07, mask=cytoplasmCombined)
        
        
        ## merge channels and save as compressed image with segmentation borders  
        ImgColor = rgbImage(normalize(img1_Cy3),
                            normalize(img1_FITC),
                            normalize(img1_DAPI))
        Imgout = paintObjects(cytoplasmRegions,
                              paintObjects(nucleusFill_blur,ImgColor, col='blue'),
                              col='white')
        writeImage(Imgout,
                   file=paste(resDir,"/",identifier,"_segmented",".tif",sep=""),
                   type="tiff",
                   quality = 10, 8 )
        
        ## compute features       
        F_DAPI = computeFeatures(nucleusFill_blur,
                                 ref=img1_DAPI,
                                 xname="nuc",
                                 refnames="nuc"
        )
        
        F_tub = computeFeatures(cytoplasmRegions,
                                ref=img1_FITC,
                                xname="cell",
                                refnames="tub")
        
        F_actin = computeFeatures(cytoplasmRegions,
                                  ref=img1_Cy3,
                                  xname="cell",
                                  refnames="act")
        
        # if feature vectors are empty an table with NAs is pushed in db
        if(!is.null(F_DAPI) && !is.null(F_actin)) {
            
            F_DAPI_tub = computeFeatures(cytoplasmRegions,
                                         ref=(img1_DAPI - mean(img1_DAPI)) * (img1_FITC - mean(img1_FITC)),
                                         xname="cell", refnames="nuctub")
            
            ## get additional features
            ##      nuclear displacement
            ##      nearest nieghbour info
            
            if(nrow(F_actin)>1 && nrow(F_DAPI)>1) {
                nucleus.displacement = sqrt(
                    rowSums(
                        (F_actin[,c("cell.act.m.cx", "cell.act.m.cx")] - F_DAPI[,c("nuc.nuc.m.cx", "nuc.nuc.m.cx")])^2
                    )
                )
                names(nucleus.displacement) = c("nuclear.displacement")
            } else {nucleus.displacement = NA}
            
            
            
            nearest.neighbours = try(get.knn(F_actin[,c("cell.act.m.cx", "cell.act.m.cx")],k=30)[["nn.dist"]][,c(10,20,30)],silent=T)
            if(inherits(nearest.neighbours,"try-error",which=F)){
                nearest.neighbours = cbind(dist.10.nn = rep(NA,nrow(F_DAPI)),
                                           dist.20.nn = rep(NA,nrow(F_DAPI)),
                                           dist.30.nn = rep(NA,nrow(F_DAPI)) )
            } else {
                colnames(nearest.neighbours) = c("dist.10.nn","dist.20.nn","dist.30.nn")  
            }
            
        
        #cbind all feature vectors 
        F_all <- cbind(id=rep(identifier,nrow(F_DAPI)),
                           F_DAPI,F_tub,F_actin,
                           F_DAPI_tub,
                           nucleus.displacement,
                           nearest.neighbours,
                           fussel = rep(strange_behavior["fussel"],nrow(F_DAPI)),
                           dark = rep(strange_behavior["dark"],nrow(F_DAPI))
            )

        F_all <- data.frame(F_all)
            
        ## push data frame in table, push empty table if feature vectors are empty  
        db_insert_into( con = sc_db$con, table = "test", values = F_all,indexes=list(c("id")))
        remove(F_DAPI)
        remove(F_tub)
        remove(F_actin)
        remove(F_DAPI_tub)
        remove(nucleus.displacement)
        remove(nearest.neighbours)
        remove(strange_behavior)
        remove(F_all)
        } else {
            F_all <- data.frame(matrix(ncol = length(mockNames))) 
            colnames(F_all) <- mockNames
            db_insert_into( con = sc_db$con, table = "test", values = F_all,indexes=list(c("id","id_s")))
            remove(F_DAPI)
            remove(F_tub)
            remove(F_actin)
            remove(F_DAPI_tub)
            remove(nucleus.displacement)
            remove(nearest.neighbours)
            remove(strange_behavior)
            remove(F_all)
        }
    }
}

#apply the function 
lapply(fileList,anlyzeImages)


