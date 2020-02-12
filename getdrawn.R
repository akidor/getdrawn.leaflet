
mk.buffershape<-function(vector,buff){
  library(raster)
  library(sp)
  trans.crs <- CRS("+init=epsg:3857")
  mycrs<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" )
  vector<-spTransform(vector,trans.crs)
  vector<-buffer(vector,buff)
  vector<-spTransform(vector,mycrs)
  return(vector)
}




get.drawn<-function(leaflet.draw){
  type <- leaflet.draw$geometry$type
  library(sp)
  mycrs<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" )
  trans.crs <- CRS("+init=epsg:3857")
  
  
  if(type == "Polygon"){
    
    shape_coordinates<-leaflet.draw$geometry$coordinates[[1]]  
    drawn_shape <- Polygon(do.call(rbind,lapply(shape_coordinates,function(x){c(x[[1]][1],x[[2]][1])})))
    drawn_shape <- SpatialPolygons(list(Polygons(list(drawn_shape),"drawn_shape")),proj4string = mycrs)
    return(drawn_shape)
    
  }else if(type == "Point"){
    
    shape_coordinates<-cbind(leaflet.draw$geometry$coordinates[[1]][1],leaflet.draw$geometry$coordinates[[2]][1])
    drawn_shape <- SpatialPoints(coords =shape_coordinates,proj4string = mycrs)
    buffdist<-0.1
    drawn_shape<-spTransform(drawn_shape,trans.crs)
    drawn_shape<-buffer(drawn_shape,buffdist)
    drawn_shape<-spTransform(drawn_shape,mycrs)
    
    return(drawn_shape)
    
  }else if(type == "LineString"){
    
    shape_coordinates<-leaflet.draw$geometry$coordinates
    drawn_shape<-list(Lines(Line(do.call(rbind,lapply(shape_coordinates,function(x){c(x[[1]],x[[2]])}))),ID=1))
    drawn_shape<-SpatialLines(drawn_shape,proj4string = mycrs)
    buffdist<-0.1
    drawn_shape<-spTransform(drawn_shape,trans.crs)
    drawn_shape<-buffer(drawn_shape,buffdist)
    drawn_shape<-spTransform(drawn_shape,mycrs)
    return(drawn_shape)
    
  }else{
    return(NULL)
  }
}




get.drawn.with.buffer<-function(leaflet.draw,buffdist){
  type <- leaflet.draw$geometry$type
  library(sp)
  mycrs<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" )
  trans.crs <- CRS("+init=epsg:3857")
  
  
  if(type == "Polygon"){
    
    shape_coordinates<-leaflet.draw$geometry$coordinates[[1]]  
    drawn_shape <- Polygon(do.call(rbind,lapply(shape_coordinates,function(x){c(x[[1]][1],x[[2]][1])})))
    drawn_shape <- SpatialPolygons(list(Polygons(list(drawn_shape),"drawn_shape")),proj4string = mycrs)
    drawn_shape<-spTransform(drawn_shape,trans.crs)
    drawn_shape<-buffer(drawn_shape,buffdist)
    drawn_shape<-spTransform(drawn_shape,mycrs)
    return(drawn_shape)
    
  }else if(type == "Point"){
    
    shape_coordinates<-cbind(leaflet.draw$geometry$coordinates[[1]][1],leaflet.draw$geometry$coordinates[[2]][1])
    drawn_shape <- SpatialPoints(coords =shape_coordinates,proj4string = mycrs)
    if(buffdist==0){
      buffdist<-10
      drawn_shape<-spTransform(drawn_shape,trans.crs)
      drawn_shape<-buffer(drawn_shape,buffdist)
      drawn_shape<-spTransform(drawn_shape,mycrs)
      
      return(drawn_shape)
      
    }
    drawn_shape<-spTransform(drawn_shape,trans.crs)
    drawn_shape<-buffer(drawn_shape,buffdist)
    drawn_shape<-spTransform(drawn_shape,mycrs)
    return(drawn_shape)
    
  }else if(type == "LineString"){
    
    shape_coordinates<-leaflet.draw$geometry$coordinates
    drawn_shape<-list(Lines(Line(do.call(rbind,lapply(shape_coordinates,function(x){c(x[[1]],x[[2]])}))),ID=1))
    drawn_shape<-SpatialLines(drawn_shape,proj4string = mycrs)
    
    if(buffdist==0){
      buffdist<-10
      drawn_shape<-spTransform(drawn_shape,trans.crs)
      drawn_shape<-buffer(drawn_shape,buffdist)
      drawn_shape<-spTransform(drawn_shape,mycrs)
      return(drawn_shape)
      
    }
    
    drawn_shape<-spTransform(drawn_shape,trans.crs)
    drawn_shape<-buffer(drawn_shape,buffdist)
    drawn_shape<-spTransform(drawn_shape,mycrs)
    return(drawn_shape)
    

  }else{
    return(NULL)
  }
}


extract.buffer<-function(vector){
  library(raster)
  parcellist<-over(parcel.pnts,vector,returnList = F)
  parcellist<-parcellist[!is.na(parcellist)]
  parcellist<-as.numeric(names(parcellist))
  parcellist<-parcel.pnts@data[parcellist,]
  # parcellist <- parcellist$pnu
  return(parcellist)
}

