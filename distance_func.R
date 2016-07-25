cosine_dist<-function(x,y){
        sum(x*y)/sqrt(sum(x*x))/sqrt(sum(y*y))
}

tanimoto_dist<-function(x,y){
        dxy<-sum(x*y)
        dx<-sum(x*x)
        
        dy<-sum(y*y)
        
        dxy/(dx+dy-dxy)
}


tanimoto_dist_fast<-function(x,y){
        
        dy<-sum(y*y)
        dx<-rowSums(x*x,1)
        dxy<-colSums(t(x)*y)
        
        dxy/(dx+dy-dxy)
}


tanimoto_dist_fast_transpose<-function(x,y){
        
        dy<-sum(y*y)
        dx<-colSums(x*x)
        dxy<-colSums(x*y)
        
        dxy/(dx+dy-dxy)
}
