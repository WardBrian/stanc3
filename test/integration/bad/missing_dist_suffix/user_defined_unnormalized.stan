functions {
   real bar_lpdf(real x){
     return 1.0;
   }
}
model {
   target += bar_lupmf(19.2);
}
