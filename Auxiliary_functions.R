getCurrentAspect <- function() {
  uy <- diff(grconvertY(1:2,"user","inches"))
  ux <- diff(grconvertX(1:2,"user","inches"))
  uy/ux
}