# From: https://www.r-bloggers.com/winsorization/
winsor1 <- 
function (x, fraction=.05)
{
  if(length(fraction) != 1 || fraction < 0 ||
     fraction > 0.5) {
    stop("bad value for 'fraction'")
  }
  lim <- quantile(x, probs=c(fraction, 1-fraction))
  x[ x < lim[1] ] <- lim[1]
  x[ x > lim[2] ] <- lim[2]
  x
}


winsor2 <- function (x, multiple=3)
{
  if(length(multiple) != 1 || multiple <= 0) {
    stop("bad value for 'multiple'")
  }
  med <- median(x, na.rm = TRUE)
  y <- x - med
  sc <- mad(y, center=0, na.rm = TRUE) * multiple
  y[ y > sc ] <- sc
  y[ y < -sc ] <- -sc
  y + med
}

# From: https://gist.github.com/jcheng5/5913297
helpPopup <- function(title, content,
                      placement=c('right', 'top', 'left', 'bottom'),
                      trigger=c('click', 'hover', 'focus', 'manual')) {
  tagList(
    singleton(
      tags$head(
        tags$script("$(function() { $(\"[data-toggle='popover']\").popover(); })")
      )
    ),
    tags$a(
      href = "#", class = "btn btn-mini", `data-toggle` = "popover",
      title = title, `data-content` = content, `data-animation` = TRUE,
      `data-placement` = match.arg(placement, several.ok=TRUE)[1],
      `data-trigger` = match.arg(trigger, several.ok=TRUE)[1],
      #tags$i(class="icon-question-sign")
      # changed based on http://stackoverflow.com/questions/30436013/info-bubble-text-in-a-shiny-interface
      icon("question")
    )
  )
}


userDataGen <- function() {
  require("MASS")
  cat(file = stderr(), 'generate data \n')
  # assign result to shared 'dataIn' variable
  loc.x <-
    rbind(mvrnorm(10, c(0, 0), matrix(c(1, 0.9, 0.9, 1), 2, 2)),  mvrnorm(10, c(4, 0), matrix(c(1,-0.9,-0.9, 1), 2, 2)))
  loc.x <- cbind(loc.x, matrix(rnorm(20 * 18), 20, 18))
  rownames(loc.x) = c(paste("A", sprintf("%02d", 1:10), sep = ""), paste("B", sprintf("%02d", 1:10), sep = ""))
  colnames(loc.x) =  LETTERS[1:20]
  
  return(loc.x)
}
