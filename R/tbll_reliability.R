#' Tbll_reliability
#'
#'  Cronbachs Alpha oder einfach nur α ist ein Maß für die interne Konsistenz einer Skala.
#'
#'  > .9	Exzellent
#'
#'  > .8	Gut / Hoch
#'
#'  > .7	Akzeptabel
#'
#'  > .6	Fragwürdig
#'
#'  > .5	Schlecht / Niedrig
#'
#'  < .5	Inakzeptabel
#'
#' Quelle http://statistikguru.de/spss/reliabilitaetsanalyse/auswerten-und-berichten-2.html
#'
#' Um die interne Konsistenz zu bestimmen, wurde Cronbachs Alpha für die Subskala positiver Affekt (insgesamt zehn Fragen) berechnet. Die interne Konsistenz war hoch, mit Cronbachs Alpha = .89 für positiven Affekt.
#'
#'
#' For reliability analysis, Cronbach’s alpha was calculated to assess the internal consistency of the subscale for positive affect, which consists of ten questions. The internal consistency of the questionnaire is satisfying, with Cronbach’s alpha for positive affect = .89.






#' @param include.inter.item.correlation,include.Alpha cronbachs Alpha
#' @param x,type,names ... Data, Formula
#' @param include.label Labels
#' @param include.item_statistics Item Statistic
#' @param include.scale_statistics  Scale Statistic
#' @param revcoded,check.keys,keys numeric oder logikal TRUE entspricht check.keys = TRUE
#' @param na.rm an psych::alpha
#' @param digits Digits
#'
#' @return list ore data.frame
#' @export
#'
#' @examples
#'
#' dat <- data.frame(
#'   x = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
#'   y = c(2, 2, 3, 4, 4, 3, 4, 5, 4, 2),
#'   z = c(3, 5, 1, 2, 3, 2, 3, 4, 4, 5),
#'   w = c(4, 2, 3, 5, 2, 4, 5, 4, 1, 1)
#' )
#'
#' Tbll_reliability(dat,
#'                  x,
#'                  y,
#'                  z,
#'                  w,
#'                  revcoded = TRUE,
#'                  include.item_statistics = FALSE)
#'
#' m1 <-
#'   Tbll_reliability(dat,
#'                    x,
#'                    y,
#'                    z,
#'                    w,
#'                    revcoded = TRUE,
#'                    include.item_statistics = FALSE)
#' m2 <-  Tbll_reliability(dat, x, y, z, w)
#'
#' Tbll_Alpha(m1, m2)
#'
#' \donttest{
#' require(psych)
#'
#' keys <-
#'   list(agree=c("-A1","A2","A3","A4","A5"),
#'        conscientious=c("C1","C2","C3","-C4","-C5"),
#'        extraversion=c("-E1","-E2","E3","E4","E5"),
#'        neuroticism=c("N1","N2","N3","N4","N5"),
#'        openness = c("O1","-O2","O3","O4","-O5"))
#'
#' scores <- psych::scoreItems(keys,bfi,min=1,max=6) #specify the minimum and maximum values
#' scores$alpha
#' summary(scores)
#' head(scores$scores)
#'
#'  }
#'
Tbll_reliability <- function(...) {
  UseMethod("Tbll_reliability")
}


#' @rdname Tbll_reliability
#' @export
Tbll_reliability.default <-
  function(...,
           include.label = TRUE,
           include.item_statistics = TRUE,
           include.scale_statistics = TRUE,
           include.Alpha = TRUE,
           include.inter.item.correlation = FALSE,
           revcoded = FALSE,
           check.keys = NULL,
           keys = revcoded,
           na.rm =TRUE,
           digits = 2) {

    X <- stp25tools::prepare_data2(...)
    n <- length(X$measure.vars)
    N <- nrow(X$data)

    if (!include.label)
      X$row_name <- X$measure.vars

    if (length(X$measure.vars) > 1)   {
      X$data[X$measure.vars] <-
        stp25tools:::dapply1(X$data[X$measure.vars])

      rslt <- item_statistik(X$data[X$measure.vars],
                             check.keys = check.keys,
                             keys = keys,
                             na.rm = na.rm)
      rslt <- skala_statistik(rslt)
      rslt$labels <- X$row_name

      item <-
        data.frame(
          Items = paste0(rslt$labels, ifelse(rslt$keys < 0, " (-)", "")),
          n = rslt$item_statistik$n,
          M = render_f(rslt$item_statistik$m, 2),
          SD = render_f(rslt$item_statistik$sd, 2),
          Trennschaerfe = render_f(rslt$psych$item.stats$r.drop, 2)
        )

      aplha_statistik <-
        with(
          rslt,
          data.frame(
            Items = Items,
            n = n,
            M = render_f(M, 2),
            SD = render_f(SD, 2),
            Range = paste(render_f(range, 2), collapse = "; "),
            Skew = render_f(Skew, 2),
            Kurtosi = render_f(Kurtosi, 2) ,
            Shapiro.Test = shapiro
          )
        )

      if (include.Alpha)
        aplha_statistik$Alpha <- render_f(rslt$Alpha, 2)

      if (include.inter.item.correlation)
        aplha_statistik$inter.item.correlation <-
        render_f(performance::item_intercor(rslt$data), 2)

      item_statistics <-
        prepare_output(item, caption = "Itemstatistiken", N = N)

      scale_statistics  <-
        prepare_output(aplha_statistik, caption = "Item-Mittelwerte", N = N)

      if (include.item_statistics & include.scale_statistics) {
        rslt <-  list(
          item_statistics = item_statistics,
          scale_statistics = scale_statistics,
          index = rslt$index,
          keys = rslt$keys
        )
        class(rslt) <- c(class(rslt), "psych_alpha")
        return(rslt)
      } else if (include.item_statistics) {
        return(item_statistics)
      } else{
        return(scale_statistics)
      }
    }
    else{
      x <- as.numeric(X$data[[1]])
      res_shapiro <- stats::shapiro.test(x)
      return(prepare_output(
        data.frame(
          Items = 1,
          n  = length(na.omit(x)),
          M  = render_f(mean(x, na.rm = na.rm), digits),
          SD = render_f(sd(x, na.rm = na.rm), digits),
          Range = paste(render_f(range(x, na.rm = na.rm), digits), collapse = "; "),
          #   Alpha = "n.a.",
          Skew    = render_f(psych::skew(x, na.rm = na.rm), digits),
          Kurtosi = render_f(psych::kurtosi(x , na.rm = na.rm), digits),
          shapiro = rndr_shapiro(res_shapiro$statistic, res_shapiro$p.value)
        )
      ),
      caption = "Item-Mittelwerte",
      N = N)
    }
  }


#' @rdname Tbll_reliability
#' @export
Tbll_reliability.psych <-
  function(x,
           digits = 2, ...) {
    if (!inherits(x, "alpha"))
      return(str(x))

    n = max(x$item.stats$n)

    item_statistics <-
      data.frame(
        Items = row.names(x$item.stats),
        n = x$item.stats$n,
        M = render_f(x$item.stats$mean, 2),
        SD = render_f(x$item.stats$sd, 2),
        Alpha.if.Deleted = render_f(x$item.stats$r.drop, 2)
      )


    scale_statistics <-
      data.frame(
        Items = length(x$keys),
        n = n,
        M = render_f(x$total$mean, 2),
        SD = render_f(x$total$sd, 2),
        Alpha = render_f(x$total$raw_alpha, 2)
      )

    scale_statistics  <-
      prepare_output(scale_statistics, caption = "Item-Mittelwerte", N = n)

    item_statistics <-
      prepare_output(item_statistics, caption = "Itemstatistiken", N = n)

    rslt <-
      list(
        item_statistics = item_statistics,
        scale_statistics = scale_statistics,
        index =  x$scores,
        keys = x$keys
      )
      class(rslt) <- c(class(rslt), "psych_alpha")
      rslt
  }




#' @rdname Tbll_reliability
#' @export
Tbll_Alpha <- function(...,
                       type = 1,
                       names = NULL) {
  if (is.null(names))
    names <-  paste(as.list(sys.call())[-1])
  skalen <- list(...)
  rslt <- NULL

  for (i in seq_along(skalen)) {
    if (inherits(skalen[[i]], "psych"))
      rslt <- rbind(rslt, .alpha_psych(skalen[[i]]))
    else{
      if (!is.data.frame(skalen[[i]]))
        skalen[[i]] <- skalen[[i]]$scale_statistics
      rslt <- rbind(rslt, skalen[[i]])
    }
  }

  prepare_output(cbind(Source = names, rslt),
                 caption = "Cronbach's alpha"
                )
}

.alpha_psych<- function(x){
  data.frame(
    Items = nrow(x$item.stats),
    n=NA,
    M = render_f(x$total$mean, 2),
    SD = render_f(x$total$sd, 2),
    Range =NA,
    Skew = NA,
    Kurtosi = NA ,
    Shapiro.Test = NA,
    Alpha = render_f(x$total$raw_alpha, 2)
  )
}


#'  es werden immer die Werte mit  check.keys = FALSE berechnet
#'  die Umcodierung wird dabei mit dappl1 erstellt.
#'
#' @noRd
item_statistik <- function(data,
                            check.keys = NULL,
                            keys = FALSE,
                            na.rm = TRUE) {
  # revcoded = keys = c(1,1,1,1,-1)
  if (is.numeric(keys) & length(keys) > 1){
    if( all(abs(keys) == 1) )
    keys <- which(keys == -1)
  }


  if (is.null(check.keys) & isFALSE(keys)) {
  #   cat("\n keine Vorgaben\n")
    keys <- rep(1, ncol(data))
    psych <- psych::alpha(data, check.keys = FALSE, na.rm = na.rm)
  }
  else  if (is.numeric(keys)) {
   #   cat("\n  keys vorgegeben\n")
    min.level <- min(data, na.rm = TRUE)
    max.level <- max(data, na.rm = TRUE)
    data[keys] <-
      stp25tools:::dapply1(data[keys],
                           function(x)
                             max.level + min.level - x)

    keys <-  ifelse(seq_along(data) %in% keys, -1, 1)
    psych <- psych::alpha(data, check.keys = FALSE, na.rm = na.rm)
  }
  else if (isTRUE(keys) | isTRUE(check.keys)) {
   #  cat("\n  keys prüfen\n")
    alp_check <-
      psych::alpha(data, check.keys = TRUE, warnings = FALSE, na.rm = na.rm)
    keys <- unlist(alp_check$keys)
    keys <-  ifelse(grepl("\\-", keys), -1, 1)

  #   print(keys)
    if ( any(keys) ) {
      min.level <- min(data, na.rm = TRUE)
      max.level <- max(data, na.rm = TRUE)
      revcoded <-  which( keys == -1 )
      cat("\nAutomatisch Umkodieren!\n")
      print(revcoded)
      cat("\n")

      data[revcoded] <-
        stp25tools:::dapply1(data[revcoded], function(x)
          max.level + min.level - x)
      psych <- psych::alpha(data, check.keys = FALSE, na.rm = na.rm)

    } else{
       # cat("\n nix Umzukodieren!\n")
      psych <- alp_check
    }
  }


  #- library Pych Version: 1.4.3
  #- Date:	 2014--March--25
  #- liefert falsche n (bei n>120 wird 122 ausgegeben)
  item_statistik <-
    list(
      m = sapply(data, mean, na.rm = TRUE),
      sd = sapply(data, sd, na.rm = TRUE),
      n = sapply(data, function(x)
        length(na.omit(x)))
    )


  Alpha <- as.numeric(psych$total$raw_alpha)

  list(
    data = data,
    keys = keys,
    psych = psych,
    item_statistik = item_statistik,
    Alpha = Alpha
  )
}


#' Die Funktion Tbll_desc_item() berechnet das gleiche nur mit der Summary() Funktion
#' @noRd
skala_statistik <- function(data) {
  data$index <- apply(
    data$data, 1,
    FUN = function(x)
      mean(x, na.rm = TRUE)
  )

  res_shapiro <- stats::shapiro.test(data$index)
  data$Items <- ncol(data$data)
  # data$N  <- nrow(data)
  data$n  <- length(na.omit(data$index))
  data$M  <- mean(data$index, na.rm = TRUE)
  data$SD <- sd(data$index, na.rm = TRUE)
  data$range <- range(data$index, na.rm = TRUE)
  data$Skew    <- psych::skew(data$index, na.rm = TRUE)
  data$Kurtosi <- psych::kurtosi(data$index , na.rm = TRUE)
  data$shapiro <-
    rndr_shapiro(res_shapiro$statistic, res_shapiro$p.value)

  data
}

