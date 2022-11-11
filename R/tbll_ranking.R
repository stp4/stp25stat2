
 #: possible error in
 # 'rndr_percent(r$rel.freq * ': unused argument (return_as_vector = FALSE)
 # at tbll_ranking.R:250





#' Tbll_ranking
#'
#' Rangordnungen von Objekten koennen durch eine Transformation der Rangreihen in
#' Intervallskalierte Merkmale ueberfuehrt werden. Die Grundidee dieser Methode geht
#' auf Thurstone (1927) nach dem "Law of Categorical Judgement" zurueck.
#'
#' Dabei werden
#' die kumulierten Haeufigkeiten in Normalverteilte z-Werte uebergefuehrt und aus diesen
#' die Intervallskalierten Markmalsauspraegungen gebildet.
#'
#' Literatur: Bortz, J. & Doering, N. (2006). Forschungsmethoden und
#'  Evaluation fuer Human-und Sozialwissenschaftler (4. Auflage). Berlin: Springer. Seite 155
#'
#' @name Tbll_ranking
#' @param ... Weitere Argumente
#' @return Vector
#' @export

Tbll_ranking <- function(...,

                      na.action = na.pass,
                      include.percent = TRUE,
                      include.freq = TRUE,
                      include.mean = TRUE,
                      include.z = TRUE,
                      include.na = TRUE,

                      include.order = TRUE,
                      decreasing = TRUE,
                      digits.mean = 2) {

caption <- "Rangreihe"
note <-"Law of Categorical Judgement"
X <- stp25tools::prepare_data2(..., na.action = na.action)

  Rangreihe_default(
    items = X$data[X$measure.vars],
    caption = caption,
    note = note,
    include.percent = include.percent,
    include.freq = include.freq,
    include.mean = include.mean,
    include.z = include.z,
    include.na = include.na,
    groups = if( length(X$data[X$group.vars]) ==0) NULL else X$data[X$group.vars],
    order = include.order,
    decreasing = decreasing,
    digits.mean = digits.mean,
    input = NULL #c("ranking", "ordering")
  )

}




#' @rdname Tbll_ranking
#'
#' @param items data.frame
#' @param groups gruppen
#' @param input  Format der Items c("ranking", "ordering"),
#' @param caption,note,output an stp25output
#' @param include.percent,include.freq,include.mean,include.z,include.na was soll ausgewertet werden
#' @param digits.mean,order,decreasing sortierung
#' @param pattern intern gruppen
#'
Rangreihe_default <- function (items,
                               caption = "",
                               note = "",
                               include.percent = TRUE,
                               include.freq = TRUE,
                               include.mean = TRUE,
                               include.z = TRUE,
                               include.na = TRUE,
                               groups = NULL,
                               order = TRUE,
                               decreasing = TRUE,
                               digits.mean = 2,
                               input = NULL, #c("ranking", "ordering"),
                               pattern = "____")
{
  N <- nrow(items)
  rankings <-  NULL #  Rang <- 1. 2. 3. usw
  inpt <-  guess_input(items)


  if (!is.null(groups)) {
    nms <- names(groups)
    data_by_group <-
      split(inpt$items, groups, sep = pattern) #-- seperator fuer mehr als ein Faktor
    # r ist eine liste mit mean und freq
    rslt <- lapply(data_by_group,
                Calc_Rank,
                rankings = inpt$rankings,
                include.na = include.na,
                mylabels = stp25tools::get_label(inpt$items))

    res <- NULL

    for (i in names(rslt)) {
      if (!is.null(rslt[[i]])) {
        res_1 <- format_rank(
          rslt[[i]],
          include.mean, include.z,
          include.percent, include.freq,
          digits.mean, order,  decreasing
        )

        if (length(nms) == 1) {
          Group <- i
          names(Group) <- nms
        }
        else{
          Group <- reshape2::colsplit(i, pattern, nms)
        }

        res <- rbind(res, cbind(Group, res_1))
        rslt$mean <- rbind(cbind(rslt[[i]]$mean, Group),rslt$mean)

      }

    }
  }
  else{
    rslt  <-
      Calc_Rank(inpt$items,
                rankings = inpt$rankings,
                include.na = include.na,
                mylabels = stp25tools::get_label(inpt$items))
    res <- format_rank(
      rslt,
      include.mean, include.z,
      include.percent, include.freq,
      digits.mean, order, decreasing
    )
  }




  tbl <- prepare_output(
    res,
    caption = paste0(caption, " (N = ", N , ")"),
    note = note,
    N = N
  )

  rslt$rankings  <- rankings
  rslt$input <- inpt$input
  rslt$items <- inpt$items
  rslt$groups <- groups

  attr(tbl, "plot") <- rslt

  return(tbl)


}



#' cleanup_Rank
#'
#' Doppelte Einträge bereinigen
#'
#' @param x data.frame
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#'  dat <-  data.frame(
#' a = c(NA, "c", "a", NA,   "a", "a", "b", "a"),
#' b = c("c", NA, "b", NA,  "a", "a", "b", "b"),
#' c = c("a", "a", "b", NA,   NA, "a", "b", "c"),
#' d = c(NA, NA, NA, NA,     "a", "a", NA, "d"),
#' e = c(NA, "e", "a", NA,   "d",   NA, NA, "e")
#' )
#' cleanup_Rank(dat)
cleanup_Rank <- function(x, col.names =  names(x)) {
  lvl <-  unique(unlist(sapply(x, levels)))

  data <-  as.data.frame(t(apply(x, 1, function(y) {
    u <- unique(y)
    if (any(is.na(u)))
      u <- u[-which(is.na(u))]
    nu <- length(u)
    n <- length(y)

    if (nu < n)
      u <- c(u, rep(NA,  n - nu))
    u
  })),
  stringsAsFactors = FALSE)
  names(data) <- col.names
  attr(data, "levels") <- lvl
  data
}




#' @param items Data.frame
#' @param rankings welche Raenge
#' @param input c("ranking", "ordering")
#'
#' @return   list(RankByRow=RankByRow, rankings=rankings, input=input)
#' @noRd
guess_input <- function(items) {

  #message("in guess_input")

  if (is.data.frame(items)) {
    if (is.factor(items[[1]]) | is.character(items[[1]])) {
      input <- "ordering"
      rankings <-  seq_len(ncol(items))


      items <- transpose3(items)
    }
    else {
      input <- "ranking"
      rankings <- seq_len(max(
        unlist(
          lapply(items, max, na.rm=TRUE)),
        na.rm=TRUE))
    }
  }
  else {
    rankings <- NULL
    input <- NULL
  }
  list(
    items = items,
    rankings = rankings,
    input = input
  )
}


#' @noRd
format_rank <- function(r, include.mean, include.z,
                        include.percent, include.freq,
                        digits.mean,
                        order, decreasing) {

  res <- data.frame( Items = r$labels,
                     stringsAsFactors = FALSE )
  if (include.percent & include.freq)
    res <- cbind( res,
                  rndr_percent(r$rel.freq * 100, r$freq),
                  stringsAsFactors = FALSE)
  else if (include.percent)
    res <- cbind( res,
                  rndr_percent(r$rel.freq * 100),
                  stringsAsFactors = FALSE)
  else{
    res <- cbind(res, as.data.frame.array(r$freq))
  }


  if (include.mean & include.z)
    res <- cbind( res,
                  render_f(r$mean[, c( "mean","sd", "z.score")],
                                     digits.mean)
    )
  else if (include.z)
    res <- cbind( res,
                  render_f(r$mean[, "z.score"],
                                     digits.mean))
  else if (include.mean)
    res <- cbind( res,
                  render_f(r$mean[, c( "mean","sd")],
                                     digits.mean))

  if (order)
    res[order(r$mean[, "z.score"],
              na.last = TRUE,
              decreasing = decreasing),]
  else
    res
}


#' @param x dataframe
#' @param rankings levels
#'
#' @return matrix
#' @noRd

table_apply <- function(x, rankings) {
  tab <- t(sapply(x, function(x, ...) {
    table(factor(x, ...))
  },
  levels = rankings, simplify = TRUE))
  colnames(tab) <- paste0("R", colnames(tab))
  tab
}




#' @param x Items
#' @param sicherheit,q_wert Das LOCJ gilt nur wenn keine 0 vorhanden sind daher hier mit 99.9% wahrscheinlichkeit
#' @param n Anzahl
#'
#' @return list("freq",     "rel.freq", "z.value",  "mean",     "labels"  )
#' @noRd


Calc_Rank <-
  function(x,
           rankings,
           include.na=TRUE,
           sicherheit = .001,
           q_wert = qnorm(1 - sicherheit),
           n = nrow(x),
           mylabels=NULL
  ) {
    if (n < 1) { return(NULL) }

    if (!is.table(x)) {

      if ( include.na ) rankings <- c(rankings, ".NA")
      tbl <- table_apply(x, rankings)

      tbl[, ncol(tbl)] <- nrow(x) - rowSums(tbl)
      rel_feq <- prop.table(tbl, 1)
      x_mean <- sapply(x,  function(x) {
        x[is.na(x)] <- length(rankings)
        c(m = mean(x, na.rm = TRUE),
          sd = sd(x, na.rm = TRUE),
          min = min(x, na.rm = TRUE),
          max = max(x, na.rm = TRUE),
          median = median(x, na.rm = TRUE)
        )})
    }
    else
    {
      mylabels<- rownames(x)
      names(mylabels)<- mylabels
      rankings <- seq_len(ncol(x))
      tbl <- x
      rel_feq <- prop.table(tbl, 1)
      mm <- rel_feq * matrix(rep(1:ncol(rel_feq),
                                 each = nrow(rel_feq)),
                             nrow = nrow(rel_feq))
      mms <- rowSums(mm)
      x_mean <- rbind(m=mms, sd=NA, se=NA , min=NA, max=NA, median=NA)
    }

    # Das LOCJ gilt nur wenn keine 0 vorhanden sind daher hier mit 99.9% wahrscheinlichkeit
    # qnorm(.001)= -3.090232
    kum_feq <- t(apply(rel_feq, 1, cumsum))
    kum_feq[which(kum_feq[, ] == 0)] <- sicherheit
    kum_feq[which(kum_feq[, ] == 1)] <- 1 - sicherheit

    #The Normal Distribution
    z.wert <- qnorm(kum_feq[,-ncol(kum_feq)])
    # z.wert[which(is.infinite(z.wert))] <- qnorm(1-sicherheit)  # NA

    zeilen.sum <- rowSums(z.wert, na.rm = TRUE)
    zeilen.mittel <- zeilen.sum / (ncol(z.wert))
    z_score <- (mean(zeilen.mittel, na.rm = T) - zeilen.mittel) * -1

    my_mean <- data.frame(Items=mylabels,
                          mean = x_mean[1L, ],
                          sd = x_mean[2L, ],
                          se = x_mean[2L, ] / sqrt(n),
                          min = x_mean[3L, ],
                          max = x_mean[4L, ],
                          median = x_mean[5L, ],
                          z.score = z_score,
                          # z.rang = length(rankings) - ((z_score + q_wert) /
                          #(q_wert * 2) * (length(rankings) - 1)),
                          stringsAsFactors = FALSE

    )
    list(
      freq = tbl,
      rel.freq = rel_feq,
      z.value = cbind(z.wert,
                      z.sum = zeilen.sum,
                      z.mittel = zeilen.mittel),
      mean = my_mean,
      labels = mylabels
    )

  }



#' @rname Tbll_ranking
#' @examples
#'
#' #' require(stp25tools)
#' #'
#' #'
#' nlv <- 5
#' n <- 2 * 3 * nlv * 1
#' set.seed(n)
#'
#' DF <-
#'   data.frame(
#'     Geschlecht = gl(2, n / 2, labels = c("Maennlich", "Weiblich")),
#'     Alter = gl(4, n / 4,   labels = c("20-29", "30-39", "40-49", "50-59")),
#'     Landwirtschaft = gl(2, n / 2, labels = c("konventionell", "biologisch"))
#'   )
#'
#' Attribute <-
#'   as.data.frame(t(apply(matrix(NA, ncol = n, nrow = 5), 2,
#'                         function(x)
#'                           sample.int(5))))
#'
#' Attribute[1, ] <- c(5, 1, 4, 2, 3)
#' Attribute[2, ] <- c(5, 1, 4, 2, 3)
#' Attribute[3, ] <- c(5, 2, 4, 3, 1)
#' Attribute[4, ] <- c(5, 1, 4, 3, 2)
#' Attribute[5, ] <- c(5, 1, 4, 3, 2)
#'
#' Attribute[21, ] <- c(1, 2, 5, 4, 3)
#' Attribute[22, ] <- c(1, 4, 5, 3, 2)
#' Attribute[23, ] <- c(2, 5, 1, 4, 3)
#' Attribute[24, ] <- c(1, 4, 2, 5, 3)
#' Attribute[25, ] <- c(1, 4, 3, 5, 2)
#'
#' attribute  <- c("Verfuegbarkeit",
#'                 "Vielfalt",
#'                 "Qualitaet",
#'                 "Geschmack",
#'                 "Preis")
#'
#' Attribute<- dapply2(Attribute, function(x) factor(x, 1:5, attribute))
#'
#' DF <- cbind(DF, Attribute)
#'
#' head(DF)
#' #' "Produkte aus konventioneller und biologischer  Landwirtschaft"
#'
#' #prepare_output<- stp25stat2::prepare_output
#' res_ranking <-
#'   Tbll_ranking( ~ V1+V2+V3+V4+V5,
#'              DF, include.percent=FALSE, include.order=FALSE, include.na=FALSE)
#'
#' res_ranking
#' attr(res_ranking, "plot")$mean
#'
#' library(PlackettLuce)
#'
#'  R <- as.rankings(attr(res_ranking, "plot")$items, attr(res_ranking, "plot")$input)
#'  mod <- PlackettLuce( R )
#'  coef(mod)
#'  summary(mod)
#'  res_ranking$pc <-
#'    round(coef(mod, log = FALSE) ,2)
#'  res_ranking$log.pc <- round(coef(mod, log = TRUE) ,2)
#'  res_ranking[order(res_ranking$pc,  decreasing=TRUE),]
#'
#'
#'
#' n <- 2 * 3 * 4 * 1
#'
#' set.seed(n)
#' kaffee <- c("Guatemala", "Vietnam", "Honduras", "Äthiopien")
#' sex<- c("male", "female")
#' age<- c("20-29", "30-39", "40-49", "50-59")
#' kaffe<- c("Espresso", "Filterkaffee", "Milchkaffee")
#'
#' DF <-
#'   data.frame(
#'     sex = factor("male",sex),
#'     Alter = factor("20-29",age ),
#'     Kaffeeform = factor("Espresso", kaffe),
#'     R1 = factor(kaffee[1], kaffee),
#'     R2 = factor(kaffee[2], kaffee),
#'     R3 = factor(kaffee[3], kaffee),
#'     R4 = factor(kaffee[4], kaffee)
#'   )
#'
#' Kaffee<- rbind(DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF,DF)
#'
#' for(i in 1:n){
#'   Kaffee<- rbind(Kaffee,
#'              c(sample(sex)[1],
#'                sample(age)[1],
#'                sample(kaffe)[1],
#'                sample(kaffee)
#'              ))
#' }
#'
#' x<-Tbll_ranking(Kaffee, R1, R2, R3, R4, by=~sex,  include.percent=FALSE, output=FALSE)
#' x
#'
#' dat<- attr(x, "plot")$mean
#' dat
#' # lattice::dotplot( reorder(Items, mean)~ z.score|"Kaffee",
#' #                   dat,
#' #                   groups=Group , xlab="",
#' #                 # xlim=range(dat$mean)*1.10 ,
#' #                  auto.key=list(), cex=1)
#'
#'
#' attr(x, "plot")$items
#' attr(x, "plot")$input
#' R <- as.rankings(attr(x, "plot")$items,
#'                  attr(x, "plot")$input)
#' mod <- PlackettLuce( R )
#' mod
#' coef(mod)
#' summary(mod)
#' # x$pc <-
#' #   round(coef(mod, log = FALSE) ,2)
#' # x$log.pc <- round(coef(mod, log = TRUE) ,2)
#' # x[order(x$pc,  decreasing=TRUE),]
#' #


