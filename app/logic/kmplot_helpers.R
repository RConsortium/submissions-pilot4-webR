box::use(
  cowplot[plot_grid],
  dplyr[desc, intersect],
  ggplot2[
    aes, element_blank, element_text, geom_text, ggplot, ggplot_build,
    ggtitle, scale_x_continuous, theme, theme_bw, unit, xlab, ylab
  ],
  stats[reorder],
  # teal.data[get_labels],
  visR[align_plots, get_risktable],
)

get_labels <- function(data, fill = TRUE) {
  stopifnot(is.data.frame(data))
  checkmate::assert_flag(fill)

  column_labels <- Map(function(col, colname) {
    label <- attr(col, "label")
    if (is.null(label)) {
      if (fill) {
        colname
      } else {
        NA_character_
      }
    } else {
      if (!checkmate::test_string(label, na.ok = TRUE)) {
        stop("label for variable ", colname, " is not a character string")
      }
      as.vector(label) # because label might be a named vector
    }
  }, data, colnames(data))
  column_labels <- unlist(column_labels, recursive = FALSE, use.names = TRUE)

  list("dataset_label" = data_label(data), "column_labels" = column_labels)
}

#' @export
add_risktable2 <- function(gg,
                           times = NULL,
                           statlist = "n.risk",
                           label = NULL,
                           group = "strata",
                           collapse = FALSE,
                           rowgutter = .16,
                           risk_font_size = 6.0,
                           risk_label_font_size = 12,
                           ...) {
  if (!(is.numeric(rowgutter) == TRUE) || (rowgutter < 0) || (rowgutter > 1)) {
    stop("rowgutter should be a numeric value in range [0, 1]")
  }

  tidy_object <- gg$data
  estimate_object <- visR:::.extract_estimate_object(gg)

  ggbld <- ggplot_build(gg)

  graphtimes <- as.numeric(ggbld$layout$panel_params[[1]]$x$get_labels())

  if (is.null(times)) times <- graphtimes

  final <-
    get_risktable(estimate_object,
      times = times,
      statlist = statlist,
      label = label,
      group = group,
      collapse = collapse
    )

  times <- as.numeric(unique(final$time))
  statlist <- attributes(final)$statlist
  title <- attributes(final)$title

  attr(final, "time_ticks") <- NULL
  attr(final, "statlist") <- NULL
  attr(final, "title") <- NULL

  tbls <- Map(
    function(statlist, title = NA) {
      ggrisk <- ggplot(
        final,
        aes(
          x = time,
          y = reorder(y_values, desc(y_values)),
          label = format(get(statlist), nsmall = 0)
        )
      ) +
        geom_text(size = risk_font_size, hjust = 0.5, vjust = 0.5, angle = 0, show.legend = FALSE) +
        theme_bw() +
        scale_x_continuous(
          breaks = graphtimes,
          limits = c(min(graphtimes), max(graphtimes))
        ) +
        theme(
          axis.title.x = element_text(
            size = 8,
            vjust = 1,
            hjust = 1
          ),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_text(size = risk_label_font_size, colour = "black", face = "plain"),
          plot.margin = unit(c(1, 0, 0, 0), "lines"),
          plot.title = element_text(hjust = 0, vjust = 0),
          legend.position = "none"
        ) +
        xlab(NULL) +
        ylab(NULL)

      if (!is.na(title) && !is.null(title)) {
        ggrisk <- ggrisk +
          ggtitle(title) +
          theme(plot.title = element_text(size = 10))
      }

      return(ggrisk)
    },
    statlist = as.list(statlist),
    title = as.list(title)
  )

  gglist <- list(gg) |>
    append(tbls)

  gg_a <- gglist |>
    align_plots()

  gg_b <- plot_grid(
    plotlist = gg_a,
    align = "none",
    nrow = length(gg_a),
    rel_heights = c(1 - (rowgutter * (length(gg_a) - 1)), rep(rowgutter, length(gg_a) - 1))
  )

  class(gg_b) <- c(class(gg_b), intersect(class(gg), c("ggsurvfit", "ggtidycmprsk")))

  components <- append(list(gg), tbls)
  names(components) <- c("visR_plot", title)
  gg_b[["components"]] <- components

  return(gg_b)
}
