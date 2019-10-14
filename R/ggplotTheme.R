# from JASPgraphs
require(ggplot2)
require(grid)
# require(extrafont)
# you may need to call the line below once to run
# font_import()
# loadfonts(device = "win")

myTheme <- function(base_size = 17, base_family = NULL,#LM Roman 10",
                    legend.position = "none", legend.justification = "top") {

  force(legend.position)
  return(ggplot2::theme(
    # generics
    rect = ggplot2::element_rect(colour = "transparent", fill = "transparent", size = 1, linetype = 1),
    text = ggplot2::element_text(size = base_size, family = base_family),
    # axis
    axis.line         = ggplot2::element_blank(),
    axis.text         = ggplot2::element_text(),#family = family, size = base_size),
    axis.ticks.length = ggplot2::unit(0.3, "cm"),
    axis.title.x      = ggplot2::element_text(margin = ggplot2::margin(t = 15, b = 5)),
    axis.title.y      = ggplot2::element_text(margin = ggplot2::margin(r = 10, l = 5)),
    axis.text.x       = ggplot2::element_text(colour = "black", margin = ggplot2::margin(t = 7)),
    axis.text.y       = ggplot2::element_text(colour = "black", margin = ggplot2::margin(r = 7)),

    # legend
    legend.background     = ggplot2::element_rect(color = "transparent", fill = "transparent"),
    legend.box.background = ggplot2::element_rect(color = "transparent", fill = "transparent"),
    legend.justification  = legend.justification,
    legend.key            = ggplot2::element_rect(color = "transparent", fill = "transparent"),
    legend.key.size       = ggplot2::unit(1, "cm"),
    legend.title          = ggplot2::element_text(hjust = 0.5),
    legend.position       = legend.position,

    # panel
    panel.border     = ggplot2::element_blank(),
    panel.spacing    = ggplot2::unit(2, "cm"),
    panel.grid       = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(color = "transparent", fill = "transparent"),

    # plot
    plot.background = ggplot2::element_rect(fill = "transparent", color = "transparent"),
    plot.title      = ggplot2::element_text(hjust = 0.5), # center title

    # facets
    strip.background = element_rect(fill = "transparent", color = "transparent")
  ))
}

#' @importFrom grid gpar segmentsGrob gTree gList unit
# geom_rangeframe is adapted from ggthemes::geom_rangeframe, but it uses the panel_scales
# to compute the endpoints of the lines rather than the data (as ggthemes::geom_rangeframe does)

ggname <- function(prefix, grob) {
  # copy of ggthemes:::ggname
  grob$name <- grid::grobName(grob, prefix)
  grob
}

#' Title
#'
#' @description Axis lines which extend to the maximum and minimum of the axis breaks. The implementation and documentation is largely adapted from \code{\link{ggthemes::geom_rangeframe}}.
#'
#'@section Aesthetics:
#' \itemize{
#' \item colour
#' \item size
#' \item linetype
#' \item alpha
#' }
#'
#' @inheritParams ggplot2::geom_point
#' @param sides A string that controls which sides of the plot the frames appear on.
#'   It can be set to a string containing any of \code{'trbl'}, for top, right,
#'   bottom, and left.
#'
#' @references Tufte, Edward R. (2001) The Visual Display of Quantitative Information, Chapter 6.
#' @references Jeffrey B. Arnold (2019). ggthemes: Extra Themes, Scales and Geoms for 'ggplot2'. R package version 4.2.0. https://CRAN.R-project.org/package=ggthemes
#'
#' @export
#'
#' @example inst/examples/ex-geom_rangeframe.R
geom_rangeframe <- function(mapping = NULL,
                            data = NULL,
                            stat = "identity",
                            position = "identity",
                            ...,
                            sides = "bl",
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRangeFrame,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      sides = sides,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_rangeframe
#' @usage NULL
#' @format NULL
#' @export
GeomRangeFrame <- ggplot2::ggproto("GeomRangeFrame", ggplot2::Geom,
  optional_aes = c("x", "y"),

  draw_panel = function(data, panel_scales, coord, sides = "bl", lineend = "butt", color = "black",
                        alpha = 1, linetype = 1, size = 1, extendForAxisTicks = TRUE) {

    rugs <- list()
    data <- coord[["transform"]](data, panel_scales)
    gp <- gpar(col = scales::alpha(color, alpha),
               lty = linetype,
               lwd = size * ggplot2::.pt,
               lineend = lineend)

    adj <- unit(0, "native") # if (extendForAxisTicks) unit(getGraphOption("axisTickWidth") / 2, "mm") else unit(0, "native")

    if (grepl("b", sides)) {
      rugs[["x_b"]] <- ggname(
        "range_x_b",
        segmentsGrob(# x0 = unit((1/1.0001)*min(panel_scales$x.major), "native"),
                     # x1 = unit(1.001*max(panel_scales$x.major), "native"),
                     x0 = unit(min(panel_scales$x.major), "native") - adj,
                     x1 = unit(max(panel_scales$x.major), "native") + adj,
                     y0 = unit(0, "npc"),
                     y1 = unit(0, "npc"),
                     gp = gp))
    }

    if (grepl("t", sides)) {
      rugs[["x_t"]] <- ggname(
        "range_x_t",
        segmentsGrob(x0 = unit(min(panel_scales$x.major), "native"),
                     x1 = unit(max(panel_scales$x.major), "native"),
                     y0 = unit(1, "npc"),
                     y1 = unit(1, "npc"),
                     gp = gp))
    }

    if (grepl("l", sides)) {
      rugs[["y_l"]] <- ggname(
        "range_y_l",
        segmentsGrob(y0 = unit(min(panel_scales$y.major), "native"),
                     y1 = unit(max(panel_scales$y.major), "native"),
                     x0 = unit(0, "npc"),
                     x1 = unit(0, "npc"),
                     gp = gp))
    }

    if (grepl("r", sides)) {
      rugs[["y_r"]] <- ggname(
        "range_y_r",
        segmentsGrob(y0 = unit(min(panel_scales$y.major), "native"),
                     y1 = unit(max(panel_scales$y.major), "native"),
                     x0 = unit(1, "npc"),
                     x1 = unit(1, "npc"),
                     gp = gp))
    }
    ggname("geom_rangeframe", gTree(children = do.call("gList", rugs)))
  },
  default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),

  draw_key = ggplot2::draw_key_path
)
