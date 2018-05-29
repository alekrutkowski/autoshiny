render <- function (x, ...)
    UseMethod('render', x)

#' @export
render.character <- function(x, .value)
    bquote(output[[.(getArgName(x))]] <- renderText(paste(.(.value), collapse='\n')))

#' @export
render.integer <- function(x, .value)
    render.as.data.frame(x, .value)

#' @export
render.double <- function(x, .value)
    render.as.data.frame(x, .value)

#' @export
render.factor <- function(x, .value)
    render.as.data.frame(x, .value)

#' @export
render.logical <- function(x, .value)
    render.as.data.frame(x, .value)

#' @export
render.matrix <- function(x, .value)
    render.data.frame(x, .value)

#' @export
render.data.frame <- function(x, .value)
    bquote(list(output[[.(downloadId(getArgName(x)))]] <- .(tableDownload(x, .value)),
                output[[.(tableId(getArgName(x)))]] <- renderDataTable(.(withRowNames(.value)),
                                                                       escape=-1)))

#' @export
render.file <- function(x, .value)
    bquote(output[[.(downloadId(getArgName(x)))]] <-
               downloadHandler(filename =
                                   function()
                                       .(.value),
                               content =
                                   function(file)
                                       file.copy(.(.value), file)))

render.as.data.frame <- function(x, .value)
    render.data.frame(x, bquote(structure(as.data.frame(.(.value)),
                                          .Names=.(outputArgNameToName(getArgName(x))))))

#' @export
render.ggplot <- function(x, .value)
    render.NULL(x, .value) # render.NULL(print(x), .value)

#' @export
render.NULL <- function(x, .value)
    bquote(list(output[[.(downloadId(getArgName(x)))]] <- .(plotDownload(x, .value)),
                output[[.(plotId(getArgName(x)))]] <- renderPlot(.(.value))))

#' @export
render.list <- function(x, .value)
    `if`(is.data.frame(x), render.data.frame(x, .value),
         lapply(withArgNames(x),
                function(y) render(y, bquote(.(.value)[[.(outputArgNameToName(getArgName(y)))]]))))

withRowNames <- function(dframe)
    bquote({
        .dframe <- .(dframe) # to avoid calculating twice below
        cbind(data.frame("<sub>Row</sub>"=
                             .(makeRowNumsIfNull(quote(.dframe))),
                         check.names=FALSE),
              `if`(is.matrix(.dframe),
                   as.data.frame(.dframe, check.names=FALSE),
                   .dframe))
    })

makeRowNumsIfNull <- function(.dframe)
    bquote({
        .rn <- row.names(.(.dframe))
        paste0('<sub>',
               as.character(`if`(is.null(.rn), # as.char needed to ensure all column types are rendered correctly and not coerced by js
                                 seq_len(nrow(.(.dframe))),
                                 .rn)),
               '</sub>')
    })

tableDownload <- function(x, .value)
    bquote(downloadHandler(filename =
                               # function()
                                   .(paste0(tableId(getArgName(x)),'.csv')),
                           content =
                               function(file)
                                   write.csv(.(.value), file),
                           contentType =
                               'text/csv'))

plotDownload <- function(x, .value)
    bquote(downloadHandler(filename =
                               # function()
                                   .(paste0(plotId(getArgName(x)),'.png')),
                           content =
                               function(file) {
                                   png(file)
                                   v <- .(.value)
                                   if ('ggplot' %in% class(v)) print(v)
                                   dev.off()
                               },
                           contentType =
                               'image/png'))
