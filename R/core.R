#' @importFrom shiny as.shiny.appobj
#' @importFrom utils methods

isFileInput <- function(x)
    is.data.frame(x) &&
    all(c('name','size','type','datapath') %in% colnames(x))

bq <- function(x)
    paste0("`",x,"`")

unq <- function(x)
    sub('^`(.*)`$','\\1',x)

makeCode <- function(fun, funname, withGoButton) {
    if (!is.function(fun))
        stop('Argument/parameter `fun` must be a function,\n',
             'but ',bq(funname),' is not a function.',
             call.=FALSE)
    if (!is.logical(withGoButton) || !(withGoButton %in% c(TRUE,FALSE)))
        stop('Argument/parameter `withGoButton` must be either TRUE or FALSE.',
             call.=FALSE)
    .formals <- formals(fun)
    fun_has_args <- !is.null(.formals)
    if (fun_has_args) {
        formals(fun) <-
            suppressWarnings(sapply(names(.formals),
                                    function(x) {
                                        message('Evaluating ',
                                                bq(funname),"\u2019s argument/parameter ",
                                                bq(x),'...')
                                        tryCatch(eval(.formals[[x]]),
                                                 error =
                                                     function(e)
                                                         stop(geterrmessage(),
                                                              call.=FALSE))
                                    },
                                    simplify=FALSE))
        .args <- withArgNames(formals(fun))
        checkClasses(.args,
                     'makeInput')
        .args.classes <-
            classes(.args)
        message('\nChecking if Shiny inputs can be coerced to\n',
                'the data types/classes expected by ',bq(funname),'\n',
                'as arguments/parameters...')
        sapply(.args.classes,
               function(x) {
                   .coerc.funname <- paste0('as.',x)
                   if (!exists(.coerc.funname))
                       stop(bq(.coerc.funname),' does not exist.',
                            call.=FALSE)
                   if (!is.function(get(.coerc.funname)))
                       stop('Function ',bq(.coerc.funname),' does not exist.',
                            call.=FALSE)
               })
        coerce <- 
            function(InputVal, DestinClass, .args) bquote({
                InputVal <- .(InputVal)
                if (class(InputVal)==.(DestinClass) && !..isFileInput(InputVal))
                    InputVal
                else if (is.null(InputVal))
                    .(default(.formals[[getArgName(.args)]]))
                else if (..isFileInput(InputVal))
                    .(`if`(DestinClass=='data.frame',
                           quote(read.csv(InputVal$datapath, check.names=FALSE)),
                           quote(InputVal)))
                else .(as.symbol(paste0('as.',DestinClass)))(InputVal)
            })
        coerceInputs <- 
            function(.args) {
                if (is.list(.args) && !is.data.frame(.args))
                    lapply(withArgNames(.args),
                           function(x) coerceInputs(x))
                else coerce(bquote(input[[.(getArgName(.args))]]),
                            classes(list(.args)),
                            .args)
            }
        .CoercedInputs <- coerceInputs(.args)
        formals(fun) <- .formals
    }
    message('\nTest run of function ',bq(funname),
            '\nwith the default argument values...')
    returns <-
        do.call(fun,
                `if`(fun_has_args,
                     lapply(.args, default),
                     list()))
    # formals(fun) <- .formals
    message('\nCompiling the Shiny app code...')
    returnsList <-
        withArgNames(list(Output=returns))
    lapply(c('makeOutput','render'),
           checkClasses,
           .args=returnsList)
    .value <-
        bquote(do.call(.(as.symbol(funname)),
                       .(`if`(fun_has_args,
                              bquote(..coerceInputs(reactiveValuesToList(input))),
                              list()))))
    list(
        ui =
            bquote(
                fluidPage(
                    titlePanel(.(funname)),
                    sidebarLayout(
                        .(if (fun_has_args)
                            bquote(sidebarPanel(
                                list(h4('Inputs'),
                                     .(lapply(.args,
                                              makeInput)))
                            ))),
                        mainPanel(.(if (withGoButton)
                            quote(actionButton('..go', "Go, (re)calculate!"))),
                            conditionalPanel(condition = "input['..go'] != 0 && !($('html').hasClass('shiny-busy'))",
                                             .(makeOutput(returnsList))),
                            conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                             tags$div(br(), "Please wait... ",
                                                      tags$img(src='https://media.giphy.com/media/3o7TKtnuHOHHUjR38Y/giphy.gif',
                                                               alt="loader",
                                                               style="width: 4em; height: 4em;"))))
                    )
                )
            ),
        server =
            bquote(
                function(input, output) {
                    .(funname) <- .(fun)
                    .(`if`(fun_has_args,
                           bquote(..coerceInputs <- function(input) .(.CoercedInputs)),
                           bquote()))
                    ..isFileInput <- .(isFileInput)
                    File <- .(File)
                    ..value <-
                        .(if (withGoButton)
                            bquote(eventReactive(input[['..go']], .(.value)))
                          else bquote(function() .(.value)))
                    .(lapply(returnsList,
                             function(x) render(x, quote(..value()))))
                }
            )
    )
}

#' Create a Shiny app (object or files) from an R function
#' 
#' @param fun A function 
#' (preferably a symbol -- a long self-explanatory name -- pointing to a pre-defined
#' function object, rather than an anonymous function) 
#' with zero or more arguments/parameters. \strong{Every argument must have a default value},
#' which will be used to define each argument's:
#' \itemize{
#'   \item type/class,
#'   \item allowed values,
#'   \item pre-selected/start-up value.
#' }
#' @param withGoButton Either \code{TRUE} or \code{FALSE} (default: \code{FALSE}).
#' It indicates if the (re)evaluation of \code{fun} in the Shiny app should be immediately
#' triggered  by every change in the value of any argument/parameter
#' (\code{withGoButton} = \code{FALSE}) or if the (re)calculation should be started
#' only when a specific button is pressed (\code{withGoButton} = \code{TRUE}).
#' The latter is preferred if the (re)evaluation of \code{fun} is significantly
#' time-consuming or if \code{fun} has no arguments (because then, without the button,
#' only refreshing the web page would trigger the (re)evaluation).
#' @param directory Path to a directory/folder where \code{makeFiles} should save the
#' compiled \code{server.R} and \code{ui.R} files.
#' @return 
#' \describe{
#'   \item{makeApp}{A Shiny app object as returned by \code{\link[shiny]{as.shiny.appobj}}.}
#'   \item{makeFiles}{\code{NULL}. This function saves two plain text files:
#' \code{ui.R} and \code{server.R} with the R code of function \code{fun}
#' translated into a Shiny app. If these files need further manual changes,
#' it is recommended that they are first re-formatted e.g. in RStudio
#' (top menu -> Code -> Reformat Code or Ctrl+Shift+A) or programmatically
#' (e.g. \url{https://github.com/google/rfmt}).}
#' }
#' @examples
#' \dontrun{
#' library(shiny)
#' 
#' ### Example 1: Trivial anonymous function
#' makeApp(function(x=1:3, y=5:9) x+y)
#' 
#' ### Example 2: Nicer function and argument names
#' `Histogram for normal distribution` <-
#'     function(`Number of observations` =
#'              # as.integer => the argument interpreted as categorical:
#'              as.integer(c(100,10,1000)))
#'         # Generic R plots as "return values" are supported:
#'         plot(hist(rnorm(`Number of observations`)))
#' makeApp(`Histogram for normal distribution`)
#' 
#' ### Example 3: Data frame in (upload CSV), data frame out (displayed and downloadable as CSV)
#' `Table of sin and cos values` <-
#'     function(`Upload CSV file with column "x"` =
#'                  data.frame(x = seq(0, 2*pi, .25))) {
#'         dta <- `Upload CSV file with column "x"`
#'         data.frame(X = dta$x,
#'                    `Sin of X` = sin(dta$x),
#'                    `Cos of X` = cos(dta$x),
#'                    check.names = FALSE)
#'     }
#' makeApp(`Table of sin and cos values`)
#' 
#' ### Example 4: Arbitrary input and output files
#' openxlsx::write.xlsx(data.frame(x=1:5,
#'                                 y=11:15),
#'                      'my_test_file.xlsx')
#' `Excel file in and out` <-
#'     function(`Input Excel file` =
#'                  File('my_test_file.xlsx')) { # File() obligatory here!
#'         my.data <- openxlsx::read.xlsx(`Input Excel file`)
#'         my.data2 <- within(my.data,
#'                            z <- x + y)
#'         openxlsx::write.xlsx(my.data2,
#'                              'my_test_file_2.xlsx')
#'         File('my_test_file_2.xlsx') # File() obligatory here too!
#'     }
#' makeApp(`Excel file in and out`)
#' 
#' ### Example 5: Using a button as a (re-)evaluation trigger
#' ### Use this option if:
#' ### - the evaluation of your functon takes time, so it should not be re-evaluated with every
#' ###   minor change of the value of inputs/arguments/parameter;
#' ### - the function is impure e.g. depends on some external data fetched internally and takes no
#' ###   arguments/parameters -- in such a case the function would be re-evaluated only through
#' ###   page refresh of the browser; the button is a faster and a more elegant solution.
#' `Get "GDP and main components" from Eurostat` <-
#'     function() {
#'         # Getting data from
#'         # http://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing ...
#'         # ... ?sort=1&file=data%2Fnama_10_gdp.tsv.gz
#'         x <- eurodata::importData('nama_10_gdp')
#'         head(x, 10)
#'     }
#' makeApp(`Get "GDP and main components" from Eurostat`,
#'         withGoButton = TRUE)
#' 
#' ### Example 6: Lists of inputs (arguments) and the output list (composite return value)
#' ### are always decomposed
#' `A function with lists everywhere` <-
#'     function(`First argument group,` = list(`number one` = 1:3,
#'                                             `number two` = letters[1:3]),
#'              `2nd arg group,` = list(`1st argument` = 11:14,
#'                                      `second arg.` = LETTERS[1:5]))
#'         list(`Some text` =
#'                  as.character(c(`First argument group,`$`number two`,
#'                                 `2nd arg group,`$`second arg.`)),
#'              `Some numbers` =
#'                  `First argument group,`$`number one` +
#'                  `2nd arg group,`$`1st argument`,
#'              `Even a ggplot2 chart` =
#'                  ggplot2::qplot(a,b,data=data.frame(a=1:20,b=log(1:20))))
#' makeApp(`A function with lists everywhere`)
#' }
#' @name make
NULL

#' @rdname make 
#' @export
makeApp <- function(fun, withGoButton=FALSE)
    as.shiny.appobj(evalAsTxtCode(makeCode(fun,
                                           unq(simpleDeparse(substitute(fun))),
                                           withGoButton)))

# #' Create Shiny app files from an R function
# #' 
# #' For the description of arguments/parameters, see the help file for
# #' \code{\link{makeApp}}.
#' @rdname make 
#' @export
makeFiles <- function(fun, withGoButton=FALSE, directory) {
    if (!dir.exists(directory))
        dir.create(directory)
    app_list <-
        makeCode(fun,
                 unq(deparse(substitute(fun))),
                 withGoButton)
    sapply(c('ui','server'),
           function(x) {
               fname <- paste0(x,'.R')
               message('Saving ',fname," ...")
               cat(c('library(shiny)',
                     simpleDeparse(app_list[[x]])),
                   file=paste0(sub('\\\\$|/$',"",directory),
                               '/',fname),
                   sep='\n')
           })
    message('Files have been saved in\n',directory)
}

# Helpers

classes <- function(.args)
    sapply(.args,
           function(x) {
               .class <- class(x)
               if ('function' %in% .class) 'function'
               else if ('NULL' %in% .class) 'NULL'
               else if ('data.frame' %in% .class) 'data.frame'
               else typeof(x)
           })

supportedClasses <- function(genericFunNameAsString)
    sub(paste0(genericFunNameAsString,'.'),
        "",
        row.names(attr(methods(genericFunNameAsString),
                       'info')))

checkClasses <- function(.args, genericFunNameAsString) {
    .args.classes <-
        classes(.args)
    supported <-
        .args.classes %in% supportedClasses(genericFunNameAsString)
    not.supported <-
        !supported
    if (any(not.supported))
        stop('The following ',
             tolower(sub('make',"",genericFunNameAsString)),
             ' types/classes are not supported:\n',
             paste(paste(names(.args)[not.supported],':',.args.classes[not.supported]),
                   collapse='\n'),
             call.=FALSE)
}

evalAsTxtCode <- function(bquoted_code)
    eval(parse(text=deparse(bquoted_code,
                            backtick=TRUE)))
