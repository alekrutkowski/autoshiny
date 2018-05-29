#' @import shiny
NULL

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
                    .(default(.args))
                else if (..isFileInput(InputVal))
                    .(`if`(DestinClass=='data.frame',
                           quote(read.csv(InputVal$datapath, check.names=FALSE)),
                           quote(InputVal)))
                else .(get(paste0('as.',DestinClass)))(InputVal)
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
    }
    message('\nTest run of function ',bq(funname),
            '\nwith the default argument values...')
    returns <-
        do.call(fun,
                `if`(fun_has_args,
                     lapply(.args, default),
                     list()))
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
                                bquote(
                                    list(h4('Inputs'),
                                         .(lapply(.args,
                                                  makeInput)))
                                )))),
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

#' Create a Shiny app object from an R function
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
#' @return A Shiny app object as returned by \code{\link[shiny]{as.shiny.appobj}}.
#' @export
makeApp <- function(fun, withGoButton=FALSE)
    as.shiny.appobj(evalAsTxtCode(makeCode(fun,
                                           unq(simpleDeparse(substitute(fun))),
                                           withGoButton)))

#' Create Shiny app files from an R function
#' 
#' For the description of arguments/parameters, see the help file for
#' \code{\link{makeApp}}.
#' 
#' @return \code{NULL}. This function saves two plain text files:
#' \code{ui.R} and \code{server.R} with the R code of function \code{fun}
#' translated into a Shiny app. If these files need further manual changes,
#' it is recommended that they are first re-formatted e.g. in RStudio
#' (top menu -> Code -> Reformat Code or Ctrl+Shift+A) or programmatically
#' (e.g. \url{https://github.com/google/rfmt}).
#' @export
makeFiles <- function(fun, withGoButton=FALSE) {
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
                   file=fname,
                   sep='\n')
           })
    NULL
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



# correctlyNamed <- function(List) {
# 	List.names <- names(List)
# 	correct.names <- make.names(List.names,
# 								unique=TRUE,
# 								allow_=TRUE)
# 	names(List) <-
# 		if (length(List.names)==0)
# 			paste0('X.',seq_len(List))
# 	else
# 	correct.names
# }

# correctlyNamed <- function(List) {
# 	List.names <- names(List)
# 	correct.names <- make.unique(List.names)
# 	names(List) <-
# 		if (length(List.names)==0)
# 			paste0('X.',seq_len(List))
# 	else
# 		correct.names
# }
