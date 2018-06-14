library(magrittr)

# Helpers -----------------------------------------------------------------

testIf <- function(msg, expr) {
    message('Testing if ',msg,'...')
    expr
    message('OK\n')
}

`support the same classes` <- function(genericFunNameAsString1,
									   genericFunNameAsString2,
									   ignore =
									   	list(character(0),
									   		 character(0))) {
	list. <- function(x,...) list(...)
	.Names <-
		c(genericFunNameAsString1,
		  genericFunNameAsString2)
	SupportedClasses <-
		.Names %>%
		lapply(. %>% autoshiny:::supportedClasses() %>% sort) %>%
		mapply(setdiff,
			   ., ignore,
			   SIMPLIFY=FALSE) %>%
		list.(ok =
			  	all(.[[1]]==.[[2]]),
			  info =
			  	structure(., .Names=.Names))
}

withSepLines <- function(txt)
	paste0('\n',txt,'\n')

listToTxt <- function(List)
	names(List) %>%
	paste0((max(nchar(.)) - nchar(.)) %>% sapply(. %>% rep.int(" ",.) %>% paste(collapse="")),
		  ' : ',
		  sapply(List,
		  	   . %>% paste(collapse=', '))) %>%
	paste(collapse='\n')

# Tests -------------------------------------------------------------------

testIf("`makeOutput` and `render` S3 methods support the same classes", {
	test.result <-
		`support the same classes`('makeOutput','render',
		                           ignore=list(character(0),
		                                       'as.data.frame'))
	if (!test.result$ok)
		stop(test.result$info %>% listToTxt %>% withSepLines)
})

testIf("`makeInput` and `default` S3 methods support the same classes", {
	test.result <-
		`support the same classes`('makeInput','default',
								   ignore=
								   	list(character(0),
								   		 c('default','stringsAsFactors')))
	if (!test.result$ok)
		stop(test.result$info %>% listToTxt %>% withSepLines)
})

