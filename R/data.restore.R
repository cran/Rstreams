data.restore <-
    function (filename, print = FALSE, verbose = FALSE, env = .GlobalEnv)
{
    dump <- openstream(filename)
    on.exit(closestream(dump))

    eol <- getstreameol(dump)

    ReadSdump <- function(top = FALSE, prefix) {
        name <- readlines(dump, eol = eol)
        code <- readlines(dump, eol = eol)
        len <- as.integer(readlines(dump, eol = eol))
        if (top && print)
            cat("\"", name, "\": ", len, " values of mode \"", code,
                "\"\n", sep="")
        if (verbose)
            cat(prefix, summary(dump)$position, name, code, len, "\n")
        if (code == "logical") {
            value <- as.logical(readlines(dump, len, eol = eol))
        }
        else if (code %in% c("numeric","integer","single")) {
            value <- as.numeric(readlines(dump, len, eol = eol))
        }
        else if (code %in% c("character", "name", "missing")) {
            value <- readlines(dump, len, eol = eol)
            if (code == "name")
            {
                value <- as.name(value)
            }
        }
        else if (code == "complex") {
            value <- as.complex(readlines(dump, len, eol = eol))
        }
        else if (code %in% c("list", "structure", "NULL", SModeNames)) {
            value <- list()
            if (len > 0) {
            	for (i in 1:len) {
                    temp <- ReadSdump(FALSE, c(prefix, " "))
                    if (temp$name != "")
                    	value[[temp$name]] <- temp$value
                    else value[[i]] <- temp$value
                }
            }
            if (code == "structure") {
                thelist <- value
                value <- thelist[[".Data"]]
                attributes(value) <-
                    thelist[-match(c(".Data", ".Dim", ".Dimnames", ".Label"),
                                   names(thelist), nomatch = 0)]
                dim(value) <- thelist[[".Dim"]]
                names(value) <- names(thelist[[".Data"]])
                if (!is.null(thelist[[".Label"]]))
                    levels(value) <- thelist[[".Label"]]
                if (!is.null(thelist[[".Dimnames"]]))
                    try(dimnames(value) <- thelist[[".Dimnames"]])
            }
            else if (code == "function")
                try(value <- as.function(value,env=env))
            else if (code %in% c("while", "if", "for", "<-", "(", "{"))
                value <- as.call(c(as.name(code),value))
            else if (code == "NULL") value <- as.name(name)
            else if (code == "call(...)")  # these aren't special in R
                value <- value[[1]]
            else if (code == "comment")   # ignore comments
                value <- NULL
            else if (code == "comment.expression")  # just keep the expression, not the comment
                value <- value[unlist(lapply(value,function(y) !is.null(y)))][[1]]
            else try(mode(value) <- code)
        }
        else {
            stop(paste("S mode \"", code, "\" (near byte offset ",
                       dump$position, ") not supported", sep=""))
        }
        list(name = name, value = value)
    }
    while (summary(dump)$position < summary(dump)$size) {
        temp <- ReadSdump(TRUE, " ")
        assign(temp$name, temp$value, env = env)
    }
    filename
}
