allstreams <- function ()
{
    result <- integer(.C("streamcount", count = as.integer(0),
                      PACKAGE = "Rstreams")$count)
    result <- .C("getstreams", as.integer(length(result)), result = result,
                 PACKAGE = "Rstreams")$result
    class(result) <- "stream"
    result
}

closestream <- function (stream)
{
    if (is.character(stream) && charmatch(stream, "all") == 1)
        .C("closeallstreams", PACKAGE = "Rstreams")
    else .C("closestream", as.integer(stream), PACKAGE = "Rstreams")
    invisible()
}

copystream <- function (src, dest, nbytes)
    .C("copystream", as.integer(src), as.integer(dest),
       nbytes = as.integer(nbytes), PACKAGE = "Rstreams")$nbytes

getstreameol <- function(stream, bufsize = 256)
{
    ## Look for CR, LF, or CR/LF in stream

    startpos <- summary(stream)$position
    streamlen <- summary(stream)$size - startpos
    on.exit(seekstream(stream,startpos))
    result <- ""
    buffer <- character(0)
    while ((nchar(result) == 0) & (length(buffer) < streamlen))
    {
        buffer <- c(buffer,readchar(stream, bufsize, len=1))
        eol <- seq(len = length(buffer)-1)[buffer[-length(buffer)] %in% c('\r','\n')]
        if (length(eol) > 0)
        {
            if ((buffer[eol[1]] == '\r') & (buffer[eol[1]+1] == '\n'))
                result <- '\r\n'
            else
                result <- buffer[eol[1]]
            return(result)
        }
    }

    ## Might have missed the very last character

    if ((length(buffer) > 0) && (buffer[length(buffer)] %in% c('\r','\n')))
        result <- buffer[length(buffer)]
    result
}

hexdump <- function (s, len = summary(s)$size - summary(s)$position)
{
    startpos <- summary(s)$position
    stoppos <- min(startpos + len, summary(s)$size)
    if (stoppos > startpos) {
        startdump <- 16 * (startpos%/%16)
        while (startdump < stoppos) {
            stop <- min(startdump + 16, stoppos)
            bytes <- readint(s, stop - startpos, 1, signed = FALSE)
            seekstream(s, startpos)
            chars <- readchar(s, stop - startpos, 1)
            chars[bytes < 32] <- "."
            hex <- rep("  ", 16)
            hex[(startpos - startdump + 1):(stop - startdump)] <-
                inttostr(bytes, 2)
            hex <- paste(hex, collapse = " ")
            showchars <- rep(" ", 16)
            showchars[(startpos - startdump + 1):(stop - startdump)] <- chars
            showchars <- paste(showchars, collapse = "", sep = "")
            cat(inttostr(startdump, 4), ": ", hex, " ", showchars,
                "\n", sep = "")
            startpos <- startdump + 16
            startdump <- startpos
        }
    }
    invisible()
}

inttostr <- function (x, digits = 0, base = 16, complement = FALSE)
{
    digitchars <- c(0:9, LETTERS)
    signchar <- c("-", "", "")
    x <- as.integer(x)
    signs <- sign(x)
    if (complement & base^digits/2 >= max(c(x + 1, -x))) {
        x[signs < 0] <- x[signs < 0] + base^digits
        signs <- rep(1, length(x))
    }
    else x <- abs(x)
    result <- character(length(x))
    while (digits > 0 || any(x != 0)) {
        result <- paste(digitchars[x%%base + 1], result, sep = "")
        x <- x%/%base
        digits <- digits - 1
    }
    paste(signchar[signs + 2], result, sep = "")
}

openstream <- function (filename, mode = "read")
{
    mode <- charmatch(mode, c("read", "write"))
    if (mode == 0)
        stop("Stream mode must be \"read\" or \"write\"")
    handle <- .C("openstream", as.character(filename), as.integer(mode),
        handle = integer(1), PACKAGE = "Rstreams")$handle
    if (handle < 0)
        stop(paste("Error", handle, "opening", filename))
    class(handle) <- "stream"
    handle
}

print.stream <- function (x)
{
    if (length(x) > 0) {
        info <- summary.stream(x)
        for (i in 1:length(x)) {
            if (is.na(info$position[i]))
                cat("Stream not current\n")
            else
                cat(info$filename[i], " at ", info$position[i],
                " of ", info$size[i], " bytes (",
                    c("read", "write")[info$mode[i]],
                " mode)\n", sep = "")
        }
    }
    invisible(x)
}

readchar <- function (stream, n = 1, len = NA, bufsize = 256)
{
    if (is.na(len))
        readlines(stream,n,bufsize,eol='\0')
    else {
        res <- .C("readchar", handle = as.integer(stream),
                   as.integer(len), n = as.integer(n),
                   result = rep(paste(rep(" ", len), collapse = ""), n),
                   PACKAGE = "Rstreams")
        res$result[seq(len=res$n)]
    }
}

readcomplex <- function (stream, n = 1, size = 8, swapbytes = FALSE)
{
    res <- .C("readfloat", handle = as.integer(stream),
              n = as.integer(2 * n), as.integer(size), as.integer(0),
              as.logical(swapbytes), result = complex(n),
              PACKAGE = "Rstreams")
    res$result[seq(len=res$n/2)]
}

readfloat <- function (stream, n = 1, size = 8, swapbytes = FALSE)
{
    res <- .C("readfloat", handle = as.integer(stream), n = as.integer(n),
       as.integer(size), as.integer(0), as.logical(swapbytes),
       result = double(n), PACKAGE = "Rstreams")
    res$result[seq(len=res$n)]
}

readint <- function (stream, n = 1, size = 4, signed = TRUE, swapbytes = FALSE)
{
    if (size < 4 | (size == 4 & signed))
        res <- .C("readint", handle = as.integer(stream),
                  n = as.integer(n), as.integer(size), as.logical(signed),
                  as.logical(swapbytes), result = integer(n),
                  PACKAGE = "Rstreams")
    else
        res <- .C("readfloat", handle = as.integer(stream),
                  n = as.integer(n), as.integer(size),
                  as.integer(1 + as.logical(signed)),
                  as.logical(swapbytes), result = double(n),
                  PACKAGE = "Rstreams")
    res$result[seq(len=res$n)]
}

readlines <- function (stream, n = 1, bufsize = 256,
                       eol = getstreameol(stream, bufsize))
{
    if (n > 0) {
        eol <- as.character(eol)
        res <- .C("readlines", handle = as.integer(stream),
                  n = as.integer(n), as.integer(bufsize), eol,
                  result = character(n), PACKAGE = "Rstreams")
        res$result[seq(len=res$n)]
    } else character(0)
}

seekstream <- function (stream, offset, origin = "start")
{
    origin <- charmatch(origin, c("start", "current", "end"))
    .C("seek", handle = as.integer(stream),
                 offset = as.integer(offset),
                 as.integer(origin), PACKAGE = "Rstreams")$offset
}

summary.stream <- function (stream)
{
    .C("streaminfo", count = as.integer(length(stream)),
       handle = as.integer(stream),
       filename = as.character(rep(paste(rep(" ", 256), collapse = ""),
       length(stream))), mode = as.integer(stream),
       position = as.integer(stream),
       size = as.integer(stream), PACKAGE = "Rstreams")
}

truncate <- function (stream)
    .C("Rtruncate", handle = as.integer(stream),
       size = as.integer(0), PACKAGE = "Rstreams")$size

writechar <- function (stream, data, asciiz = FALSE)
{
    .C("writechar", handle = as.integer(stream),
       as.integer(length(data)),
       as.integer(asciiz), as.character(data), PACKAGE = "Rstreams")
    invisible()
}

writecomplex <- function (stream, data, size = 8, swapbytes = FALSE)
{
    .C("writefloat", handle = as.integer(stream),
       as.integer(2 * length(data)), as.integer(size),
       as.integer(0), as.logical(swapbytes),
       as.complex(data), PACKAGE = "Rstreams")
    invisible()
}

writefloat <- function (stream, data, size = 8, swapbytes = FALSE)
{
    .C("writefloat", handle = as.integer(stream),
       as.integer(length(data)), as.integer(size), as.integer(0),
       as.logical(swapbytes), as.double(data), PACKAGE = "Rstreams")
    invisible()
}

writeint <- function (stream, data, size = 4, swapbytes = FALSE)
{
    if (size <= 4)
        .C("writeint", handle = as.integer(stream),
           as.integer(length(data)), as.integer(size),
           as.logical(swapbytes), as.integer(data),
           PACKAGE = "Rstreams")
    else .C("writefloat", handle = as.integer(stream),
            as.integer(length(data)), as.integer(size),
            as.integer(1), as.logical(swapbytes), as.double(data),
            PACKAGE = "Rstreams")
    invisible()
}
