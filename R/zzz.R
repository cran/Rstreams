.First.lib <- function(lib, pkg) {
    if(Machine()$integer.max != 2147483647)
        stop("Current implementation assumes 32-bit integers")
    library.dynam("Rstreams", pkg, lib)
}
