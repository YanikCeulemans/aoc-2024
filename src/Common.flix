mod Common {
    pub def readAllLinesOrCrash(filename: String): List[String] \ {FileReadWithResult} =
        match FileReadWithResult.readLines(filename) {
            case Err(IoError.IoError(kind, msg)) => bug!("could not read file due to IoError: ${kind}: ${msg}")
            case Ok(lines) => lines
        }
    
    pub def isEven(n: Int32): Bool = n == ((n / 2) * 2)

    pub def parMap(f: a -> b, xs: List[a]): List[b] =
        match xs {
            case Nil => Nil
            case x :: rest =>
                par (r <- f(x); rs <- parMap(f, rest))
                    yield r :: rs
        }
}