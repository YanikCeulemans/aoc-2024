mod Common {
    pub def readAllLinesOrCrash(filename: String): List[String] \ {FileReadWithResult} =
        match FileReadWithResult.readLines(filename) {
            case Err(IoError.IoError(kind, msg)) => bug!("${kind}: ${msg}")
            case Ok(lines) => lines
        }
}