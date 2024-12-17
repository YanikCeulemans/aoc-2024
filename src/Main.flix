def timedApp(filename: String): Int32 \ {Logger, Diagnostics.GetDiagnosticTimeMark, FileReadWithResult} =
    use Diagnostics.{GetDiagnosticTimeMark, getElapsedTime};
    
    let mark = GetDiagnosticTimeMark.getDiagnosticTimeMark();
    let result = DaySix.PartTwo.app(filename);
    let elapsedTime = Diagnostics.getElapsedTime(mark);
    let () = Logger.debug("app elapsed time: ${elapsedTime}");
    result

def main(): Unit \ IO =
    let app = 
        timedApp |>
        FileReadWithResult.handle |> 
        Logger.handle |>
        Diagnostics.GetDiagnosticTimeMark.handle;

    app("six.txt") |>
    println