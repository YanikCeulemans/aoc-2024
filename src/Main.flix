def timedApp(filename: String): Int64 \ {Logger, Diagnostics.GetDiagnosticTimeMark, FileReadWithResult} =
    use Diagnostics.{GetDiagnosticTimeMark, getElapsedTime};
    
    let mark = GetDiagnosticTimeMark.getDiagnosticTimeMark();
    let lines = Common.readAllLinesOrCrash(filename);
    let result = DaySeven.PartOne.app(lines);
    let elapsedTime = Diagnostics.getElapsedTime(mark);
    let () = Logger.debug("app elapsed time: ${elapsedTime}");
    result

def main(): Unit \ IO =
    let app = 
        timedApp |>
        FileReadWithResult.handle |> 
        Logger.handle |>
        Diagnostics.GetDiagnosticTimeMark.handle;

    app("seven.txt") |>
    println