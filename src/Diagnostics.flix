mod Diagnostics {
    use Int64.modulo
    enum TimeSpan(Int64)

    instance Sub[TimeSpan] {
        pub def sub(x: TimeSpan, y: TimeSpan): TimeSpan =
            let TimeSpan.TimeSpan(xi64) = x;
            let TimeSpan.TimeSpan(yi64) = y;
            TimeSpan.TimeSpan(xi64 - yi64)
    }

    instance Add[TimeSpan] {
        pub def add(x: TimeSpan, y: TimeSpan): TimeSpan =
            let TimeSpan.TimeSpan(xi64) = x;
            let TimeSpan.TimeSpan(yi64) = y;
            TimeSpan.TimeSpan(xi64 + yi64)
    }
    
    instance ToString[TimeSpan] {
        pub def toString(x: TimeSpan): String =
            let pl = String.padLeft(2, '0');
            let pl3 = String.padLeft(3, '0');
            let TimeSpan.TimeSpan(timeSpan) = x;
            let microSeconds = timeSpan / 1_000i64;
            let milliseconds = microSeconds / 1_000i64;
            let seconds = milliseconds / 1_000i64;
            let minutes = seconds / 60i64;
            let hours = minutes / 60i64;
            let formattedMicroseconds = pl3("${microSeconds `modulo` 1_000i64}");
            let formattedMilliseconds = pl("${milliseconds `modulo` 1_000i64}");
            let formattedSeconds = pl("${seconds `modulo` 60i64}");
            let formattedMinutes = pl("${minutes `modulo` 60i64}");
            let formattedHours = pl("${hours `modulo` 60i64}");
            "${formattedHours}:${formattedMinutes}:${formattedSeconds}.${formattedMilliseconds},${formattedMicroseconds}"
    }
    

    pub eff GetDiagnosticTimeMark {
        def getDiagnosticTimeMark(): TimeSpan
    }

    mod GetDiagnosticTimeMark {
        import java.lang.System
        use Diagnostics.TimeSpan

        pub def handle(f: a -> b \ ef): a -> b \ (ef - {GetDiagnosticTimeMark} + IO) =
            x -> 
                run {
                    f(x)
                } with GetDiagnosticTimeMark {
                    def getDiagnosticTimeMark(_unit, k) =
                        let nanoTime = System.nanoTime();
                        k(TimeSpan.TimeSpan(nanoTime))
                }

        pub def runWithIO(f: Unit -> a \ ef): a \ (ef - {GetDiagnosticTimeMark} + IO) =
            handle(f)()
    }

    pub def getElapsedTime(startTime: TimeSpan): TimeSpan \ {GetDiagnosticTimeMark} =
        let end = GetDiagnosticTimeMark.getDiagnosticTimeMark();
        end - startTime
}