import java.lang.System

mod Diagnostics {
    import java.lang.System
    pub def getElapsedTime(timeUnit: TimeUnit, startTime: Int64): Int64 \ IO =
        let end = System.nanoTime();
        let delta = end - startTime;
        match timeUnit {
            case TimeUnit.Nanoseconds => delta
            case TimeUnit.Microseconds => delta / 1_000i64
            case TimeUnit.Milliseconds => delta / 1_000_000i64
            case TimeUnit.Seconds => delta / 1_000_000_000i64
            case TimeUnit.Minutes => delta / 1_000_000_000i64 / 60i64
            case TimeUnit.Hours => delta / 1_000_000_000i64 / 60i64 / 60i64
            case TimeUnit.Days => delta / 1_000_000_000i64 / 60i64 / 60i64 / 24i64
        }
}

def main(): Unit \ IO =
    let mark = System.nanoTime();
    App.runAll(() -> DayFive.PartTwo.app("five.txt")) |>
    println;
    let elapsed = Diagnostics.getElapsedTime(TimeUnit.Milliseconds, mark);
    println("total time: ${elapsed}ms")