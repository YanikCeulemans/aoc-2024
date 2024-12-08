mod DayTwo {
    import java.lang.Math
    def parseReport(line: String): List[Int32] =
        line |>
        String.splitOn({ substr = " " }) |>
        List.filter(x -> not(String.isEmpty(x))) |>
        List.map(n ->
            match Int32.parse(10, n) {
                case Result.Err(_) => bug!("expected number but got: ${n}")
                case Result.Ok(x) => x
            }
        )

    def diffIsValid(x: Int32, y: Int32): Bool = {
        let diff = unsafe Math.abs(x - y);
        0 < diff and diff <= 3
    }

    pub def rmAt(index: Int32, xs: List[a]): List[a] =
        let (left, right) = List.splitAt(index, xs);
        left ++ List.drop(1, right)

    mod PartOne {
        def analyzeReport(report: List[Int32]): Bool =
            def help(acc: Bool, comparison: Comparison, xs: List[Int32]) : Bool =
                match xs {
                    case Nil => acc
                    case _ :: Nil => acc
                    case first :: second :: rest 
                        if Order.compare(first, second) == comparison
                            and DayTwo.diffIsValid(first, second)
                        =>
                        help(true, comparison, second :: rest)
                    case _ => false
                };

            match report {
                case _single :: Nil => true
                case first :: second :: rest if DayTwo.diffIsValid(first, second) =>
                    let comparison = Order.compare(first, second);
                    help(true, comparison, second :: rest)
                case _ => false
            }

        pub def app(filename: String): Int32 \ {FileReadWithResult} =
            let lines = 
                match FileReadWithResult.readLines(filename) {
                    case Result.Err(IoError.IoError(kind, msg)) =>
                        bug!("IoError: ${kind} ${msg}")
                    case Result.Ok(x) => x
                };

            let reports = List.map(DayTwo.parseReport, lines);
            List.count(analyzeReport, reports)
    }

    mod PartTwo {
        type alias State = {
            comparison = Comparison,
            index = Int32,
            unsafeLevelIndex = Option[Int32]
        }
        
        def analyseReport(report: List[Int32]): Bool =
            def findUnsafeLevelIndex(xs: List[Int32]) : Option[Int32] = {
                def go(state: State, ys: List[Int32]): State = 
                    match ys {
                        case Nil => state
                        case _ :: Nil => state
                        case first :: second :: rest =>
                            let diffIsValid = DayTwo.diffIsValid(first, second);
                            let comparisonIsSame = Order.compare(first, second) == state#comparison;
                            if (diffIsValid and comparisonIsSame) {
                                let newState = {
                                    index = state#index + 1
                                    | state
                                };
                                go(newState, second :: rest)
                            } else {
                                {
                                    unsafeLevelIndex = Some(state#index)
                                    | state
                                }
                            }
                    };

                match xs {
                    case first :: second :: rest =>
                        let diffIsValid = DayTwo.diffIsValid(first, second);
                        if (not(diffIsValid)) Some(0)
                        else {
                            let state = {
                                comparison = Order.compare(first, second),
                                index = 1,
                                unsafeLevelIndex = None
                            };
                            go(state, second :: rest)#unsafeLevelIndex
                        }
                    case _ => None
                }
            };

            match findUnsafeLevelIndex((report)) {
                case None => true
                case Some(unsafeIndex) =>
                    let dampenedFirst =
                        DayTwo.rmAt(0, report) |>
                        findUnsafeLevelIndex;
                    let dampenedLeft =
                        DayTwo.rmAt(unsafeIndex, report) |>
                        findUnsafeLevelIndex;
                    let dampenedRight =
                        DayTwo.rmAt(unsafeIndex + 1, report) |>
                        findUnsafeLevelIndex;
                    match (dampenedFirst, dampenedLeft, dampenedRight) {
                        case (Some(_), Some(_), Some(_)) => false
                        case _ => true
                    }
            }

        pub def app(filename: String): Int32 \ {FileReadWithResult} =
            let lines = 
                match FileReadWithResult.readLines(filename) {
                    case Result.Err(IoError.IoError(kind, msg)) =>
                        bug!("IoError: ${kind} ${msg}")
                    case Result.Ok(x) => x
                };

            let reports = List.map(DayTwo.parseReport, lines);
            List.count(analyseReport, reports)
    }
}