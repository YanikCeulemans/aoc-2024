mod DayOne {
    mod One {
        import java.lang.Math

        def extractNumbers(line: String): Result[String, List[Int32]] =
            line
            |> Regex.submatches({ substr = regex"\\d+" })
            |> List.traverse(Int32.parse(10))
            |> Result.mapErr(_ -> "digits regex matched non-digit?!")

        def extractNumberPairs(numbers: List[Int32])
            : Result[String, (Int32, Int32)] =
            match numbers {
                case left :: right :: Nil => Result.Ok((left, right))
                case _ => Result.Err("line isnt exactly 2 numbers?")
            }

        def parseLinesOrCrash(lines: List[String])
            : (List[Int32], List[Int32]) =
            let parsed = forM(
                numbersList <- List.traverse(extractNumbers, lines);
                numberPairs <- List.traverse(extractNumberPairs, numbersList)
            ) yield List.unzip(numberPairs);
            
            match parsed {
                case Result.Err(msg) => bug!(msg)
                case Result.Ok(r) => r
            }

        def calcDistance(x: Int32, y: Int32): Int32 =
            unsafe Math.abs(x - y)

        pub def app(filename: String)
            : Int32 \ {FileReadWithResult} =
            let lines = 
                match FileReadWithResult.readLines(filename) {
                    case Result.Err(IoError.IoError(kind, msg)) => 
                        bug!("IoError: ${kind} ${msg}")
                    case Result.Ok(x) => x
                };
            let (left, right) = parseLinesOrCrash(lines);
            List.zipWith(calcDistance, List.sort(left), List.sort(right)) |>
            List.sum
    }

    mod Two {
        import java.lang.Math
        def extractNumbers(line: String): Result[String, List[Int32]] =
            line
            |> Regex.submatches({ substr = regex"\\d+" })
            |> List.traverse(Int32.parse(10))
            |> Result.mapErr(_ -> "digits regex matched non-digit?!")

        def extractNumberPairs(numbers: List[Int32])
            : Result[String, (Int32, Int32)] =
            match numbers {
                case left :: right :: Nil => Result.Ok((left, right))
                case _ => Result.Err("line isnt exactly 2 numbers?")
            }

        def parseLinesOrCrash(lines: List[String])
            : (List[Int32], List[Int32]) =
            let parsed = forM(
                numbersList <- List.traverse(extractNumbers, lines);
                numberPairs <- List.traverse(extractNumberPairs, numbersList)
            ) yield List.unzip(numberPairs);
            
            match parsed {
                case Result.Err(msg) => bug!(msg)
                case Result.Ok(r) => r
            }

        def findOccuranceAndMutliply(frequency: Map[Int32, Int32], x: Int32)
            : Int32 =
            Map.getWithDefault(x, 0, frequency) * x

        pub def app(filename: String)
            : Int32 \ {FileReadWithResult} =
            let lines = 
                match FileReadWithResult.readLines(filename) {
                    case Result.Err(IoError.IoError(kind, msg)) => 
                        bug!("IoError: ${kind} ${msg}")
                    case Result.Ok(x) => x
                };
            let (left, right) = parseLinesOrCrash(lines);
            let frequency = List.frequency(right);
            left |>
            List.map(findOccuranceAndMutliply(frequency)) |>
            List.sum
    }
    // correct answer: 20520794
}
