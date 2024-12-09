mod DayThree {
    def readAllLinesOrCrash(filename: String): List[String] \ {FileReadWithResult} =
        match FileReadWithResult.readLines(filename) {
            case Err(IoError.IoError(kind, msg)) => bug!("${kind}: ${msg}")
            case Ok(lines) => lines
        }

    mod PartOne {
        def extractMultiplicationPairs(line: String): List[(Int32, Int32)] =
            Regex.submatches(substr = regex"mul\\(\\d{1,3},\\d{1,3}\\)", line) |>
            List.filterMap(regexMatch ->
                let parsedInts =
                    Regex.submatches(substr = regex"\\d+", regexMatch) |>
                    List.traverse(Int32.parse(10));
                match parsedInts {
                    case Ok(left :: right :: Nil) => Some((left, right))
                    case _ => None
                }
            )

        def multiplyPair(p: (Int32, Int32)): Int32 =
            let (left, right) = p;
            left * right

        pub def app(filename: String): Int32 \ {FileReadWithResult} =
            DayThree.readAllLinesOrCrash(filename) |>
            List.flatMap(extractMultiplicationPairs) |>
            List.map(multiplyPair) |>
            List.sum
    }

    mod PartTwo {
        enum Operation with ToString {
            case Multiply(Int32, Int32),
            case Do,
            case Dont
        }

        mod Operation {
            pub def parse(input: String): List[Operation] =
                Regex.submatches(substr = regex"do\\(\\)|don't\\(\\)|mul\\(\\d{1,3},\\d{1,3}\\)", input) |>
                List.filterMap(x -> 
                    match x {
                        case "do()" => Some(Do)
                        case "don't()" => Some(Dont)
                        case mul if String.startsWith(prefix = "mul(", mul) =>
                            Regex.submatches(substr = regex"\\d{1,3}", mul) |>
                            List.traverse(Int32.parse(10)) |>
                            y ->
                                match y {
                                    case Ok(left :: right :: Nil) => Some(Multiply(left, right))
                                    case _ => None
                                }
                        case _ => None
                    }
                )
        }

        type alias OperationState = {
            result = Int32,
            isInDo = Bool
        }

        def interpretOperations(operations: List[Operation]): Int32 =
            def interpret(state: OperationState, operation: Operation): OperationState = 
                match operation {
                    case Operation.Do => { isInDo = true | state }
                    case Operation.Dont => { isInDo = false | state }
                    case Operation.Multiply(a, b) if (state#isInDo) => 
                        { result = state#result + (a * b) | state }
                    case Operation.Multiply(_, _) => state
                };

            List.foldLeft(interpret, { result = 0, isInDo = true }, operations)#result

        pub def app(filename: String): Int32 \ {FileReadWithResult} =
            DayThree.readAllLinesOrCrash(filename) |>
            List.flatMap(Operation.parse) |>
            interpretOperations
    }
}