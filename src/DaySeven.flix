mod DaySeven {
    enum Equation({ testValue = Int64, numbers = List[Int64] })

    instance ToString[Equation] {
        pub def toString(x: Equation): String =
            let Equation.Equation(eq) = x;
            "{ testValue = ${eq#testValue}, numbers = ${eq#numbers} }"
    }
    
    mod Equation {
        pub def testValue(equation: Equation): Int64 =
            let Equation(eq) = equation;
            eq#testValue

        pub def parse(input: String): Option[Equation] =
            input |>
            String.splitOn(substr = ":") |>
            List.flatMap(String.splitOn(substr = " ")) |>
            List.filter(String.nonEmpty) |>
            List.traverse(Int64.parse(10)) |>
            Result.toOption |>
            Option.flatMap(x ->
                match x {
                    case testValue :: nums => 
                        Equation({ testValue = testValue, numbers = nums }) |>
                        Some
                    case _ => None
                }
            )

        pub def canProduceTestValue(equation: Equation): Bool =
            let Equation(eq) = equation;
            
            // Not tail recursive :(
            def go(acc: List[Int64], nums: List[Int64]): List[Int64] =
                match nums {
                    case first :: second :: rest =>
                        go(acc, first + second :: rest) ::: go(acc, first * second :: rest)
                    case single :: Nil => single :: acc
                    case _ => acc
                };
            
            go(List.empty(), eq#numbers) |> 
            List.memberOf(eq#testValue)
    }

    mod PartOne {
        use DaySeven.Equation

        pub def app(lines: List[String]): Int64 =
            lines |>
            List.filterMap(Equation.parse) |>
            List.filter(Equation.canProduceTestValue) |>
            List.map(Equation.testValue) |>
            List.fold
    }
}