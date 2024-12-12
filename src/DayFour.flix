mod DayFour {
    pub def get2D(x: Int32, y: Int32, xss: Vector[Vector[a]]): a =
        Vector.get(y, xss) |>
        Vector.get(x)

    pub def diagonals(xss: List[List[a]]): List[List[a]] =
        let yss = List.toVector(List.map(List.toVector, xss));
        let countY = Vector.length(yss);
        let countX = Vector.get(0, yss) |> Vector.length;
        let topRight =
            List.range(1, countX) |>
            List.map(idxX ->
                List.range(0, countY - idxX) |>
                List.map(idxY -> get2D(idxY + idxX, idxY, yss))
            );
        let bottomLeft =
            List.range(0, countY) |>
            List.map(idxY -> 
                List.range(0, countX - idxY) |>
                List.map(idxX -> get2D(idxX, idxX + idxY, yss))
            );
        List.append(bottomLeft, topRight)

    mod PartOne {
        use DayFour.{get2D, diagonals}

        def countXmas(chars: List[Char]): Int32 =
            def help(total: Int32, cs: List[Char]): Int32 =
                match cs {
                    case 'X' :: 'M' :: 'A' :: 'S' :: rest => help(total + 1, rest)
                    case _ :: rest => help(total, rest)
                    case Nil => total
                };
            
            help(0, chars)

        def countXmasAllDirections(chars: List[List[Char]]): Int32 =
            List.sum (List#{
                chars |> List.map(countXmas) |> List.sum,
                chars |> List.map(List.reverse >> countXmas) |> List.sum,
                chars |> List.transpose |> List.map(countXmas) |> List.sum,
                chars |> List.transpose |> List.map(List.reverse >> countXmas) |> List.sum,
                chars |> diagonals |> List.map(countXmas) |> List.sum,
                chars |> diagonals |> List.map(List.reverse >> countXmas) |> List.sum,
                chars |> List.transpose |> List.reverse |> diagonals |> List.map(countXmas) |> List.sum,
                chars |> List.transpose |> List.reverse |> diagonals |> List.map(List.reverse >> countXmas) |> List.sum
            })

        pub def app(filename: String): Int32 \ FileReadWithResult =
            Common.readAllLinesOrCrash(filename) |>
            List.map(String.toList) |>
            countXmasAllDirections
    }

    mod PartTwo {
        use DayFour.get2D

        def drop2D(rowAmount: Int32, colAmount: Int32, xss: List[List[a]]): List[List[a]] =
            List.drop(rowAmount, List.map(List.drop(colAmount), xss))

        def getCenterDiagonal(length: Int32, xss: List[List[a]]): List[a] =
            let vss = List.toVector(List.map(List.toVector, xss));
            let countY = Vector.length(vss);
            let countX = Vector.length(Vector.get(0, vss));
            if (countY < length or countX < length) List.empty() 
            else
                List.range(0, length) |>
                List.map(idx -> get2D(idx, idx, vss))

        def isMas(chars: List[Char]): Bool =
            match chars {
                case 'M' :: 'A' :: 'S' :: Nil => true
                case _ => false
            }

        def isMasCross(chars: List[List[Char]]): Bool =
            let threeByThree = List.take(3, List.map(List.take(3), chars));
            let diag1 =
                List#{
                    threeByThree |> getCenterDiagonal(3),
                    threeByThree |> getCenterDiagonal(3) |> List.reverse
                } |>
                List.exists(isMas);
            let diag2 =
                List#{
                    threeByThree |> List.transpose |> List.reverse |> getCenterDiagonal(3),
                    threeByThree |> List.transpose |> List.reverse |> getCenterDiagonal(3) |> List.reverse
                } |>
                List.exists(isMas);
            diag1 and diag2

        def countAllMasCrosses(chars: List[List[Char]]): Int32 =
            // We assume this is a square list of s by s elements...
            let size = List.length(chars);
            let r =
                List.range(0, size) |>
                List.foldLeft((acc, idxY) -> 
                    List.range(0, size) |>
                    List.foldLeft((innerAcc, idxX ) ->
                        if (drop2D(idxX, idxY, chars) |> isMasCross) innerAcc + 1
                        else innerAcc
                    , acc)
                , 0);
            r

        pub def app(filename: String): Int32 \ {FileReadWithResult} =
            Common.readAllLinesOrCrash(filename) |>
            List.map(String.toList) |>
            countAllMasCrosses
    }
}