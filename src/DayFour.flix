mod DayFour {
    mod PartOne {
        pub def buildTestList(): List[List[Int32]] =
            List#{
                List#{1,2,3},
                List#{4,5,6},
                List#{7,8,9}
            }

        def get2D(x: Int32, y: Int32, xss: Vector[Vector[a]]): a =
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
                dbg!!(chars |> List.transpose) |> List.map(countXmas) |> List.sum,
                chars |> List.transpose |> List.map(List.reverse >> countXmas) |> List.sum,
                chars |> diagonals |> List.map(countXmas) |> List.sum,
                chars |> diagonals |> List.map(List.reverse >> countXmas) |> List.sum,
                chars |> List.transpose |> diagonals |> List.map(countXmas) |> List.sum,
                chars |> List.transpose |> diagonals |> List.map(List.reverse >> countXmas) |> List.sum
            })

        pub def app(filename: String): Int32 \ FileReadWithResult =
            Common.readAllLinesOrCrash(filename) |>
            List.map(String.toList) |>
            countXmasAllDirections
    }
}