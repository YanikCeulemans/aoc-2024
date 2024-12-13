mod DayFive {
    mod PartOne {
        def toPair(xs: List[a]): Option[(a, a)] =
            match xs {
                case left :: right :: Nil => Some((left, right))
                case _ => None
            }

        enum PageOrderingRules(Map[String, Set[String]]) with ToString

        mod PageOrderingRules {
            use DayFive.PartOne

            pub def ofList(rulesList: List[String]): PageOrderingRules =
                rulesList |>
                List.map(String.splitOn(substr = "|")) |>
                List.filterMap(PartOne.toPair) |>
                List.foldLeft((acc, pair) -> 
                    let (left, right) = pair;
                    Map.insertWith(Set.union, left, Set.singleton(right), acc)
                , Map#{}) |>
                PageOrderingRules

            pub def isUpdatePageNumberCorrect(
                pageNumber: String,
                laterPageNumbers: List[String],
                pageOrderingRules: PageOrderingRules
            ): Bool =
                let PageOrderingRules(rulesMap) = pageOrderingRules;
                match Map.get(pageNumber, rulesMap) {
                    case None => false
                    case Some(validPageNumbers) =>
                        Set.isSubsetOf(List.toSet(laterPageNumbers), validPageNumbers)
                }
        }

        enum Update(List[String]) with ToString

        mod Update {
            use DayFive.PartOne.PageOrderingRules
            pub def parse(serialized: String): Update =
                String.splitOn(substr = ",", serialized) |>
                Update

            pub def isInCorrectOrder(pageOrderingRules: PageOrderingRules, update: Update): Bool =
                def help(acc: Bool, pageNumbers: List[String]): Bool =
                    match pageNumbers {
                        case Nil => acc
                        case hd :: tail => 
                            let pageNumberIsCorrect =
                                PageOrderingRules.isUpdatePageNumberCorrect(hd, tail, pageOrderingRules);
                            help(acc and pageNumberIsCorrect, tail)
                    };

                let Update(pageNumbers) = update;
                pageNumbers |>
                List.init |>
                Option.map(help(true)) |>
                Option.getWithDefault(true)

            pub def getMiddle(update: Update): Option[String] =
                let Update(pageNumbers) = update;
                let pageNumbersLength = List.length(pageNumbers);
                if (Common.isEven(pageNumbersLength)) None
                else
                    let middleIndex = pageNumbersLength / 2;
                    List.zipWithIndex(pageNumbers) |>
                    List.find(fst >> Eq.eq(middleIndex)) |>
                    Option.map(snd)
        }

        pub def app(filename: String): Int32 \ {FileReadWithResult} =
            let (pageOrderingRuleLines, updateLines) =
                Common.readAllLinesOrCrash(filename) |>
                List.span(String.nonEmpty) |>
                // TODO: This is just fmap over the tuple
                (tuple -> 
                    let (left, right) = tuple;
                    (left, List.filter(String.nonEmpty, right))
                );

            let parsedPageOrderingRules =
                PageOrderingRules.ofList(pageOrderingRuleLines);

            List.map(Update.parse, updateLines) |>
            List.filter(Update.isInCorrectOrder(parsedPageOrderingRules)) |>
            List.filterMap(Update.getMiddle) |>
            List.filterMap(Int32.fromString) |>
            List.sum
    }
}