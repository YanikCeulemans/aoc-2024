mod DayFive {
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
            List.filterMap(DayFive.toPair) |>
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

        pub def compare(
            pageOrderingRules: PageOrderingRules,
            pageNumberA: String,
            pageNumberB: String
        ): Comparison =
            let PageOrderingRules(rules) = pageOrderingRules;
            let pageNumberAIsLessThanB = 
                Map.get(pageNumberA, rules) |>
                Option.getWithDefault(Set#{}) |>
                Set.memberOf(pageNumberB);
            let pageNumberBIsLessThanA =
                Map.get(pageNumberB, rules) |>
                Option.getWithDefault(Set#{}) |>
                Set.memberOf(pageNumberA);

            match (pageNumberAIsLessThanB, pageNumberBIsLessThanA) {
                case (true, false) => Comparison.LessThan
                case (false, true) => Comparison.GreaterThan
                case (false, false) => Comparison.EqualTo
                case _ => 
                    let bugMsg =
                        String.intercalate(String.lineSeparator(), List#{
                            "wut?!",
                            "pageNumberA: ${pageNumberA}",
                            "pageNumberB: ${pageNumberB}",
                            "pageNumberAIsLessThanB: ${pageNumberAIsLessThanB}",
                            "pageNumberBIsLessThanA: ${pageNumberBIsLessThanA}",
                            "rules:",
                            "${rules}"
                        });
                    bug!(bugMsg)
            }
    }

    enum Update(List[String]) with ToString

    mod Update {
        use DayFive.PageOrderingRules
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

        pub def sortByRules(pageOrderingRules: PageOrderingRules, update: Update): Update =
            let Update(pageNumbers) = update;
            List.sortWith(PageOrderingRules.compare(pageOrderingRules), pageNumbers) |>
            Update

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

    mod PartOne {
        pub def app(filename: String): Int32 \ {FileReadWithResult} =
            let (pageOrderingRuleLines, updateLines) =
                Common.readAllLinesOrCrash(filename) |>
                List.span(String.nonEmpty) |>
                (tuple -> 
                    let (left, right) = tuple;
                    (left, List.filter(String.nonEmpty, right))
                );

            let parsedPageOrderingRules =
                DayFive.PageOrderingRules.ofList(pageOrderingRuleLines);

            List.map(DayFive.Update.parse, updateLines) |>
            List.filter(DayFive.Update.isInCorrectOrder(parsedPageOrderingRules)) |>
            List.filterMap(DayFive.Update.getMiddle) |>
            List.filterMap(Int32.fromString) |>
            List.sum
    }

    mod PartTwo {
        use DayFive.PartOne

        pub def app(filename: String): Int32 \ {FileReadWithResult} =
            let (pageOrderingRuleLines, updateLines) =
                Common.readAllLinesOrCrash(filename) |>
                List.span(String.nonEmpty) |>
                (tuple -> 
                    let (left, right) = tuple;
                    (left, List.filter(String.nonEmpty, right))
                );

            let parsedPageOrderingRules =
                DayFive.PageOrderingRules.ofList(pageOrderingRuleLines);

            List.map(DayFive.Update.parse, updateLines) |>
            List.filter(update -> not(DayFive.Update.isInCorrectOrder(parsedPageOrderingRules, update))) |>
            List.map(DayFive.Update.sortByRules(parsedPageOrderingRules)) |>
            List.filterMap(DayFive.Update.getMiddle) |>
            List.filterMap(Int32.fromString) |>
            List.sum
    }
}