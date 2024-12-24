pub enum Parser[a](String -> Result[String, (String, a)])

mod Parser {
    pub def runParser(parser: Parser[a], input: String): Result[String, (String, a)] =
        let Parser(f) = parser;
        f(input)

    pub def pMany(parser: Parser[a]): Parser[List[a]] =
        def pRec(acc: (String, List[a])): (String, List[a]) = {
            let Parser(f) = parser;
            let (input, xs) = acc;
            match f(input) {
                case Err(_) => (input, List.reverse(xs))
                case Ok((rest, x)) =>
                    pRec((rest, x :: xs))
            }
        };

        def p(input: String): Result[String, (String, List[a])] = 
            pRec((input, List.empty())) |> Ok;

        Parser(p)

    pub def pSatisfy(f: Char -> Bool): Parser[Char] =
        def p(input: String): Result[String, (String, Char)] =
            match String.toList(input) {
                case c :: cs if f(c) =>
                    Ok((List.join("", cs), c))
                case _ =>
                    Err("no char found satisfying predicate")
            };
        
        Parser(p)

    pub def pChar(c: Char): Parser[Char] =
        def p(input: String): Result[String, (String, Char)] = 
            match String.toList(input) {
                case x :: rest if x == c =>
                    Ok((List.join("", rest), c))
                case _ => Err("char ${c} not found")
            };
        
        Parser(p)
}