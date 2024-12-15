mod DaySix {
    enum Direction {
        case North, East, South, West
    }

    mod Direction {
        pub def turnRight(direction: Direction): Direction =
            match direction {
                case North => East
                case East => South
                case South => West
                case West => North
            }
    }

    enum Position(Int32, Int32) with Eq, Order

    mod Position {
        use DaySix.Direction
        pub def isOutsideOf(limit: Int32, position: Position): Bool =
            let Position(x, y) = position;
            x >= limit or y >= limit or x < 0 or y < 0
    }

    enum Guard(Direction, Position)

    mod Guard {
        use DaySix.{ Direction, Position }

        pub def move(obstacles: Set[Position], guard: Guard): Guard =
            let Guard(direction, position) = guard;
            let Position.Position(x, y) = position;
            let movedPosition = 
                match direction {
                    case Direction.North =>
                        Position.Position(x, y - 1)
                    case Direction.East =>
                        Position.Position(x + 1, y)
                    case Direction.South =>
                        Position.Position(x, y + 1)
                    case Direction.West =>
                        Position.Position(x - 1, y)
                };

            if (Set.memberOf(movedPosition, obstacles))
                let turnedDirection = Direction.turnRight(direction);
                move(obstacles, Guard(turnedDirection, position))
            else
                Guard(direction, movedPosition)

        pub def position(guard: Guard): Position =
            let Guard(_, position) = guard;
            position
    }

    enum FloorPlan({ obstacles = Set[Position], guard = Guard, size = Int32 })

    mod FloorPlan {
        use DaySix.{Position, Guard, Direction}

        type alias ParseState = {
            obstacles = List[Position],
            guard = Option[Guard]
        }

        def parseWithState(
            state: ParseState,
            idxX: Int32,
            idxY: Int32,
            c: Char
        ): ParseState =
            match c {
                case '#' => 
                    { 
                        obstacles = 
                            Position.Position(idxX, idxY) ::
                            state#obstacles 
                        | state
                    }
                case '^' =>
                    {
                        guard = 
                            Position.Position(idxX, idxY) |>
                            Guard.Guard(Direction.North) |>
                            Some
                        | state
                    }
                case _ => state
            }

        pub def parse(lines: List[String]): Option[FloorPlan] =
            lines |>
            List.zipWithIndex |>
            List.foldLeft((state, idxTupleY) ->
                let (idxY, s) = idxTupleY;
                String.toList(s) |>
                List.zipWithIndex |>
                List.foldLeft((innerState, idxTupleX) ->
                    let (idxX, c) = idxTupleX;
                    parseWithState(innerState, idxX, idxY, c)
                , state)
            , { obstacles = List.empty(), guard = None }) |>
            state -> match state#guard {
                case None => None
                case Some(guard) => 
                    { 
                        obstacles = List.toSet(state#obstacles),
                        guard = guard,
                        size = List.length(lines) 
                    } |>
                    FloorPlan |>
                    Some
            } 

        pub def predict(floorPlan: FloorPlan): List[Position] =
            let FloorPlan(plan) = floorPlan;
            def help(visited: List[Position], guard: Guard): List[Position] = {

                let movedGuard = Guard.move(plan#obstacles, guard);
                let movedGuardPosition = Guard.position(movedGuard);
                let guardMovedOffGrid = 
                    Position.isOutsideOf(plan#size, movedGuardPosition);

                if (guardMovedOffGrid)
                    visited
                else
                    help(movedGuardPosition :: visited, movedGuard)
            };

            help(List.empty(), plan#guard)
    }

    mod PartOne {
        pub def app(filename: String): Int32 \ {FileReadWithResult} =
            let result =
                Common.readAllLinesOrCrash(filename) |>
                DaySix.FloorPlan.parse |>
                Option.map(
                    DaySix.FloorPlan.predict >> 
                    List.toSet >> 
                    Set.count(constant(true))
                );

            match result {
                case None => bug!("invalid floorplan")
                case Some(r) => r
            }
    }
}