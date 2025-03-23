open System

type Cell =
    | Mine
    | Empty of int

type GameState = {
    Board: Cell[,]
    Revealed: bool[,]
    Flags: bool[,]
    Width: int
    Height: int
    Mines: int
    GameOver: bool
}

let rand = Random()

let directions = [|
    (-1, -1); (-1, 0); (-1, 1)
    (0, -1);          (0, 1)
    (1, -1);  (1, 0); (1, 1)
|]

let isInBounds x y width height =
    x >= 0 && y >= 0 && x < width && y < height

let placeMines (board: Cell[,]) width height mines =
    let mutable count = 0
    while count < mines do
        let x = rand.Next(width)
        let y = rand.Next(height)
        match board.[x, y] with
        | Mine -> ()
        | _ ->
            board.[x, y] <- Mine
            count <- count + 1

let countAdjacentMines (board: Cell[,]) x y width height =
    directions
    |> Array.sumBy (fun (dx, dy) ->
        let nx, ny = x + dx, y + dy
        if isInBounds nx ny width height then
            match board.[nx, ny] with
            | Mine -> 1
            | _ -> 0
        else 0
    )

let initializeBoard width height mines =
    let board: Cell[,] = Array2D.create width height (Empty 0)
    placeMines board width height mines
    for x in 0 .. width - 1 do
        for y in 0 .. height - 1 do
            match board.[x, y] with
            | Mine -> ()
            | Empty _ ->
                let count = countAdjacentMines board x y width height
                board.[x, y] <- Empty count
    board

let printBoard state =
    Console.Clear()
    printfn "\n    %s" (String.concat "" [ for i in 0 .. state.Width - 1 -> sprintf "%2d " i ])
    for y in 0 .. state.Height - 1 do
        Console.Write(sprintf "%2d  " y)
        for x in 0 .. state.Width - 1 do
            if state.Flags.[x, y] then
                Console.Write(" F ")
            elif not state.Revealed.[x, y] then
                Console.Write("[ ]")
            else
                match state.Board.[x, y] with
                | Mine -> Console.Write("[*]")
                | Empty 0 -> Console.Write("   ")
                | Empty n -> Console.Write(sprintf "[%d]" n)
        Console.WriteLine()
    Console.WriteLine()

let rec reveal state x y =
    if not (isInBounds x y state.Width state.Height) || state.Revealed.[x, y] || state.Flags.[x, y] then
        state
    else
        state.Revealed.[x, y] <- true
        match state.Board.[x, y] with
        | Mine -> { state with GameOver = true }
        | Empty 0 ->
            directions
            |> Array.fold (fun s (dx, dy) -> reveal s (x + dx) (y + dy)) state
        | _ -> state

let checkWin state =
    let mutable unrevealed = 0
    for x in 0 .. state.Width - 1 do
        for y in 0 .. state.Height - 1 do
            if not state.Revealed.[x, y] && state.Board.[x, y] <> Mine then
                unrevealed <- unrevealed + 1
    unrevealed = 0

let toggleFlag state x y =
    if isInBounds x y state.Width state.Height && not state.Revealed.[x, y] then
        state.Flags.[x, y] <- not state.Flags.[x, y]
    state

let rec gameLoop state =
    printBoard state
    if state.GameOver then
        printfn "💥 You hit a mine! Game Over!\n"
    elif checkWin state then
        printfn "🎉 You cleared the board! You win!\n"
    else
        printf "Enter coordinates (x y) or flag (F x y): "
        let input = Console.ReadLine().Split([|' ';','|], StringSplitOptions.RemoveEmptyEntries)
        match input with
        | [| "F"; xStr; yStr |] ->
            match Int32.TryParse(xStr), Int32.TryParse(yStr) with
            | (true, x), (true, y) ->
                let newState = toggleFlag state x y
                gameLoop newState
            | _ ->
                printfn "Invalid flag input. Try again."
                gameLoop state
        | [| xStr; yStr |] ->
            match Int32.TryParse(xStr), Int32.TryParse(yStr) with
            | (true, x), (true, y) when isInBounds x y state.Width state.Height ->
                let newState = reveal state x y
                gameLoop newState
            | _ ->
                printfn "Invalid coordinates. Try again."
                gameLoop state
        | _ ->
            printfn "Invalid input format. Try again."
            gameLoop state

let promptConfig prompt defaultValue =
    printf "%s (default %d): " prompt defaultValue
    let input = Console.ReadLine()
    match Int32.TryParse(input) with
    | true, n when n > 0 -> n
    | _ -> defaultValue

[<EntryPoint>]
let main _ =
    Console.OutputEncoding <- System.Text.Encoding.UTF8
    printfn "🧨 Welcome to Minesweeper (F# Edition with Flags)!"
    let width = promptConfig "Enter board width" 10
    let height = promptConfig "Enter board height" 10
    let maxMines = width * height - 1
    let minesInput = promptConfig "Enter number of mines" 10
    let mines = if minesInput > maxMines then maxMines else minesInput

    let board = initializeBoard width height mines
    let revealed = Array2D.create width height false
    let flags = Array2D.create width height false
    let state = {
        Board = board
        Revealed = revealed
        Flags = flags
        Width = width
        Height = height
        Mines = mines
        GameOver = false
    }

    gameLoop state
    0
