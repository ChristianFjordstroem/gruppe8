module Chess

/// The possible colors of chess pieces
type Color = White | Black

/// A superset of positions on a board
type Position = int * int

/// <summary> An abstract chess piece. </summary>
/// <param name = "col"> The color black or white </param>
[<AbstractClass>]
type chessPiece(color : Color) =
  let mutable _position : Position option = None

  /// The type of the chess piece as a string, e.g., "king" or "rook".
  abstract member nameOfType : string

  /// The color either White or Black
  member this.color = color

  /// The position as a Position option, e.g., None, Some (0,0), Some
  /// (3,4).
  member this.position
    with get() = _position
    and set(pos) = _position <- pos

  /// Return the first letter of the piece's type usint capital case
  /// for white pieces and lower case for black pieces. E.g., "K" and
  /// "k" for white and a black king respectively.
  override this.ToString () =
    match color with
      White -> (string this.nameOfType.[0]).ToUpper ()
      | Black -> (string this.nameOfType.[0]).ToLower ()

  /// A maximum list of relative runs, a piece may make regardless of
  /// its position and the other pieces on the board. For example, a
  /// rook can move up, down, left, and right, so its list must
  /// contain 4 runs, and the "up" run must contain 7 positions
  /// [(-1,0); (-2,0)]...[-7,0]]. Runs must be ordered such that the
  /// first in a list is closest to the piece at hand.
  abstract member candiateRelativeMoves : Position list list

/// A chess board.
type Board () =
  let _board = Collections.Array2D.create<chessPiece option> 8 8 None

  /// <summary> Wrap a position as option type. </summary>
  /// <param name = "pos"> a position </param>
  /// <returns> Some pos or None if the position is on the board or
  /// not </returns>
  let validPositionWrap (pos : Position) : Position option =
    let (rank, file) = pos // square coordinate
    if rank < 0 || rank > 7 || file < 0 || file > 7 
    then None
    else Some (rank, file)

  /// <summary> Converts relative coordinates to absolute and removes
  /// out of board coordinates. </summary>
  /// <param name = "pos"> an absolute position </param>
  /// <param name = "lst"> a list of relative positions </param>
  /// <returns> A list of absolute and valid positions </returns>
  let relativeToAbsolute (pos : Position) (lst : Position list) : Position list =
    let addPair (a : Position) (b : Position) : Position = 
      (fst a + fst b, snd a + snd b)
    // Add origin and delta positions
    List.map (addPair pos) lst
    // Choose absolute positions that are on the board
    |> List.choose validPositionWrap

  /// <summary> Find the tuple of empty squares and first neighbour if any. </summary>
  /// <param name = "run"> A run of absolute positions </param>
  /// <returns> A pair of a list of empty neighbouring positions and
  /// a possible neighbouring piece, which blocks the run. </returns>
  let getVacantNOccupied (run : Position list) : (Position list * (chessPiece option)) =
    try
      // Find index of first non-vacant square of a run
      let idx = List.findIndex (fun (i, j) -> _board.[i,j].IsSome) run
      let (i,j) = run.[idx]
      let piece = _board.[i, j] // The first non-vacant neighbour
      if idx = 0
      then ([], piece)
      else (run.[..(idx-1)], piece)
    with
      _ -> (run, None) // outside the board

  /// Board is indexed using .[,] notation
  member this.Item
    with get(a : int, b : int) = _board.[a, b]
    and set(a : int, b : int) (p : chessPiece option) =
      if p.IsSome then p.Value.position <- Some (a,b)
      _board.[a, b] <- p

  /// Produce string of board for, e.g., the printfn function.
  override this.ToString() =
    let rec boardStr (i : int) (j : int) : string =
      match (i,j) with 
        (8,0) -> ""
        | _ ->
          let stripOption (p : chessPiece option) : string = 
            match p with
              None -> ""
              | Some p -> p.ToString()
          // print top to bottom row
          let pieceStr = stripOption _board.[7-i,j]
          let lineSep = " " + String.replicate (8*4-1) "-"
          match (i,j) with 
          (0,0) -> 
            let str = sprintf "%s\n| %1s " lineSep pieceStr
            str + boardStr 0 1
          | (i,7) -> 
            let str = sprintf "| %1s |\n%s\n" pieceStr lineSep
            str + boardStr (i+1) 0 
          | (i,j) -> 
            let str = sprintf "| %1s " pieceStr
            str + boardStr i (j+1)
    boardStr 0 0

  /// <summary> Move piece from a source to a target position. Any
  /// piece on the target position is removed. </summary>
  /// <param name = "source"> The source position </param>
  /// <param name = "target"> The target position </param>
  member this.move (source : Position) (target : Position) : unit =
    this.[fst target, snd target] <- this.[fst source, snd source]
    this.[fst source, snd source] <- None

  /// <summary> Find the list of available empty positions for this
  /// piece, and the list of possible opponent pieces, which can be
  /// taken. </summary>
  /// <param name = "piece"> A chess piece </param>
  /// <returns> A pair of lists of all available moves and neighbours,
  /// e.g., ([(1,0); (2,0);...], [p1; p2]) </returns>
  member this.availableMoves (piece : chessPiece) : (Position list * chessPiece list)  =
    match piece.position with
      None -> 
        ([],[])
      | Some p ->
        let convertNWrap = 
          (relativeToAbsolute p) >> getVacantNOccupied
        let vacantPieceLists = List.map convertNWrap piece.candiateRelativeMoves
        // Extract and merge lists of vacant squares
        let vacant = List.collect fst vacantPieceLists
        // Extract and merge lists of first obstruction pieces and filter out own pieces (FIXED to only include enemy pieces using List.Filter)
        let opponent = List.filter (fun (p : chessPiece) -> p.color <> piece.color) (List.choose snd vacantPieceLists)
        (vacant, opponent)

/// <summary> Abstract player for the chess game, which must be inherited from. </summary>
/// <param name="color"> The color of the player. </param>
/// <param name="board"> The chessboard the player is playing on. </param>
[<AbstractClass>]
type Player(color, board) = 
  let _color : Color = color
  let _board : Board = board

  /// <summary> 
  ///   Gets the next move for the player and returns the move as a codestring or "quit".
  ///   Codestrings are formatted as "a1 a5", where a1 is the position of the piece to move and a5 is the destination to move the piece to.
  ///   The method ensures that the codestring is valid, meaning there is a piece at the source position, 
  ///   it belongs to the player, and the target position is a valid move for that piece.
  /// </summary>
  /// <returns> Returns either "quit" or a valid codestring. </returns>
  abstract member nextMove : unit -> string
  
  /// <summary> Gets the color of the player. </summary>
  /// <returns> The color of the player. </returns>
  member this.color = color

  /// <summary> Parses a codestring into source and target position. </summary>
  /// <returns> Returns the source and target position as a tuple option or None if the codestring is invalid. </returns>
  member this.parseCodestring (codestring : string) : (Position * Position) option =
    if codestring.Length <> 5 || codestring.[2] <> ' ' then
      None
    else
      let lCodestring = codestring.ToLower()
      let sourceFile = int lCodestring.[0] - 97
      let sourceRank = int lCodestring.[1] - 49
      let targetFile = int lCodestring.[3] - 97
      let targetRank = int lCodestring.[4] - 49
      if sourceFile >= 0 && sourceFile < 8 && sourceRank >= 0 && sourceRank < 8 && targetFile >= 0 && targetFile < 8 && targetRank >= 0 && targetRank < 8 then
        let source = (sourceRank, sourceFile)
        let target = (targetRank, targetFile)
        Some (source, target)
      else
        None

/// <summary> Human player for the chess game. </summary>
/// <param name="color"> The color of the player. </param>
/// <param name="board"> The chessboard the player is playing on. </param>
type Human(color, board) =
  inherit Player(color, board)

  /// <summary> 
  ///   Asks the player through the console for a valid command. 
  ///   See description in parent class Player for details on command format.
  /// </summary>
  /// <returns> Returns either "quit" or a valid codestring. </returns>
  override this.nextMove() = 
    let mutable valid = false
    let mutable codestring = ""
    // Keep asking player until he enters something valid
    while not valid do
      printfn "%s" (color.ToString() + ": Enter move or quit: ")
      codestring <- System.Console.ReadLine()
      if (codestring.ToLower() = "quit") then
        valid <- true
      else
        let move = this.parseCodestring codestring
        // Check if entered codestring is valid
        if move.IsSome then
          // Extract source and target position
          let source = fst move.Value
          let target = snd move.Value
          // Get piece from board at source position
          let piece = board.[fst source, snd source]
          // Check there is a piece on the source position
          if piece.IsSome then
            // Check piece belongs to player
            if piece.Value.color = color then
              // Get available moves for piece
              let availableMoves = board.availableMoves piece.Value
              // Available moves to vacant positions
              let vacantMoves = fst availableMoves
              // Available moves to enemy positions (maps from piece to position)
              let enemyMoves = List.map (fun (piece : chessPiece) -> piece.position.Value) (snd availableMoves)
              // Check if entered target position is among valid moves
              if List.contains target vacantMoves || List.contains target enemyMoves then
                valid <- true
              else
                printfn "Movement was invalid."
            else
              printfn "Opponent piece at start coordinate."
          else
            printfn "No piece at start coordinate."
        else
          printfn "Movement was invalid."
    codestring
