open Chess
open Pieces

/// <summary> Game class for a simple chess game. </summary>
type Game() =
  //Initialize players
  let _board = Board()
  let _player = Human(White, _board)
  let _opponent = Human(Black, _board)

  // Constructor
  do
    // Place pieces on board
    _board.[0,0] <- Some (rook (White) :> chessPiece)
    _board.[0,4] <- Some (king (White) :> chessPiece)
    _board.[0,7] <- Some (rook (White) :> chessPiece)
    _board.[7,0] <- Some (rook (Black) :> chessPiece)
    _board.[7,4] <- Some (king (Black) :> chessPiece)
    _board.[7,7] <- Some (rook (Black) :> chessPiece)

  /// <summary> Prints the valid moves for a piece </summary>
  /// <param name="p"> Piece to print. </param>
  let printPiece (p : chessPiece) : unit =
    printfn "%A: %A %A" p p.position (_board.availableMoves p)

  /// <summary> Runs the game until a player enters quit. </summary>
  member this.run () =
    let rec aTurn (p1: Player) (p2: Player) : unit =
      printfn "%A" _board
      printfn "Player pieces and available moves:"
      for rank = 0 to 7 do
        for file = 0 to 7 do
          let pieceOption = _board.[rank, file]
          if pieceOption.IsSome && pieceOption.Value.color = p1.color then
            printPiece pieceOption.Value
      // Ask player for next move
      let codestring = p1.nextMove()
      // Execute move or quit
      let lCodestring = codestring.ToLower()
      if lCodestring.ToLower() <> "quit" then
        let move = p1.parseCodestring codestring
        _board.move (fst move.Value) (snd move.Value)
        aTurn p2 p1

    aTurn _player _opponent
    printfn "\nThanks for playing!"

// Run the game
let game = Game()
game.run()
