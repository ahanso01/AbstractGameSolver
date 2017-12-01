structure TTT :> GAME = struct
  type config = char option vector * Player.player
  exception Move
  structure Move = struct
   	type move = int * int
   	exception Move
   	fun fromString (stringMove) =
   		case stringMove of "upper left" => (0, 0)
   			| "upper middle" => (0, 1)
   			| "upper right" => (0, 2)
   			| "middle left" => (1, 0)
   			| "middle" => (1, 1)
   			| "middle right" => (1, 2)
   			| "lower left" => (2, 0)
   			| "lower middle" => (2, 1)
   			| "lower right" => (2, 2)
   			| _ => raise Move

   	fun prompt (player) = "Square for Player" ^ Player.toString(player) ^ "?"

   	fun toString player (x, y) = "Player " ^ Player.toString(player) ^ 
   		"moves to row " ^ Int.toString(x) ^ "and column " ^ Int.toString(y) ^ "."
  end
  
  fun toString (board, player) = 
  		Char.toString(Option.valOf(Vector.sub(board, 0))) ^ "|" ^ Char.toString(Option.valOf(Vector.sub(board, 1))) ^ "|" ^Char.toString(Option.valOf(Vector.sub(board, 2))) ^ "\n"
  		^ "---+---+---\n" ^
  		Char.toString(Option.valOf(Vector.sub(board, 3))) ^ "|" ^ Char.toString(Option.valOf(Vector.sub(board, 4))) ^ "|" ^Char.toString(Option.valOf(Vector.sub(board, 5))) ^ "\n"
  		^ "---+---+---\n" ^
  		Char.toString(Option.valOf(Vector.sub(board, 6))) ^ "|" ^ Char.toString(Option.valOf(Vector.sub(board, 7))) ^ "|" ^Char.toString(Option.valOf(Vector.sub(board, 8))) ^ "\n"
  		^ "Player " ^ Player.toString(player) ^ "'s turn to move"

  fun initial (player) = (Vector.tabulate(9, (fn (n) => NONE)), player)

  fun whoseturn (board, player) = player

  fun makemove (board, player) (x, y) = 
  		let val space = (x * 3) + y
  			val whatsThere = Vector.sub(board, space)
  			val playerName = String.sub(Player.toString(player), 0)
  		in if not (Option.isSome(whatsThere)) then 
  			(Vector.update(board, space, SOME playerName), 
  												Player.otherplayer(player))
  		   else raise Move
  		end

  	fun xChecker (board) =
  		case board of 
  			  #[SOME #"X", _, _, SOME #"X", _, _, SOME #"X", _, _] => true
  			| #[_, SOME #"X", _, _, SOME #"X", _, _, SOME #"X", _] => true
  			| #[_, _, SOME #"X", _, _, SOME #"X", _, _, SOME #"X"] => true
  			| #[SOME #"X", SOME #"X", SOME #"X", _, _, _, _, _, _] => true
  			| #[_, _, _, SOME #"X", SOME #"X", SOME #"X", _, _, _] => true
  			| #[_, _, _, _, _, _, SOME #"X", SOME #"X", SOME #"X"] => true
  			| #[SOME #"X", _, _, _, SOME #"X", _, _, _, SOME #"X"] => true
  			| #[_, _, SOME #"X", _, SOME #"X", _, SOME #"X", _, _] => true
  			| _ => false  

  	fun oChecker (board) =
  		case board of
  			  #[SOME #"O", _, _, SOME #"O", _, _, SOME #"O", _, _] => true
  			| #[_, SOME #"O", _, _, SOME #"O", _, _, SOME #"O", _] => true
  			| #[_, _, SOME #"O", _, _, SOME #"O", _, _, SOME #"O"] => true
  			| #[SOME #"O", SOME #"O", SOME #"O", _, _, _, _, _, _] => true
  			| #[_, _, _, SOME #"O", SOME #"O", SOME #"O", _, _, _] => true
  			| #[_, _, _, _, _, _, SOME #"O", SOME #"O", SOME #"O"] => true
  			| #[SOME #"O", _, _, _, SOME #"O", _, _, _, SOME #"O"] => true
  			| #[_, _, SOME #"O", _, SOME #"O", _, SOME #"O", _, _] => true
  			| _ => false

    fun boardSpace(board) = 
    			Vector.exists (fn (elem) => not (Option.isSome(elem))) board

  	fun outcome (board, player) = 
  		let val xWin = xChecker(board)
  			val oWin = oChecker(board)
  			val space = boardSpace(board)
  		in if xWin then SOME (Player.WINS Player.X)
  		   else if oWin then SOME (Player.WINS Player.O)
  		   else if not space then SOME (Player.TIE)
  		   else NONE
  		end

  	fun finished cfg = 
  		let val outcome = outcome(cfg)
  		in case outcome of SOME _ => true
  			| NONE => false
  		end

  	fun possmoves (board, player) = 
  	 Vector.foldri (fn(i, vtr, xs) => if not (Option.isSome(vtr)) then 
  									        (i div 3, i mod 3)::xs 
  									  else xs) [] board



end