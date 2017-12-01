functor AgsFun (structure Game : GAME) :> AGS 
   where type Game.Move.move = Game.Move.move
   and   type Game.config    = Game.config
= struct
  structure Game = Game

  fun bestresult conf = ...
  fun bestmove conf = ...
  fun forecast conf = ...
end