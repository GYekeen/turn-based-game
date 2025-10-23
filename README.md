Turn-Based Game Engine
Turn-Based Game is a Clarity-powered on-chain game engine that enables secure, verifiable, and transparent multiplayer games on the Stacks blockchain.
It ensures each player takes turns in sequence, tracks all moves on-chain, and enforces win conditions through smart contract logic.

Features
Create and join game sessions
Enforce fair turn-based rules
Record and verify all moves on-chain
Handle wins, draws, and forfeits
Transparent game state tracking

Technical Overview
Language: Clarity
Game States:
waiting — created but awaiting opponent
active — both players joined and turns in progress
finished — game concluded
Core Functions:
create-game(opponent) → start new game
join-game(game-id) → opponent joins
submit-move(game-id, move-data) → play a move
end-game(game-id, winner) → end match
forfeit(game-id) → voluntarily end game
