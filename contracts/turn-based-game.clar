;; ------------------------------------------------------------
;; turn-based-game.clar
;; On-chain Turn-Based Game (Tic-Tac-Toe)
;; Works on Stacks blockchain (STX)
;; ------------------------------------------------------------

(define-constant ERR_GAME_EXISTS u100)
(define-constant ERR_NOT_PLAYER u101)
(define-constant ERR_NOT_TURN u102)
(define-constant ERR_INVALID_MOVE u103)
(define-constant ERR_GAME_OVER u104)
(define-constant ERR_CELL_TAKEN u105)
(define-constant ERR_GAME_NOT_FOUND u106)

(define-data-var game-counter uint u0)

(define-map games
  { game-id: uint }
  {
    player-x: principal,
    player-o: principal,
    board: (list 9 uint), ;; 0-empty, 1-X, 2-O
    turn: principal,
    winner: (optional principal),
    status: (string-ascii 10) ;; "active" | "ended"
  }
)

;; ------------------------------------------------------------
;; Helper Functions
;; ------------------------------------------------------------

(define-private (empty-board)
  (list u0 u0 u0 u0 u0 u0 u0 u0 u0))

(define-private (check-win (board (list 9 uint)) (mark uint))
  (let (
        (b board)
        (wins (list
          (list u0 u1 u2)
          (list u3 u4 u5)
          (list u6 u7 u8)
          (list u0 u3 u6)
          (list u1 u4 u7)
          (list u2 u5 u8)
          (list u0 u4 u8)
          (list u2 u4 u6)
        ))
      )
      (or
     ;; Rows
     (and (is-eq (unwrap-panic (element-at? board u0)) mark)
       (is-eq (unwrap-panic (element-at? board u1)) mark)
       (is-eq (unwrap-panic (element-at? board u2)) mark))
     (and (is-eq (unwrap-panic (element-at? board u3)) mark)
       (is-eq (unwrap-panic (element-at? board u4)) mark)
       (is-eq (unwrap-panic (element-at? board u5)) mark))
     (and (is-eq (unwrap-panic (element-at? board u6)) mark)
       (is-eq (unwrap-panic (element-at? board u7)) mark)
       (is-eq (unwrap-panic (element-at? board u8)) mark))
     ;; Columns
     (and (is-eq (unwrap-panic (element-at? board u0)) mark)
       (is-eq (unwrap-panic (element-at? board u3)) mark)
       (is-eq (unwrap-panic (element-at? board u6)) mark))
     (and (is-eq (unwrap-panic (element-at? board u1)) mark)
       (is-eq (unwrap-panic (element-at? board u4)) mark)
       (is-eq (unwrap-panic (element-at? board u7)) mark))
     (and (is-eq (unwrap-panic (element-at? board u2)) mark)
       (is-eq (unwrap-panic (element-at? board u5)) mark)
       (is-eq (unwrap-panic (element-at? board u8)) mark))
     ;; Diagonals
     (and (is-eq (unwrap-panic (element-at? board u0)) mark)
       (is-eq (unwrap-panic (element-at? board u4)) mark)
       (is-eq (unwrap-panic (element-at? board u8)) mark))
     (and (is-eq (unwrap-panic (element-at? board u2)) mark)
       (is-eq (unwrap-panic (element-at? board u4)) mark)
       (is-eq (unwrap-panic (element-at? board u6)) mark))
      )
  )
)

(define-private (is-board-full (board (list 9 uint)))
  (and
    (not (is-eq (unwrap-panic (element-at? board u0)) u0))
    (not (is-eq (unwrap-panic (element-at? board u1)) u0))
    (not (is-eq (unwrap-panic (element-at? board u2)) u0))
    (not (is-eq (unwrap-panic (element-at? board u3)) u0))
    (not (is-eq (unwrap-panic (element-at? board u4)) u0))
    (not (is-eq (unwrap-panic (element-at? board u5)) u0))
    (not (is-eq (unwrap-panic (element-at? board u6)) u0))
    (not (is-eq (unwrap-panic (element-at? board u7)) u0))
    (not (is-eq (unwrap-panic (element-at? board u8)) u0))
  ))

(define-private (replace-at (board (list 9 uint)) (pos uint) (val uint))
  (list
    (if (is-eq pos u0) val (unwrap-panic (element-at? board u0)))
    (if (is-eq pos u1) val (unwrap-panic (element-at? board u1)))
    (if (is-eq pos u2) val (unwrap-panic (element-at? board u2)))
    (if (is-eq pos u3) val (unwrap-panic (element-at? board u3)))
    (if (is-eq pos u4) val (unwrap-panic (element-at? board u4)))
    (if (is-eq pos u5) val (unwrap-panic (element-at? board u5)))
    (if (is-eq pos u6) val (unwrap-panic (element-at? board u6)))
    (if (is-eq pos u7) val (unwrap-panic (element-at? board u7)))
    (if (is-eq pos u8) val (unwrap-panic (element-at? board u8)))
  )
)

;; ------------------------------------------------------------
;; Public Functions
;; ------------------------------------------------------------

;; Start a new game
(define-public (create-game (opponent principal))
  (begin
    (var-set game-counter (+ (var-get game-counter) u1))
    (let ((gid (var-get game-counter)))
      (map-set games
        { game-id: gid }
        {
          player-x: tx-sender,
          player-o: opponent,
          board: (empty-board),
          turn: tx-sender,
          winner: none,
          status: "active"
        }
      )
      (ok gid)
    )
  )
)

;; Make a move
(define-public (make-move (game-id uint) (position uint))
  (let ((game (map-get? games { game-id: game-id })))
    (match game
      game-data
      (let (
            (board (get board game-data))
            (turn (get turn game-data))
            (player-x (get player-x game-data))
            (player-o (get player-o game-data))
            (winner (get winner game-data))
            (status (get status game-data))
      )
        (asserts! (is-eq status "active") (err ERR_GAME_OVER))
        (asserts! (or (is-eq tx-sender player-x) (is-eq tx-sender player-o)) (err ERR_NOT_PLAYER))
        (asserts! (is-eq tx-sender turn) (err ERR_NOT_TURN))
        (asserts! (< position u9) (err ERR_INVALID_MOVE))

  ;; Check cell empty
  (asserts! (is-eq (unwrap-panic (element-at? board position)) u0) (err ERR_CELL_TAKEN))

        ;; Update board with player mark
        (let ((mark (if (is-eq tx-sender player-x) u1 u2)))
          (let ((new-board (replace-at board position mark)))
            ;; Check win or draw
            (let ((did-win (check-win new-board mark))
                  (is-full (is-board-full new-board)))
              (if did-win
                (let ((next none))
                  (map-set games { game-id: game-id }
                    {
                      player-x: player-x,
                      player-o: player-o,
                      board: new-board,
                      turn: tx-sender,
                      winner: (some tx-sender),
                      status: "ended"
                    })
                  (ok { status: "won", winner: (some tx-sender), next: next })
                )
                (if is-full
                  (let ((next none))
                    (map-set games { game-id: game-id }
                      {
                        player-x: player-x,
                        player-o: player-o,
                        board: new-board,
                        turn: tx-sender,
                        winner: none,
                        status: "ended"
                      })
                    (ok { status: "draw", winner: none, next: next })
                  )
                  (let ((next (some (if (is-eq tx-sender player-x) player-o player-x))))
                    (map-set games { game-id: game-id }
                      {
                        player-x: player-x,
                        player-o: player-o,
                        board: new-board,
                        turn: (if (is-eq tx-sender player-x) player-o player-x),
                        winner: none,
                        status: "active"
                      })
                    (ok { status: "next-turn", winner: none, next: next })
                  )
                )
              )
            )
          )
        )
      )
      (err ERR_GAME_NOT_FOUND)
    )
  )
)

;; ------------------------------------------------------------
;; Read-only functions
;; ------------------------------------------------------------

(define-read-only (get-game (game-id uint))
  (ok (map-get? games { game-id: game-id })))

(define-read-only (get-total-games)
  (ok (var-get game-counter)))
