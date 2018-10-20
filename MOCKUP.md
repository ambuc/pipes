# MOCKUP

 01234567890123456789  :: grid: 20 x 10
0┏┯━━━━━━━━━━━━━━━━━┓  :: pieces: a ─ │      <=> ━ ┃      (and intermediates)
1┃┬                 ┃           : b  ┌ ┐ └ ┘ <=>  ┏ ┓ ┗ ┛ 
2┃╭╯│               ┃           : c ┼        <=> ╋
3┃   .              ┃           : d  ├ ┬ ┤ ┴ <=>  ┣ ┳ ┫ ┻
4┃     .            ┃           : e ╴ ╵ ╶ ╷  <=> ╸ ╹ ╺ ╻
5┃                  ┃           
6┃                  ┃
7┃         .......  ┃
8┃                ..┃
9┗━━━━━━━━━━━━━━━━━┷┛

  012345678     01234     01234     01234
0 .........    0         0┌-──┐    0 ┄ ┄
1 . . . . . => 1 + +  => 1│a b│ => 1┆├┆┐┆
2 .........    2         2│ ╷ │    2 ┄ ┄
3 . . . . .    3 + +     3│c│d│    3┆╵┆│┆
4 .........    4         4└─┴-┘    4 ┄ ┄
5 . . . . .
6 .........                          so 0,0 <- (1,1) +- (1,0),(0,1),(-1,0),(0,-1)
7 . . . . .                             1,1 <- (3,3) += (...)
8 .........                             2,2 <- (5,5)
                                        w,h <- (2w+1, 2h+1) + (+-1,0), (0,+-1)
  4 inc. <-----------  2x2 .
   (5)

for range 4x4
  - divide in quarters [1,2,3?] x [1,2,3?]
  - pick holes in three (news choose 3)
  - repeat for subquarters


Setup)
 1) randomly pick two terminuses on the edge
 2) randomly pick path between them
 3) randomly pick pieces along that path which contain the necessary edges
 4) randomly pick any piece for the remaining void squares (some can be blank)
 5) randomly rotate all pieces
Solve)
 1) wasd / arrows to highlight a piece (denoted by double thickness?)
 2) space to rotate cw, shift-space to rotate ccw
 3) on each position, evaluate board for solve? (this can be cached)
 4) (optional) independent of repl loop, animate pulsing water along connected
    portions of pipes (length 2? framerate?) this pulsing ends at the connected
    graph's edge and skips any pipe with the cursor on it.
      ───
      ╾──
      ━──
      ╼╾─
      ─━─
      ─╼╾
      ──━
      ──╼
      ───

