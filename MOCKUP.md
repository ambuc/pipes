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

