package challenge

import base.Challenge

object Day23b extends Challenge {
/*
   Here's the reasoning behind the optimized program

  |set b 65
  |set c b
  |jnz a 2
  |jnz 1 5
  |mul b 100
  |sub b -100000
  |set c b
  |sub c -17000
  |set f 1
  |set d 2
  |set e 2
  |set g d
  |mul g e
  |sub g b
  |jnz g 2
  |set f 0
  |sub e -1
  |set g e
  |sub g b
  |jnz g -8
  |sub d -1
  |set g d
  |sub g b
  |jnz g -13
  |jnz f 2
  |sub h -1
  |set g b
  |sub g c
  |jnz g 2
  |jnz 1 3
  |sub b -17
  |jnz 1 -23
  |
  |===============
  |
  |b = c = 65
  |if (a != 0)
  |    b = b * 100 + 100000
  |    c = b + 17000
  |do
  |    f = 1
  |    d = 2
  |    do
  |        e = 2
  |        do
  |            g = d * e -b
  |            if (g == 0)
  |                f = 0
  |            e = e + 1
  |            g = e - b
  |        while g != 0
  |        d = d+1
  |        g = d - b
  |    while g != 0
  |    if (f != 0)
  |        h = h + 1
  |    g = b - c
  |    if (g != 0)
  |        break
  |    b = b + 17
  |while (true)
  |
  |===============
  |
  |b = 106500
  |c = 123500
  |for (b in range(106500, c , 17) {
  |    f = 1
  |    for d in range(2, b, 1) {
  |        for (e in range(2, b, 1)
  |            if (d * e == b) // Find two numbers (d, e) whose product is b i.e. making b a non-prime number
  |                f = 0
  |        }
  |    }
  |    if (f == 0) h = h + 1 // increment counter if non-prime was found
  |}
  |
*/

  def isPrime(i: Int): Boolean = !(2 until i).exists(x => i % x == 0)

  override def run(): Any = {
    106500.to(123500).by(17).count(!isPrime(_))
  }

}
