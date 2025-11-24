// scala
package generator

enum Ins:
  case Add, Sub, Mul, Div
  case Ldi(n: Int)
  case JZ(offset: Int)
  case JMP(offset: Int)
