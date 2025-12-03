package generator

enum Ins :
  case Ldi(n: Int)
  case Add, Sub, Mul, Div, Push
  case Test(i: List[Ins], j: List[Ins])
  case Search(p: Int)
  case Extend
  case Pushenv
  case Popenv
  case Mkclos(code: List[Ins])
  case Apply
  case Return
