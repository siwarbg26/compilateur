package generator

enum ATerm:
  case AInt(n: Int)
  case AVar(name: String, index: Int)
  case AAdd(left: ATerm, right: ATerm)
  case ASub(left: ATerm, right: ATerm)
  case AMul(left: ATerm, right: ATerm)
  case ADiv(left: ATerm, right: ATerm)
  case AIf(cond: ATerm, thenBranch: ATerm, elseBranch: ATerm)
  case ALet(name: String, value: ATerm, body: ATerm)
  case AFun(param: String, body: ATerm)
  case AApp(func: ATerm, arg: ATerm)
type Code = List[Ins]
