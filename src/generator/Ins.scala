package generator

enum Ins :
  case Ldi(n: Int)      // Load immediate
  case Add, Sub, Mul, Div, Push
  case Test(i: List[Ins], j: List[Ins])  // Conditional branch
  case Search(p: Int)   // Load variable at De Bruijn index p
  case Extend           // Add accumulator to environment
  case Pushenv          // Save environment on stack
  case Popenv           // Restore environment from stack
  case Mkclos(idx: Int, code: List[Ins])  // Create closure with index
  case Apply            // Apply function
  case Mkrecclos(idx: Int, code: List[Ins])  // Create reflexive closure (for fix)