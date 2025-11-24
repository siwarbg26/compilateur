package test
import PCF.Calc.main

@main
def test(): Unit =
  test("test/test0.calc")

def test(file: String): Unit =
  val args = Array(file)
  main(args)


