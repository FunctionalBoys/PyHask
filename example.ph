let x : int := 1
let y : float := 2.0
let o: int := x

def h() -> void:
  pass

class Hola:
  init:
    __init__(i: int):
      pass

  def f(d : float) -> int:
    return 1

class MemberedClass:
  init:
    let member1 : int
    let member2 : float

    __init__(i: int, f: float):
      print(i)
      print(f)
      self.member1 := i
      print(self.member1)
      self.member2 := f
      print(self.member2)
 
  def calc() -> float:
    print(self.member1)
    print(self.member2)
    return self.member1 * self.member2

class SonClass(MemberedClass):
  init:
    __init__(i: int, f: float):
      super.__init__(i,f)

  def calc() -> float:
    return super.calc()

def g(i : int) -> int:
  let b : bool := True
  if True:
    return 5
  for j :int := 0 : True : j := j + 1:
    if False:
      i := 200
    else
      continue
  
  let c : int := 5
  if c == 5:
    return 1

  return 2

def p(f: float) -> float:
  return 5.0

let l : char[5]
create a : Hola(5)
create m : SonClass(5+2,1.0+p(1.0))
print(m.calc())