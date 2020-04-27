let x : int := 1

def h() -> void:
  pass

class Hola:
  init:
    Hola(i: int):
      pass

  def f(d : float) -> int:
    let de : string := 'hola'
    pass

class MemberedClass:
  init:
    let member1 : int
    let member2 : float

    MemberedClass(i: int, f: float):
      self.member1 := i
      self.member2 := f

  def f(i: int) -> void:
    self.member1 := i

def g(i : int) -> int:
  let b : bool := True
  if True:
    return 5
  for j :int := 0 : True : j := j + 1:
    if False:
      i := 200
      break
    else
      continue
  
  let c : int := 5
  if c == 5:
    return 1

  return 2

def p(f: float) -> float:
  return 5.0

main:
  create a : Hola(5)
  create m : MemberedClass(5+2,1.0+p(1.0))
  m.f(1)
  g(2+m.f(1))
  while True:
    print(1)
