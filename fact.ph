def factorial(i:int) -> int:
  if i == 0:
    return 1
  return i * factorial(i-1)

let i : int
i := read
print(factorial(i))