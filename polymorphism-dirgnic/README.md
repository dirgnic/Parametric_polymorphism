//test1 works 
//test3 works

[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/sPguvZJ5)
# Parametric Polymorphism

The goal of this project is to implement parametric polymorphism by
monomorphization. We recommend starting from the functional language from
homework 06.

The following could be an example source program:

```
def identity[A](x : A) {
  return x
}

def main() {
  let a = identity[Int](5);
  let b = identity[Bool](false);
  return 0
}
```

This should be transformed to something like:

```
def identity_Int(x : Int) {
  return x
}

def identity_Bool(x : Bool) {
  return x
}

def main() {
  let a = identity_Int(5);
  let b = identity_Bool(false);
  return 0
}
```

Replace this file with installation and usage instructions and provide at least
three example programs that demonstrate what you have achieved.


# 🔁 Parametric Polymorphism via Monomorphization

This project demonstrates the implementation of **parametric polymorphism** through **monomorphization**. Starting from the language from Homework 06, we added support for generic functions by duplicating them at compile time for each type they're used with.

---
Pipeline Idea:


<img width="407" alt="Screenshot 2025-04-01 at 00 38 31" src="https://github.com/user-attachments/assets/fa017cb2-f253-4f9f-9b28-5bd192d86b31" />


---

Installation & Usage

Requirements

- [Stack](https://docs.haskellstack.org/en/stable/README/)
- GHC >= 9.6.6

Build

```bash
stack build

-- Run a Test:

stack exec try2 test/test1.txt > test/test1.output

-- You can compare the output with:

diff test/test1.out test/test1.golden

Parametric polymorphism using monomorphization

Type-driven renaming (e.g., identity[Int] → identity_Int)

Basic expressions: variables, arithmetic, conditionals, and booleans

Lightweight interpreter for evaluation

-- Example Programs
```
test1: 
```

def add[A](x : A, y : A) {
  return x;
}

def main() {
  return add[Int](10, 20);
}

Transforms to:

def add_Int(x : Int, y : Int) {
  return x;
}

def main() {
  return add_Int(10, 20);
}

Output:
golden: Num 10
```
test2:
```
def choose[A](cond : Bool, x : A, y : A) {
  if cond then x else y;
}

def main() {
  let z = choose[Int](true, 1, 2);
  return 0;
}

Transforms to:

def choose_Int(cond : Bool, x : Int, y : Int) {
  if cond then x else y;
}

def main() {
  let z = choose_Int(true, 1, 2);
  return 0;
}

Output:

z = Num 1
golden: Num 0

```
test3: Identity polymorphism
```
def identity[A](x : A) {
  return x;
}

def main() {
  let a = identity ;
  let b = identity[Bool](false);
  return 0;
}

Transforms to:

def identity_Int(x : Int) {
  return x;
}

def identity_Bool(x : Bool) {
  return x;
}

def main() {
  let a = identity_Int(5);
  let b = identity_Bool(false);
  return 0;
}

Output:
a = Num 5
b = Bool False
golden: Num 0




