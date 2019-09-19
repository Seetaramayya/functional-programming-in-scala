# Functional Programming in Scala

This repository contains solved exercises in [Functional Programming in Scala](http://manning.com/bjarnason/) book.

Chapter descriptions:

* Part 1: Introduction to functional programing
  * ✅ Chapter 3: Functional data structures (page number: 29)
  * ✅ Chapter 4: Handling errors without exceptions (page number: 48)
    * Effect way to write `sequence` in terms of `traverse` (to avoid going through the collection twice)
    ```
      def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(identity)
      def traverse[E, A, B](l: List[A])(f: A => Either[E, B]): Either[E, List[B]]
    
    ```  
  * ✅ Chapter 5: Strictness and laziness (page number: 64)
  * ✅ Chapter 6: Purely functional state (page number: 78)

* Part 2: Functional Design and combinator libraries
  * Chapter 7: Purely functional parallelism (page number: 95)
  * Chapter 8: Property-based testing (page number: 124)
  * Chapter 9: Parser Combinator (page number: 146)

* Part 3: Functional Design Patterns
  * Chapter 10: Monoids (page number: 175)
  * Chapter 11: Monads (page number: 187)
  * Chapter 12: Applicative and traversable functors (page number: 205)

* Part 4: Breaking the rules: Effects and I/O
  * Chapter 13: External effects and I/O (page number: 229)
  * Chapter 14: local effects and the ST monad (page number: 254)
  * Chapter 15: Stream processing and incremental (page number: 268)
  
  
### Cats Kernel 

   Cats kernel laws, dumping these laws here for now.
  
|        Name         | Associative? | Commutative? | Identity? | Inverse? | Idempotent? |
|-------------------- |--------------|--------------|-----------|----------|-------------|
|Semigroup            |      ✅      |              |           |          |             |
|CommutativeSemigroup |      ✅      |      ✅      |           |          |             |
|Monoid               |      ✅      |              |     ✅    |          |             |
|Band                 |      ✅      |              |           |          |     ✅      |
|Semilattice          |      ✅      |      ✅      |           |          |     ✅      |
|Group                |      ✅      |              |    ✅     |    ✅    |             |
|CommutativeMonoid    |      ✅      |      ✅      |    ✅     |          |             |
|CommutativeGroup     |      ✅      |      ✅      |    ✅     |    ✅    |             |
|BoundedSemilattice   |      ✅      |      ✅      |    ✅     |          |     ✅      |
