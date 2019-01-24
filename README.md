# Fetisch Math Library for F#

Fetisch is a self-contained math library, primarily focusing on Linear Algebra and Symbolic Algebra.

### Main Features

- BigRational, a custom bigint-based rational primitive
- Permutation, an API for dealing with permutations
- Symbolic Algebra
	- Expression Parser
	- Expression Simplification 
    - Expression Matching and Manipulation API (like grep & sed for algebraic expressions)
    - Numeric Calculator
- Linear Algebra:
	- Matrix, supports any mxn matrices of any underlying field type (not just BigRational but also symbolic expressions)
	- Matrix solver showing each step during transformation
    - standard Matrix operations, such as `determinant`, `trace`, and more.
    - and of course a `Vector<'T>`

### TODO
- LaTeX formatter
- Derivatives
- iterative determinant computation, so each computation step can be introspected (and even exported to LaTeX)
