# SIUnits

[![Build Status](https://travis-ci.org/loladiro/SIUnits.jl.png)](https://travis-ci.org/loladiro/SIUnits.jl)
[![Coverage Status](https://coveralls.io/repos/loladiro/SIUnits.jl/badge.png?branch=master)](https://coveralls.io/r/loladiro/SIUnits.jl?branch=master)

This package provides efficient unit-checked computations based for units in the
SIUnits systems. To use this package use (after installing it using Pkg.add)
```
using SIUnits
```
optionally, you may also use
```
using SIUnits
using SIUnits.ShortUnits
```
instead, to load a number of abbreviations into the current namespace
(e.g. `kg` instead of `KiloGram`). These abbreviations are not loaded
by default to avoid flooding the namespace where this is not desired. 
Note that all examples in this README assume that the second form was
used. To make the examples work with the first form, just substitute the
written out names, e.g. `Volt` for `V` and `Nano*Meter` for `nm`.

# Usage

`SIUnits.jl` integrates into the number promotion system and all the usual
arithmetic operations (`+`,`-`,`*`,`/`,`^`,`sqrt`) work as one would expect. 
In particular, addition and subtraction is allowed between two quantities with
the same units:

```julia
julia> 1V + 2V
3 kg m²s⁻³A⁻¹

julia> (1//2)s - 1s
-1//2 s
```

However, you may not add or subtract quantities whose units differ:
```
julia> 1s + 2V
ERROR: Unit mismatch. Got (s ) + (kg m²s⁻³A⁻¹)
```

Consistently, multiplication and division increase or decrease the exponents 
of the base units, e.g.:

```
julia> 1N
1 kg m s⁻²

julia> 1N/m
1 kg s⁻²

julia> 1N*s^2
1 kg m
```

You may also take square roots of quantities with units:
```
julia> sqrt(1s^2)
1.0 s
```

However, currently, the result must have integral exponents. Support for
fractional exponents may be added in the future:

```
julia> sqrt(1m)
ERROR: InexactError()
```

# Converting between unitful and unitless quantities

`SIUnits.jl` does not define implicit `convert` methods to avoid silently losing
unit information where this may be undesirable. Instead, `SIUnits.jl` extends
the various forced type conversions e.g. `float`, `float64` and `int`. Packages
writing generic code should use these where a specific unitless value is 
required. 

# Implementation details

Where possible (i.e. where the compiler can reason about the
type of a variable), there is no runtime overhead. For example:

```julia
julia> [1V, 2V, 3V]
3-element Array{SIQuantity{Int64,2,1,-3,-1,0,0,0},1}:
 1 kg m²s⁻³A⁻¹
 2 kg m²s⁻³A⁻¹
 3 kg m²s⁻³A⁻¹

julia> sizeof(ans)
24

```

this is the same amount of storage as that taken up by a simple array of three
64bit integers:

```julia
julia> sizeof([1 2 3])
24
```

This shows that there is no runtime memory overhead. Similarly, the code 
generated to add two 64bit integers:

```
julia> code_native(+,(Uint64,Uint64))
    .section    __TEXT,__text,regular,pure_instructions
Filename: int.jl
Source line: 42
    push    RBP
    mov RBP, RSP
Source line: 42
    add RDI, RSI
    mov RAX, RDI
    pop RBP
    ret
```

is exactly the same as the code two add two 64bit integer quantities with units:

```
julia> code_native(+,typeof((1V,2V)))
    .section    __TEXT,__text,regular,pure_instructions
Filename: /Users/kfischer/.julia/SIUnits/src/SIUnits.jl
Source line: 122
    push    RBP
    mov RBP, RSP
Source line: 122
    add RDI, RSI
Source line: 123
    mov RAX, RDI
    pop RBP
    ret
``` 

This is achieved by keeping track of the exponents as part of the type rather
than of the value. An SIQuantity is defined as

```
    immutable SIQuantity{T<:Number,m,kg,s,A,K,mol,cd} <: Number
        val::T
    end
```

where the `m,kg,s,A,K,mol,cd` type parameters keep track of the exponents of 
the respective base units. This definition is the core of the package. The
rest makes it play nicely with the numeric promotion sytem to make sure that
generic code will work just fine on `SIQuantities`.
