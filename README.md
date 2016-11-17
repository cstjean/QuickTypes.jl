# QuickTypes

[![Build Status](https://travis-ci.org/cstjean/QuickTypes.jl.svg?branch=master)](https://travis-ci.org/cstjean/QuickTypes.jl)

[![Coverage Status](https://coveralls.io/repos/cstjean/QuickTypes.jl/badge.svg?branch=master&service=github)](https://coveralls.io/github/cstjean/QuickTypes.jl?branch=master)

[![codecov.io](http://codecov.io/github/cstjean/QuickTypes.jl/coverage.svg?branch=master)](http://codecov.io/github/cstjean/QuickTypes.jl?branch=master)

Types are central to Julia programming, but the built-in `type` and `immutable` definitions can be cumbersome to write. QuickTypes.jl provides two alternative macros, `@qtype` and `@qimmutable`, with a more convenient syntax:

```julia
using QuickTypes      # install with Pkg.add("QuickTypes")

# Equivalent to
# type Wall
#    width
#    height
# end
@qtype Wall(width, height)

# Optional and keyword-arguments
@qtype Cat(name, age::Int, nlegs=4; species="Siamese")

# Parametric type
@qtype Pack{T}(animals::Vector{T})

# Inheritance
abstract Tree
@qtype Maple(qty_syrup::Float64) <: Tree

# Immutables work the same way
@qimmutable SquaredNumber(x2::Number)

# Arguments can be validated with do-syntax
@qimmutable Human(name, height::Float64) do
    @assert height > 0    # arbitrary code, executed before creating the object
end
```

See also [Parameters.jl](https://github.com/mauro3/Parameters.jl).