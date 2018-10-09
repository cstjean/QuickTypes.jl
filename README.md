# QuickTypes

[![Build Status](https://travis-ci.org/cstjean/QuickTypes.jl.svg?branch=master)](https://travis-ci.org/cstjean/QuickTypes.jl)

[![Coverage Status](https://coveralls.io/repos/cstjean/QuickTypes.jl/badge.svg?branch=master&service=github)](https://coveralls.io/github/cstjean/QuickTypes.jl?branch=master)

[![codecov.io](http://codecov.io/github/cstjean/QuickTypes.jl/coverage.svg?branch=master)](http://codecov.io/github/cstjean/QuickTypes.jl?branch=master)

Types are central to Julia programming, but the built-in `struct` and `mutable struct` definitions can be cumbersome to write. QuickTypes.jl provides two alternative macros, `@qstruct` and `@qmutable`, with a more convenient syntax:

```julia
using QuickTypes      # install with Pkg.add("QuickTypes")

# Equivalent to
# struct Wall
#    width
#    height
# end
@qstruct Wall(width, height)

# Optional and keyword-arguments
@qstruct Cat(name, age::Int, nlegs=4; species="Siamese")

# Parametric type
@qstruct Pack{T}(animals::Vector{T})

# Inheritance
abstract type Tree end
@qstruct Maple(qty_syrup::Float64) <: Tree

# Mutable structs work the same way
@qmutable Window(height::Float64, width::Float64)

# Arguments can be validated using do-syntax
@qstruct Human(name, height::Float64) do
    @assert height > 0    # arbitrary code, executed in the constructor
end
```

### More options

```julia
# _concise_show takes out the type parameters when printing the object
@qstruct Group{X}(members::X; _concise_show=true)
Group([1,1+1])
> Group([1,2])            # instead of Group{Array{Int64,1}}([1,2])

# `_fp` (for Fully Parametric) automatically adds type parameters. For example:
@qstruct_fp Plane(nwheels, weight::Number; brand=:zoomba)
# is equivalent to `@qstruct Plane{T, U <: Number, V}(nwheels::T, weight::U; brand::V=:zoomba)`
# For even greater specialization, see `?@qstruct_np`.
```

See also [Parameters.jl](https://github.com/mauro3/Parameters.jl).
