# QuickTypes

[![Build Status](https://travis-ci.org/cstjean/QuickTypes.jl.svg?branch=master)](https://travis-ci.org/cstjean/QuickTypes.jl)

[![Coverage Status](https://coveralls.io/repos/cstjean/QuickTypes.jl/badge.svg?branch=master&service=github)](https://coveralls.io/github/cstjean/QuickTypes.jl?branch=master)

[![codecov.io](http://codecov.io/github/cstjean/QuickTypes.jl/coverage.svg?branch=master)](http://codecov.io/github/cstjean/QuickTypes.jl?branch=master)

`QuickTypes.@qtype` and `@qimmutable` are macros for defining types more
concisely, by writing down their constructor.

```julia
Pkg.clone("https://github.com/cstjean/QuickTypes.jl.git") # to install

using QuickTypes

@qtype Window(price::Int, color=:black; opacity::Float64=1.0)
```

is equivalent to:

```julia
type Window <: Any
    price::Int
    color
    opacity::Float64
    Window(price,color=:black; opacity=1.0)
        new(price,color,opacity)
end
```

It also supports parametric types and inheritance:

```julia
abstract Vehicle

@qtype Car{T<:Number, U}(size::T, nwheels::Int=4; manufacturer::U=nothing,
                         brand::String="off-brand") <: Vehicle

# expands into:

type Car{T <: Number,U} <: Vehicle
    size::T
    nwheels::Int
    manufacturer::U
    brand::String
    Car(size,nwheels=4; manufacturer=nothing,brand="off-brand") = 
        new(size,nwheels,manufacturer,brand)
end
Car{T <: Number,U}(size::T,nwheels::Int=4; manufacturer::U=nothing,brand::String="off-brand") =
    Car{T,U}(size,nwheels; manufacturer=manufacturer,brand=brand))
```

See also [Parameters.jl](https://github.com/mauro3/Parameters.jl) for a similar
macro, with a different syntax.