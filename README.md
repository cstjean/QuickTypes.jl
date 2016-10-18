# QuickTypes

[![Build Status](https://travis-ci.org/cstjean/QuickTypes.jl.svg?branch=master)](https://travis-ci.org/cstjean/QuickTypes.jl)

[![Coverage Status](https://coveralls.io/repos/cstjean/QuickTypes.jl/badge.svg?branch=master&service=github)](https://coveralls.io/github/cstjean/QuickTypes.jl?branch=master)

[![codecov.io](http://codecov.io/github/cstjean/QuickTypes.jl/coverage.svg?branch=master)](http://codecov.io/github/cstjean/QuickTypes.jl?branch=master)

`QuickTypes.@qtype` and `@qimmutable` are macros for defining types more
concisely, by writing down their constructor.

```julia
Pkg.clone("https://github.com/cstjean/QuickTypes.jl.git") # to install

using QuickTypes

@qtype Door(width, height)
@qtype Window(price::Int, color=:blue; opacity::Float64=1.0)

Window(100; opacity=0.2)    # yields Window(100,:blue;opacity=0.2)
```

It macroexpands into:

```julia
type Window
    price::Int
    color
    opacity::Float64
    Window(price,color=:black; opacity=1.0)
        new(price,color,opacity)
end
```

`@qtype` and `@qimmutable` also support parametric types and inheritance:

```julia
abstract Vehicle

@qtype Car{T<:Number, U}(size::T, nwheels::Int=4; manufacturer::U=nothing,
                         brand::String="off-brand") <: Vehicle

Car(300, 6; manufacturer=:Toyota)  # yields Car{Int64,Symbol}(300,6;manufacturer=:Toyota,brand="off-brand")
```

See also [Parameters.jl](https://github.com/mauro3/Parameters.jl).