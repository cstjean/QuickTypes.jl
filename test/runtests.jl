using Compat: @compat
using QuickTypes
using QuickTypes: construct, roottypeof, fieldsof
using Base.Test

@compat abstract type Vehicle end

@qimmutable Car{T<:Number, U}(size::T, nwheels::Int=4; manufacturer::U=nothing,
                              brand::String="off-brand") <: Vehicle

c = Car(10; manufacturer=("Danone", "Hershey"))

@test c.size==10
@test c.nwheels==4
@test c.manufacturer==("Danone", "Hershey")
@test c.brand=="off-brand"
# Check that the fields are in the right order
@test fieldnames(c) == [:size, :nwheels, :manufacturer, :brand]
# This is essentially the definition of these functions.
# I'm not sure that it can be made to work in 0.6
# https://discourse.julialang.org/t/getting-the-base-type-in-0-6/2425/4
## @test construct(roottypeof(c), fieldsof(c)...) == c

################################################################################

@qimmutable Empty()
Empty()

################################################################################

@qimmutable Kwaroo(x; y=10)
@test Kwaroo(5) == Kwaroo(5; y=10)

################################################################################

@qtype Foo{T}(x::T; y=2) do
    @assert x < 10
end

@test_throws AssertionError Foo(11; y=10.0)
@test_throws AssertionError construct(Foo, 11, 10.0)

################################################################################
# Fully-parametric

@qstruct_fp Plane(nwheels; brand=:zoomba) <: Vehicle

@test_throws MethodError Plane{Int, Symbol}(2; brand=12)
@test Plane{Int, Symbol}(2; brand=:zoomba).brand == :zoomba
@test supertype(Plane) == Vehicle
