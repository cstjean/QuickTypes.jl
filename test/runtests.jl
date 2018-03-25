using Compat: @compat
using QuickTypes
using QuickTypes: construct, roottypeof, fieldsof, type_parameters, roottype,
      tuple_parameters
using Base.Test

@compat abstract type Vehicle end

@qstruct Car{T<:Number, U}(size::T, nwheels::Int=4; manufacturer::U=nothing,
                              brand::String="off-brand") <: Vehicle

c = Car(10; manufacturer=("Danone", "Hershey"))

@test c.size==10
@test c.nwheels==4
@test c.manufacturer==("Danone", "Hershey")
@test c.brand=="off-brand"
# Check that the fields are in the right order
@test fieldnames(c) == [:size, :nwheels, :manufacturer, :brand]
# This is essentially the definition of these functions.
# This works in Julia 0.6, but not in 0.5. Since it's not part of the exported API,
# we diable the test.
# @test construct(roottypeof(c), fieldsof(c)...) == c
@test type_parameters(Vector{Int}) == Base.Core.svec(Int64, 1)
@test tuple_parameters(Tuple{Int, Float64}) == Base.Core.svec(Int64, Float64)
@inferred roottypeof(1=>2) == Pair

################################################################################

@qstruct Empty()
Empty()

# Used to yield:
#     WARNING: static parameter T does not occur in signature for Type.
#     The method will not be callable.
@qstruct Blah{T}()

################################################################################

@qstruct Boring(x::Int)
@inferred Boring(10)
@test Boring(10).x == 10
@qstruct ParametricBoring{X}(x::X)
@inferred ParametricBoring(10)
@test ParametricBoring(10).x === 10

@qstruct Kwaroo(x; y=10)
@test Kwaroo(5) == Kwaroo(5; y=10)

################################################################################

@qmutable Foo{T}(x::T; y=2) do
    @assert x < 10
end

@test_throws AssertionError Foo(11; y=10.0)
@test_throws AssertionError construct(Foo, 11, 10.0)

################################################################################
# Fully-parametric

@qstruct_fp Plane(nwheels::Number; brand=:zoomba) do
    @assert nwheels < 100
end <: Vehicle

@test_throws MethodError Plane{Int, Symbol}(2; brand=12)
@test Plane{Int, Symbol}(2; brand=:zoomba).brand == :zoomba
@test supertype(Plane) == Vehicle
@test_throws MethodError Plane("happy")

@qstruct_fp NoFields()   # was an error before it was special-cased

################################################################################
# Narrowly-parametric

@qstruct_fp Foo_fp(a, b)
@qstruct_np Foo_np(a, b)
convert_f(foo) = convert(foo.a, 10)
@test_throws(Exception, @inferred convert_f(Foo_fp(Int, 2)))
@inferred convert_f(Foo_np(Int, 2))
@test fieldtype(typeof(Foo_np(Int, 2)), :a) == Type{Int64}

