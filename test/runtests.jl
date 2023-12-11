using QuickTypes
using QuickTypes: construct, roottypeof, fieldsof, type_parameters, roottype,
      tuple_parameters, @d
using Test
using ConstructionBase: setproperties

abstract type Vehicle end

@qstruct Car{T<:Number, U}(size::T, nwheels::Int=4; manufacturer::U=nothing,
                           brand::String="off-brand") <: Vehicle

c = Car(10; manufacturer=("Danone", "Hershey"))

@test c.size==10
@test c.nwheels==4
@test c.manufacturer==("Danone", "Hershey")
@test c.brand=="off-brand"
c2 = @inferred setproperties(c, (size=42, nwheels=8))
@test c2.nwheels == 8
@test c2.size == 42
@test c2.brand == c.brand
# Check that the fields are in the right order
@test collect(fieldnames(Car)) == [:size, :nwheels, :manufacturer, :brand]
# This is essentially the definition of these functions.
@test construct(roottypeof(c), fieldsof(c)...) == c
@test type_parameters(Vector{Int}) == Base.Core.svec(Int64, 1)
@test tuple_parameters(Tuple{Int, Float64}) == Base.Core.svec(Int64, Float64)
@inferred roottypeof(1=>2) == Pair

################################################################################

@qstruct Empty()
Empty()
@test setproperties(Empty(), NamedTuple()) === Empty()

# Used to yield:
#     WARNING: static parameter T does not occur in signature for Type.
#     The method will not be callable.
@qstruct Blah{T}()

################################################################################

@qstruct Boring(x::Int)
@inferred Boring(10)
@test Boring(10).x == 10
@test Boring(10.0).x == 10   # check that convert is called correctly
@qstruct ParametricBoring{X}(x::X; _concise_show=true)
@inferred ParametricBoring(10)
@test ParametricBoring(10).x === 10
o = ParametricBoring(1)
@test setproperties(o, x=:one).x === :one

@qstruct Kwaroo(x; y=10)
@test Kwaroo(5) == Kwaroo(5; y=10)
o = Kwaroo(5, y=10)
o2 = @inferred setproperties(o, (x=:five, y=100.0))
@test o2 isa Kwaroo
@test o2.x === :five
@test o2.y === 100.0

################################################################################
# Slurping

@qstruct Slurp(x, y=1, args...; kwargs...)
s = Slurp(1,2,3,4,5,6,7; x=1, y=10+2)
@test s.args == (3,4,5,6,7)
@test s.kwargs == pairs((x=1, y=12))
s2 = @inferred setproperties(s, x=:hello)
@test s2 isa Slurp
@test s2.x == :hello
@test s2.y == s.y

let
    @unpack_Slurp Slurp(10)
    @test x == 10
    @test y == 1
end

# Slurping (parametric case)

@qstruct SlurpParam{T}(x::AbstractVector{T}, y=1, args...; kwargs...)
s = SlurpParam([1,2,3,4,5,6,7], 8, 9,10; x=1, y=10+2)
@test s.args == (9,10)
@test s.kwargs == pairs((x=1, y=12))


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
# This used to be a MethodError, but since we moved the outer constructor inside
# the type, it has become a TypeError. Not sure why!
@test_throws TypeError Plane("happy")

@qstruct_fp NoFields()   # was an error before it was special-cased

o = Plane(4)
o2 = @inferred setproperties(o, brand=10, nwheels=o.nwheels)
@test o2 isa Plane
@test o2.brand === 10
@test o2.nwheels === o.nwheels

################################################################################
# Narrowly-parametric

@qstruct_fp Foo_fp(a, b)
@qstruct_np Foo_np(a, b)
convert_f(foo) = convert(foo.a, 10)
@test_throws(Exception, @inferred convert_f(Foo_fp(Int, 2)))
@inferred convert_f(Foo_np(Int, 2))
@test fieldtype(typeof(Foo_np(Int, 2)), :a) == Type{Int64}

@qstruct Issue11(;no_default_value)
@test_throws UndefKeywordError Issue11()

################################################################################
# @qfunctor

@qfunctor function Action(a; kw=100)(x)
    return a + x + kw
end
# Test that the
@test occursin("runtests.jl", string(which(Action(1), Tuple{Int}).file))

@test Action(2)(10) == 112

@qfunctor ParamAction{X}(a::X)(b::T) where T = (a, b, X, T)
@test ParamAction(1)(2.0) == (1, 2.0, Int, Float64)
@test string(ParamAction(1)) == "ParamAction{Int64}(1)"

io = IOBuffer();
show(io, MIME"text/plain"(), ParamAction(1))
@test String(take!(io)) == "ParamAction{Int64}(1)"

@qfunctor Bus(x)(y) <: Real = 2
@test Bus(2) isa Real

@qfunctor function Tractor(x)(p) <: Number
    x+p
end

@test Tractor(9)(3) == 12
@test Tractor(9) isa Number

################################################################################
# @destruct

@destruct foo(Ref(x)) = x+2
@destruct foo(Ref{Float64}(x)) = x+10
@test foo(Ref(10)) == 12
@test foo(Ref(10.0)) == 20
@destruct foo(a, (Ref{T} where T)(x)) = a + x

struct LongerStruct{X}
    a
    b
    c::X
end

@destruct function kwfun(LongerStruct{X}(u,v; c, bof=b)) where X
    return u, v, c, bof
end
@test kwfun(LongerStruct(4,5,6)) == (4,5,6,5)

@destruct nested(LongerStruct(Ref(Ref(a)))) = a
@test nested(LongerStruct(Ref(Ref(44)), 3, 4)) == 44

@destruct tup_destruct(Ref((a,Ref(b)))) = (a, b)
@test tup_destruct(Ref((1,Ref(2)))) == (1,2)

@d Ref(x) := Ref(111)
@test x == 111

@destruct for (LongerStruct(Ref(xx)), Ref(yy)) in [(LongerStruct(Ref(55), 10, 20), Ref(66))]
    @test (xx, yy) == (55, 66)
end

@d LongerStruct(x)(y) = (x, y)
@test LongerStruct(10, 20, 30)(5) == (10, 5)

@test @d((Ref(x), Ref(y)) -> x+2)(Ref(10), Ref(20)) == 12

@d with_type(Ref(a::Int)) = a
@test with_type(Ref(1)) === 1
@test with_type(Ref(2.0)) === 2   # This is not necessarily _desirable_, but it's reasonable...

struct NotDestruct
    a
end
@qstruct MyException()
QuickTypes.check_destructurable(::NotDestruct) = throw(MyException())

@d dontdestruct(NotDestruct(x)) = x

@test_throws MyException dontdestruct(NotDestruct(x))
