module QuickTypes

using MacroTools: @capture, prewalk, @match, splitarg, @q, splitdef, combinedef, isdef
import ConstructionBase

export @qmutable, @qstruct
export @qmutable_fp, @qstruct_fp
export @qstruct_np, @qmutable_np
export @qfunctor
export @destruct

const special_kwargs = [:_define_show, :_concise_show]

# These are not exported for now, because they are rather specific extensions.
""" For a type X defined with `@qmutable/@qstruct` and with fields `a, b, c,
...`, `QuickTypes.construct(X, 1, 2, 3...)` is a purely-positional constructor
of `X`. This is useful for writing generic structure traversal. For any object
of a type defined by `@qstruct`, this holds:

    QuickTypes.construct(QuickTypes.roottypeof(o), QuickTypes.fieldsof(o)...) == o
"""
function construct end


""" `roottypeof(obj)` returns the type of obj with generic parametric types. Eg.
`roottypeof(a::SomeType{Int}) -> SomeType{T}`. See `QuickTypes.construct` """
@generated roottypeof(obj_type) = roottype(obj_type)
""" `roottype(typ::Type)` returns the parameterless type. Eg. `roottype(X{A}) => X` """
roottype(typ::Type) = Base.typename(typ).wrapper
type_parameters(typ) = typ.parameters
""" `fieldtypes(typ)` returns the types of the fields of a composite type. """
fieldtypes(typ::Type) = # not type-stable ATM. The generated function seemed to have
                        # fieldnames return [] for some reason. Maybe I should try
                        # (fieldtype(typ, 1), fieldtype(typ, 2), ...) with `ntuple`
    tuple((fieldtype(typ, f) for f in fieldnames(typ))...)
""" `tuple_parameters{T<:Tuple}(::Type{T})` returns the type of each element of the
tuple, as `svec(type1, type2, ...)` """
tuple_parameters(::Type{T}) where {T<:Tuple} = type_parameters(T)
@generated tuple_parameters_stable(::Type{T}) where {T<:Tuple} =
    :(error("This is broken; it returns a type-tuples, but those cannot be fully-typed, they are instead Tuple{DataType, DataType, ...}. TODO: perhaps we can return Some{...}?"))
    #tuple(type_parameters(T)...)
@generated type_length(::Type{T}) where T = length(fieldnames(T))
is_mutable(::Type{T}) where T = T.mutable

""" `fieldsof(obj)` returns the fields of `obj` in a tuple.
See also `QuickTypes.construct` """
@generated fieldsof(obj) = :(tuple($([:(obj.$field)
                                      for field in fieldnames(obj)]...)))

type_simple_name(ty::Type)::Symbol = ty.name.name

################################################################################

""" `parse_funcall(fcall)`

Parses `fname(args; kwargs)`. Returns `(fname, args, kwargs, constraints)`
"""
function parse_funcall(fcall)
    if @capture(fcall, fun_(args__; kwargs__) do; constraints_ end)
        return (fun, args, kwargs, constraints)
    elseif @capture(fcall, fun_(args__) do; constraints_ end)
        return (fun, args, Any[], constraints)
    end

    if @capture(fcall, fun_(args__; kwargs__))
    elseif @capture(fcall, fun_(args__))
        kwargs = Any[]
    else
        error("Not a funcall: $fcall")
    end

    if !isempty(args) && @capture(args[1], ()->constraints_)
        args = args[2:end]
    else
        constraints = nothing
    end

    return (fun, args, kwargs, constraints)
end

function get_sym(e::Expr)
    @assert e.head==:(::)
    e.args[1]
end
get_sym(e::Symbol) = e

""" Build the Base.show function definition for that type """
function build_show_def(define_show::Bool, concise_show::Bool, name, fields, kwfields)
    if !define_show && !concise_show return nothing end

    quote
        function Base.show(io::IO, obj::$name)
            print(io, $(concise_show ? string(name) : @q(typeof(obj))))
            write(io, "(")
            # Positional args
            $([@q begin
               show(io, obj.$(get_sym(field)));
               $(field==last(fields) ? nothing : @q(write(io, ", ")))
               end
               for field in fields]...)
            # separating semicolon
            $(if !isempty(kwfields)
              @q(write(io, "; ")) end)
            # Keyword args
            $([@q begin
               write(io, $(string(get_sym(kwfield)))); write(io, "=");
               show(io, obj.$(get_sym(kwfield)));
               $(kwfield==last(kwfields) ? nothing : @q(write(io, ", ")))
               end
               for kwfield in kwfields]...)
            write(io, ")")
        end
    end
end

function all_type_vars_present(type_vars, args)
    # If we create an outer constructor whose parameters are not part of the args, we get
    #     > @qstruct Blah{T}()
    #     WARNING: static parameter T does not occur in signature for Type.
    #     The method will not be callable.
    # So we detect this case and don't emit an outer constructor
    s = Set(type_vars)
    for arg in args
        prewalk(arg) do x
            if isa(x, Symbol)
                delete!(s, x)
            end
            x
        end
    end
    return isempty(s)
end

narrow_typeof(t::Type{T}) where {T} = Type{T}
narrow_typeof(t::T) where {T} = T

# Helper for @qmutable/@qstruct
# narrow_types means that
function qexpansion(__source__, def::Expr, mutable::Bool, fully_parametric::Bool,narrow_types::Bool)
    if !@capture(def, typ_def_ <: parent_type_)
        typ_def = def
        parent_type = :Any
    end
    typ, args, kwargs, constraints = parse_funcall(typ_def)
    typ_def = @q($typ($(args...); $(kwargs...)))
    if fully_parametric
        typ, typ_def, args, kwargs = make_parametric(typ, typ_def, args, kwargs)
    end

    get_type_var(v::Symbol) = v
    get_type_var(e::Expr) = e.args[1]
    if @capture(typ, name_{type_params__})
        parametric = true
        type_vars = map(get_type_var, type_params)
        type_with_vars = @q($name{$(type_vars...)})
    else
        type_vars = []
        type_params = []
        parametric = false
        name = typ
        type_with_vars = name
    end
    # Parse the regular arguments
    fields = Any[]; kwfields = Any[]
    constr_args = Any[]; constr_kwargs = Any[]
    o_constr_args = Any[]; o_constr_kwargs = Any[]
    new_args = Any[]
    arg_names = Symbol[]
    reg_kwargs = Any[] # the passed kwargs, but without _concise_show et al.
    for arg in args
        arg_name, arg_type, slurp, default = splitarg(arg)
        if slurp
            @assert arg_type == :Any "Slurping with type arguments not supported"
            @assert default === nothing "Slurping with default not supported"
            arg_type = :Tuple
            push!(constr_args, arg)
            push!(o_constr_args, arg)
        else
            push!(constr_args,
                  default === nothing ? arg_name : Expr(:kw, arg_name, default))
            push!(o_constr_args, arg_name)
        end
        push!(fields, @q($arg_name::$arg_type))
        push!(new_args, arg_name)
        push!(arg_names, arg_name)
    end
    # Parse keyword-arguments
    define_show = nothing # see after the loop
    concise_show = false # default
    for kwarg in kwargs  # keyword arguments
        arg_name, arg_type, slurp, default = splitarg(kwarg)
        if arg_name == :_define_show
            define_show = default::Bool
            continue
        end
        if arg_name == :_concise_show
            concise_show = default::Bool
            continue
        end
        if slurp
            @assert arg_type == :Any "Slurping with type arguments not supported"
            @assert default === nothing "Slurping with default not supported"
            arg_type = Base.Iterators.Pairs
            push!(new_args, arg_name)
            push!(constr_kwargs, kwarg)
            push!(o_constr_kwargs, kwarg)
        else
            push!(new_args, arg_name)
            push!(constr_kwargs,
                  default === nothing ? arg_name : Expr(:kw, arg_name, default))
            push!(o_constr_kwargs, Expr(:kw, arg_name, arg_name))
        end
        push!(reg_kwargs, kwarg)
        push!(kwfields, @q($arg_name::$arg_type))
        push!(arg_names, arg_name)
    end
    # By default, only define Base.show when there are keyword arguments --- otherwise
    # the native `show` is perfectly sufficient.
    if define_show === nothing; define_show = !isempty(kwfields) end

    # -------------- end of parsing -------------

    if narrow_types
        given_types = [@q($QuickTypes.narrow_typeof($a))
                       for a in (fields..., kwfields...)]
    else
        given_types = type_vars
    end
    inner_constr = @q begin
        function $type_with_vars($(constr_args...);
                                 $(constr_kwargs...)) where {$(type_params...)}
            $__source__
            $constraints
            return new{$(type_vars...)}($(new_args...))
        end
    end
    straight_constr = @q function $name($(args...); $(reg_kwargs...)) where {$(type_vars...)}
        $__source__
        $name{$(given_types...)}($(o_constr_args...); $(o_constr_kwargs...))
    end
    type_def =
        @q begin
            $__source__
            Base.@__doc__ $(Expr(:struct,
                                 mutable, Expr(:<:, typ, parent_type),
                                 Expr(:block, fields..., kwfields...,
                                      inner_constr,
                                      ((parametric &&
                                        all_type_vars_present(type_vars, [args; kwargs]))
                                       ? [straight_constr] : [])...)))
        end
    construct_def = quote
         function $QuickTypes.construct(::Type{$name}, $(arg_names...))
             $name($(o_constr_args...);
                   $(o_constr_kwargs...))
         end
         function $QuickTypes.ConstructionBase.constructorof(::Type{<:$name})
             (args...) -> $QuickTypes.construct($name, args...)
         end
    end
    @gensym obj
    unpack_def = quote
        macro $(Symbol(:unpack_, name))(obj_expr)
            esc(Expr(:block,
                     Expr(:(=), $(Expr(:quote, obj)), obj_expr),
                     $([Expr(:quote, :($arg = $obj.$arg)) for arg in arg_names]...)))
        end
    end
    esc(Expr(:toplevel,
             type_def,
             construct_def,
             build_show_def(define_show, concise_show, name, fields, kwfields),
             unpack_def,
             nothing))
end


""" Quick type definition.

```julia
@qstruct Car(size, nwheels::Int=4; brand::String="unnamed") <: Vehicle
```

expands into

```julia
struct Car <: Vehicle
    size
    nwheels::Int
    brand::String
    Car(size, nwheels=4; brand="unnamed") = new(size, nwheels, brand)
end
```

Also supports parametric types: `@qstruct Door{T}(size::T)`. Invariants can be
enforced using do-syntax:
```julia
@qstruct Human(name, height::Float64) do
    @assert height > 0    # arbitrary code, executed before creating the object
end
```

Note: `@qstruct` automatically defines a `Base.show` method for the new type,
unless `_define_show=false` (eg. `@qstruct(x, y; _define_show=false)`).
"""
macro qstruct(def)
    return qexpansion(__source__, def, false, false, false)
end

""" Quick mutable struct definition. See ?@qstruct """
macro qmutable(def)
    return qexpansion(__source__, def, true, false, false)
end

# -----------------------------------------------------------------------------
# Fully-parametric


# Helper for qstruct_fp
function make_parametric(typ, typ_def, args, kwargs)
    all_types = []
    type_counter = 1
    function new_type(parent)
        new_ty = Symbol(:T, type_counter)
        type_counter += 1
        push!(all_types, @q($new_ty <: $parent))
        return new_ty
    end
    #add_type(field::Symbol) = @q($field::$(new_type()))
    function add_type(field)
        name, parent_type, slurp, val = splitarg(field)
        @assert !slurp "Slurping not supported. TODO"
        if name in special_kwargs
            return field
        elseif val==nothing
            return @q($name::$(new_type(parent_type)))
        else
            return Expr(:kw, @q($name::$(new_type(parent_type))), val)
        end
    end

    typed_args = map(add_type, args)
    typed_kwargs = map(add_type, kwargs)
    new_typ = @q($typ{$(all_types...)})

    if type_counter == 1
        # Has to special-case the "no type parameters" case because of
        # https://github.com/JuliaLang/julia/issues/20878
        return (typ, typ_def, args, kwargs)
    else
        return (new_typ, @q($new_typ($(typed_args...); $(typed_kwargs...))),
                typed_args, typed_kwargs)
    end
end

""" Fully-parametric version of `@qstruct`. `@qstruct_fp Foo(a, b=2)` is like
`@qstruct Foo{T, U}(a::T, B::U=2)` """
macro qstruct_fp(def)
    return qexpansion(__source__, def, false, true, false)
end
""" Fully-parametric version of `@qmutable`. `@qmutable_fp Foo(a, b=2)` is like
`@qmutable Foo{T, U}(a::T, B::U=2)` """
macro qmutable_fp(def)
    return qexpansion(__source__, def, true, true, false)
end


""" Narrowly-parametric version of `@qstruct`. `@qstruct_np Foo(a, b=2)` is like
`@qstruct Foo{T, U}(a::T, B::U=2)`, but it will additionally specialize on types:
`Foo(Int, 2.0) ==> Foo{Type{Int64},Float64}(Int64, 2.0)` """
macro qstruct_np(def)
    return qexpansion(__source__, def, false, true, true)
end
macro qmutable_np(def)
    return qexpansion(__source__, def, true, true, true)
end

################################################################################
# @qfunctor

""" Abstract type for qfunctors. Was introduced to improve printing of QFunctor """
abstract type QFunctor <: Function end

Base.show(io::IO, mime::MIME"text/plain", qf::QFunctor) =
    # Without this method, functors print like functions, i.e.
    #     (::Foo) (generic function with 1 methods)
    # which is bothersome because the fields don't show up.
    Base.@invoke(Base.show(io::typeof(io), mime::typeof(mime), qf::Any))

"""
```julia
@qfunctor function Action(verb::Symbol)(what)
     println(verb, "ing of ", what)
end <: AbstractAction
```

is equivalent to

```julia
@qstruct Action(verb::Symbol) <: AbstractAction
function (__self__::Action)(what)
    let verb = __self__.verb
        println(verb, "ing of ", what)
    end
end
```

Note that if no parent type is specified, `<: Function` is used.
"""
macro qfunctor(fdef0)
    if @capture(fdef0, A_ <: parenttype_)
        fdef = A
    elseif @capture(fdef0, A_ <: parenttype_ = rhs_)
        fdef = :($A = $rhs)
    else
        fdef = fdef0
        parenttype = :($QFunctor)
    end
    di = splitdef(fdef)
    if @capture(di[:body], begin <:ptype_; restofbody__ end) # function Foo(x)(k) <: P ... end
        parenttype = ptype
        di[:body] = Expr(:block, restofbody...)
    end
    type_def = di[:name]
    if @capture(type_def, typename_(args__; kwargs__))
        all_args = map(first ∘ splitarg, vcat(args, kwargs))
    else
        @assert @capture(type_def, typename_(args__))
        all_args = map(first ∘ splitarg, args)
    end
    di[:name] = :(__self__::$typename)
    if @capture(type_def, tname_{Ts__}(__))
        di[:whereparams] = tuple(Ts..., get(di, :whereparams, ())...)
    end
    di[:body] =
        @q begin
            $__source__
            # I wish I could have used @unpack_Foo, but it seems we can't define a macro and use
            # it in the same top-level block.
            $(Expr(:tuple, Expr(:parameters, all_args...))) = __self__ # namedtuple destructuring
            $(di[:body])
        end
    esc(quote
        $QuickTypes.@qstruct $type_def <: $parenttype
        $(combinedef(di))
        end)
end

################################################################################
# @destruct

""" Implement this function for objects that aren't meant to be destructured. Throw an error. """
check_destructurable(x) = nothing

macro destruct_assignment(ass)
    if @capture(ass, typ_(args__; kwargs__) = rhs_)
        nothing
    elseif @capture(ass, typ_(args__) = rhs_)
        kwargs = []
    elseif @capture(ass, (args__,) = rhs_)
        kwargs = []
        typ = Tuple
    else
        @assert @capture(ass, lhs_ = _)  # regular assignment
        return esc(ass)
    end
    obj = rhs isa Symbol ? rhs : gensym(:obj)  # to avoid too many gensyms
    body = []
    for (i, a) in enumerate(args)
        push!(body, :($QuickTypes.@destruct_assignment $a = $Base.getfield($obj, $i)))
    end
    for x in kwargs
        local_var, prop = @capture(x, a_ = b_) ? (a, b) : (x, x)
        prop::Symbol
        push!(body, :($local_var = $obj.$prop))
    end
    esc(@q begin
        $obj = $rhs::$typ
        $QuickTypes.check_destructurable($obj)
        $(body...)
        end)
end

macro destruct_function(fdef)
    di = splitdef(fdef)
    prologue = []
    function proc_arg(a)
        if @capture(a, f_(__))
            @gensym g
            push!(prologue, :($QuickTypes.@destruct_assignment $a = $g))
            return :($g::$f)
        else
            return a
        end
    end
    if haskey(di, :name) # anonymous functions don't have names
        di[:name] = proc_arg(di[:name])     # handle callable objects
    end
    di[:args] = map(proc_arg, di[:args])
    di[:kwargs] = map(proc_arg, get(di, :kwargs, []))
    di[:body] = @q begin
        $(prologue...)
        $(di[:body])
    end
    return esc(combinedef(di))
end

""" Destructuring for objects.

```julia
struct House
    owner
    price
    n_windows
end

@destruct function energy_cost(House(o; n_windows))
    return o == "Bob" ? 10000 : n_windows * 5
end
```

becomes

```julia
function energy_cost(temp_var::House)
    o = getfield(temp_var, 1)         # get the first field
    n_windows = temp_var.n_windows

    return o == "Bob" ? 10000 : n_windows * 5
end
```

This enables syntax like `@destruct mean_price(DataFrame(; price)) = mean(price)`. Destructuring
can also be applied to assignments with `@destruct Ref(x) := ...` and `for` loops. It can be nested:
`@destruct energy_cost(House(Landlord(name, age))) = ...`

Type annotations on fields _do not participate in dispatch_, but are instead converted to.

```julia
julia> @d foo(Ref(a::Int)) = a
foo (generic function with 1 method)

julia> foo(Ref(2.0))
2  # not 2.0
```

`@d ...` is a synonym for `@destruct`. Import it with `using QuickTypes: @d`.
"""
macro destruct(expr::Expr)
    if @capture(expr, lhs_ := rhs_)
        esc(:($QuickTypes.@destruct_assignment $lhs = $rhs))
    elseif @capture(expr, for x_ in seq_ body__ end)
        @gensym g
        esc(@q begin
            for $g in $seq
                $QuickTypes.@destruct_assignment $x = $g
                $(body...)
            end
            end)
    elseif isdef(expr)
        esc(:($QuickTypes.@destruct_function $expr))
    else
        error("@destruct does not handle expressions like $expr")
    end
end

""" Short-hand for `@destruct` """
macro d(expr)
    esc(:($QuickTypes.@destruct $expr))
end

end # module
