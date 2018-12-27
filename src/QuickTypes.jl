__precompile__()

module QuickTypes

using MacroTools: @capture, prewalk, @match, splitarg
import Compat

export @qmutable, @qstruct
export @qmutable_fp, @qstruct_fp
export @qstruct_np, @qmutable_np

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
roottype(typ::Type) = Compat.TypeUtils.typename(typ).wrapper
type_parameters(typ) = typ.parameters
""" `fieldtypes(typ)` returns the types of the fields of a composite type. """
fieldtypes(typ::Type) = # not type-stable ATM. The generated function seemed to have
                        # fieldnames return [] for some reason. Maybe I should try
                        # (fieldtype(typ, 1), fieldtype(typ, 2), ...)
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

    :(function Base.show(io::IO, obj::$name)
        print(io, $(concise_show ? string(name) : :(typeof(obj))))
        write(io, "(")
        # Positional args
        $([:(show(io, obj.$(get_sym(field)));
             $(field==last(fields) ? nothing : :(write(io, ", "))))
           for field in fields]...)
        # separating semicolon
        $(if !isempty(kwfields)
            :(write(io, "; ")) end)
        # Keyword args
        $([:(write(io, $(string(get_sym(kwfield)))); write(io, "=");
             show(io, obj.$(get_sym(kwfield)));
             $(kwfield==last(kwfields) ? nothing : :(write(io, ", "))))
           for kwfield in kwfields]...)
        write(io, ")")
    end)
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
function qexpansion(def, mutable, fully_parametric, narrow_types)
    if !@capture(def, typ_def_ <: parent_type_)
        typ_def = def
        parent_type = :Any
    end
    typ, args, kwargs, constraints = parse_funcall(typ_def)
    typ_def = :($typ($(args...); $(kwargs...)))
    if fully_parametric
        typ, typ_def, args, kwargs = make_parametric(typ, typ_def, args, kwargs)
    end

    get_type_var(v::Symbol) = v
    get_type_var(e::Expr) = e.args[1]
    if @capture(typ, name_{type_params__})
        parametric = true
        type_vars = map(get_type_var, type_params)
        type_with_vars = :($name{$(type_vars...)})
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
        else
            push!(constr_args, 
                  default === nothing ? arg_name : Expr(:kw, arg_name, default))
        end            
        push!(fields, :($arg_name::$arg_type))
        push!(new_args, arg_name)
        push!(arg_names, arg_name)
        push!(o_constr_args, arg_name)
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
            arg_type = :(Vector{Pair})
            if VERSION < v"0.7-"
                push!(new_args, :([k => v for (k, v) in $arg_name]))
            else
                push!(new_args, :(collect(Pair, $arg_name)))
            end
            push!(constr_kwargs, kwarg)
        else
            push!(new_args, arg_name)
            push!(constr_kwargs, 
                  default === nothing ? arg_name : Expr(:kw, arg_name, default))
        end
        push!(reg_kwargs, kwarg)
        push!(kwfields, :($arg_name::$arg_type))
        push!(arg_names, arg_name)
        push!(o_constr_kwargs, Expr(:kw, arg_name, arg_name))
    end
    # By default, only define Base.show when there are keyword arguments --- otherwise
    # the native `show` is perfectly sufficient.
    if define_show === nothing; define_show = !isempty(kwfields) end

    # -------------- end of parsing -------------

    if narrow_types
        given_types = [:($QuickTypes.narrow_typeof($a))
                       for a in (fields..., kwfields...)]
    else
        given_types = type_vars
    end
    inner_constr = quote
        function $type_with_vars($(constr_args...);
                                 $(constr_kwargs...)) where {$(type_params...)}
            $constraints
            return new{$(type_vars...)}($(new_args...))
        end
    end
    straight_constr = :($name($(args...); $(reg_kwargs...)) where {$(type_vars...)} =
                        $name{$(given_types...)}($(o_constr_args...);
                                                 $(o_constr_kwargs...)))
    type_def =
        :(Base.@__doc__ $(Expr(VERSION < v"0.7-" ? :type : :struct,
                               mutable, Expr(:<:, typ, parent_type),
                               Expr(:block, fields..., kwfields...,
                                    inner_constr,
                                    ((parametric &&
                                      all_type_vars_present(type_vars, [args; kwargs]))
                                     ? [straight_constr] : [])...))))
    construct_def =
         :(function $QuickTypes.construct(::Type{$name}, $(arg_names...))
             $name($(o_constr_args...);
                   $(o_constr_kwargs...))
         end)
    esc(Expr(:toplevel,
             type_def,
             construct_def,
             build_show_def(define_show, concise_show, name, fields, kwfields),
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
    return qexpansion(def, false, false, false)
end

""" Quick mutable struct definition. See ?@qstruct """
macro qmutable(def)
    return qexpansion(def, true, false, false)
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
        push!(all_types, :($new_ty <: $parent))
        return new_ty
    end
    #add_type(field::Symbol) = :($field::$(new_type()))
    function add_type(field)
        name, parent_type, slurp, val = splitarg(field)
        @assert !slurp "Slurping not supported. TODO"
        if name in special_kwargs
            return field
        elseif val==nothing
            return :($name::$(new_type(parent_type)))
        else
            return Expr(:kw, :($name::$(new_type(parent_type))), val)
        end
    end
    
    typed_args = map(add_type, args)
    typed_kwargs = map(add_type, kwargs)
    new_typ = :($typ{$(all_types...)})

    if type_counter == 1
        # Has to special-case the "no type parameters" case because of
        # https://github.com/JuliaLang/julia/issues/20878
        return (typ, typ_def, args, kwargs)
    else
        return (new_typ, :($new_typ($(typed_args...); $(typed_kwargs...))),
                typed_args, typed_kwargs)
    end
end

""" Fully-parametric version of `@qstruct`. `@qstruct_fp Foo(a, b=2)` is like
`@qstruct Foo{T, U}(a::T, B::U=2)` """
macro qstruct_fp(def)
    return qexpansion(def, false, true, false)
end
""" Fully-parametric version of `@qmutable`. `@qmutable_fp Foo(a, b=2)` is like
`@qmutable Foo{T, U}(a::T, B::U=2)` """
macro qmutable_fp(def)
    return qexpansion(def, true, true, false)
end


""" Narrowly-parametric version of `@qstruct`. `@qstruct_np Foo(a, b=2)` is like
`@qstruct Foo{T, U}(a::T, B::U=2)`, but it will additionally specialize on types:
`Foo(Int, 2.0) ==> Foo{Type{Int64},Float64}(Int64, 2.0)` """
macro qstruct_np(def)
    return qexpansion(def, false, true, true)
end
macro qmutable_np(def)
    return qexpansion(def, true, true, true)
end

end # module
