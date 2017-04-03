__precompile__()

module QuickTypes

using MacroTools: @capture
import Compat # TODO before tag: remove this dep!

export @qmutable, @qstruct  # Julia 0.6
export @qtype, @qimmutable  # Julia 0.5
export @qmutable_fp, @qstruct_fp  # Julia 0.6

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
""" `field_types(typ)` returns the types of the fields of a composite type. """
field_types(typ::Type) = typ.types

""" `fieldsof(obj)` returns the fields of `obj` in a tuple.
See also `QuickTypes.construct` """
@generated fieldsof(obj) = :(tuple($([:(obj.$field)
                                      for field in fieldnames(obj)]...)))

type_simple_name(ty::Type)::Symbol = ty.name.name

################################################################################

""" `parse_funcall(fcall)`

Parses `fname(args; kwargs)`. Returns `(fname, args, kwargs)` """
function parse_funcall(fcall)
    if @capture(fcall, fun_(args__; kwargs__))
        (fun, args, kwargs)
    elseif @capture(fcall, fun_(args__))
        (fun, args, Any[])
    else
        error("Not a funcall: $fcall")
    end
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
            :(write(io, ";")) end)
        # Keyword args
        $([:(write(io, $(string(get_sym(kwfield)))); write(io, "=");
             show(io, obj.$(get_sym(kwfield)));
             $(kwfield==last(kwfields) ? nothing : :(write(io, ", "))))
           for kwfield in kwfields]...)
        write(io, ")")
    end)
end

# Helper for @qmutable/@qstruct
function qexpansion(def, mutable, fully_parametric)
    if !@capture(def, typ_def_ <: parent_type_)
        typ_def = def
        parent_type = :Any
    end
    typ, args, kwargs = parse_funcall(typ_def)
    if !isempty(args) && @capture(args[1], ()->constraints_)
        args = args[2:end]
        typ_def = :($typ($(args...); $(kwargs...)))
    else
        constraints = nothing
    end
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
    new_args = Symbol[]
    for arg in args
        if isa(arg, Expr) && arg.head==:kw # default arguments
            fsym = get_sym(arg.args[1])
            push!(fields, arg.args[1])
            push!(constr_args, Expr(:kw, fsym, arg.args[2]))
            push!(new_args, fsym)
            push!(o_constr_args, fsym)
        else # normal arguments
            push!(fields, arg)
            push!(constr_args, get_sym(arg))
            push!(new_args, get_sym(arg))
            push!(o_constr_args, get_sym(arg))
        end
    end
    # Parse keyword-arguments
    define_show = nothing # see after the loop
    concise_show = false # default
    for kwarg in kwargs  # keyword arguments
        fsym = get_sym(kwarg.args[1])::Symbol
        if fsym == :_define_show
            define_show = kwarg.args[2]::Bool
            continue
        end
        if fsym == :_concise_show
            concise_show = kwarg.args[2]::Bool
            continue
        end
        push!(kwfields, kwarg.args[1])
        push!(constr_kwargs, Expr(:kw, fsym, kwarg.args[2]))
        push!(new_args, fsym)
        push!(o_constr_kwargs, Expr(:kw, fsym, fsym))
    end
    # By default, only define Base.show when there are keyword arguments --- otherwise
    # the native `show` is perfectly sufficient.
    if define_show === nothing define_show = !isempty(kwfields) end

    # -------------- end of parsing -------------

    inner_constr = quote
        function (::Type{$type_with_vars}){$(type_params...)}($(constr_args...);
                                                              $(constr_kwargs...))
            $constraints
            return new{$(type_vars...)}($(new_args...))
        end
    end
    outer_constr = (parametric ?
                    (length(o_constr_kwargs) > 0 ?
                     :($typ_def =
                       $name{$(type_vars...)}($(o_constr_args...);
                                              $(o_constr_kwargs...))) :
                     # Special-casing necessary because of julialang#18845. Fixed in 0.5.1
                     :($typ_def =
                       $name{$(type_vars...)}($(o_constr_args...)))) :
                    nothing)
    type_def =
        :(Base.@__doc__ $(Expr(:type, mutable, Expr(:<:, typ, parent_type),
                               Expr(:block, fields..., kwfields...,
                                    inner_constr))))
    construct_def =
        (length(o_constr_kwargs) > 0 ?
         :(function $QuickTypes.construct(::Type{$name}, $(new_args...))
             $constraints
             $name($(o_constr_args...);
                   $(o_constr_kwargs...))
         end) :
         # Special-casing necessary because of julialang#18845
         # Solved on 0.5.1
         :(function $QuickTypes.construct(::Type{$name}, $(new_args...))
             $constraints
             $name($(o_constr_args...))
         end))
    esc(Expr(:toplevel,
             type_def,
             outer_constr,
             construct_def,
             build_show_def(define_show, concise_show, name, fields, kwfields),
             # Temporary for 0.6 compatibility. DELETEME
             :(similar_object(::$name, fields...) = $name(fields...)),
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
    return qexpansion(def, false, false)
end
macro qimmutable(def)  # 0.5 and below
    return qexpansion(def, false, false)
end

""" Quick mutable struct definition. See ?@qstruct """
macro qmutable(def)
    return qexpansion(def, true, false)
end
macro qtype(def)   # 0.5 and below
    return qexpansion(def, true, false)
end

# -----------------------------------------------------------------------------
# Fully-parametric

""" `macro_keyword_args(kwarg)`

Macro helper: identfies x=y as a keyword argument (for macro or function), and returns
(x, y) """
function macro_keyword_args(kwarg)
    # In 0.6, macro don't have keyword arguments, and treat @foo(x=1) as a positional
    # argument that's an assignment. 
    @assert kwarg.head == :kw || kwarg.head == :(=)
    return (kwarg.args[1], kwarg.args[2])
end


# Helper for qstruct_fp
function make_parametric(typ, typ_def, args, kwargs)
    all_types = []
    type_counter = 1
    function new_type()
        new_ty = Symbol(:T, type_counter)
        type_counter += 1
        push!(all_types, new_ty)
        return new_ty
    end
    add_type(field::Symbol) = :($field::$(new_type()))
    function add_type(field::Expr)
        name, val = macro_keyword_args(field)
        if name in special_kwargs
            return field
        else
            return Expr(:kw, :($name::$(new_type())), val)
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
    return qexpansion(def, false, true)
end
""" Fully-parametric version of `@qmutable`. `@qmutable_fp Foo(a, b=2)` is like
`@qmutable Foo{T, U}(a::T, B::U=2)` """
macro qmutable_fp(def)
    return qexpansion(def, true, true)
end


end # module
