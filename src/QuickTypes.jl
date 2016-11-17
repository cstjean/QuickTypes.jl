__precompile__()

module QuickTypes

using MacroTools: @capture

export @qtype, @qimmutable

# These are not exported for now, because they are rather specific extensions.
""" For a type X defined with `@qtype/@qimmutable` and with fields `a, b, c,
...`, `QuickTypes.construct(X, 1, 2, 3...)` is a purely-positional constructor
of `X`. This is useful for writing generic structure traversal. For any object
of a type defined by `@qimmutable`, this holds:

    QuickTypes.construct(QuickTypes.roottypeof(o), QuickTypes.fieldsof(o)...) == o
 """
function construct end


""" `roottypeof(obj)` returns the type of obj with generic parametric types. Eg.
`roottypeof(a::SomeType{Int}) -> SomeType{T}`. See `QuickTypes.construct` """
@generated roottypeof(obj) = obj.name.primary


""" `fieldsof(obj)` returns the fields of `obj` in a tuple.
See also `QuickTypes.construct` """
@generated fieldsof(obj) = :(tuple($([:(obj.$field)
                                      for field in fieldnames(obj)]...)))


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

# Helper for @qtype/@qimmutable
function qexpansion(def, mutable)
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
    get_type_var(v::Symbol) = v
    get_type_var(e::Expr) = e.args[1]
    if @capture(typ, name_{type_params__})
        parametric = true
        type_vars = map(get_type_var, type_params)
    else
        parametric = false
        name = typ
    end
    function get_sym(e::Expr) 
        @assert e.head==:(::)
        e.args[1]
    end
    get_sym(e::Symbol) = e
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
    define_show = true
    for kwarg in kwargs  # keyword arguments
        fsym = get_sym(kwarg.args[1])::Symbol
        if fsym == :_define_show
            define_show = kwarg.args[2]::Bool
            continue
        end
        push!(kwfields, kwarg.args[1])
        push!(constr_kwargs, Expr(:kw, fsym, kwarg.args[2]))
        push!(new_args, fsym)
        push!(o_constr_kwargs, Expr(:kw, fsym, fsym))
    end
    inner_constr = quote
        function $name($(constr_args...); $(constr_kwargs...))
            $constraints
            return new($(new_args...))
        end
    end
    outer_constr = (parametric ?
                    (length(o_constr_kwargs) > 0 ?
                     :($typ_def =
                       $name{$(type_vars...)}($(o_constr_args...);
                                              $(o_constr_kwargs...))) :
                     # Special-casing necessary because of julialang#18845
                     :($typ_def =
                       $name{$(type_vars...)}($(o_constr_args...)))) :
                    nothing)
    # The Base.show function for that type
    if define_show
        show_expr = :(function Base.show(io::IO, obj::$name)
            print(io, typeof(obj))
            write(io, "(")
            # Positional args
            $([:(show(io, obj.$(get_sym(field)));
                 # Print comma. I would prefer printing ", " but that's not
                 # what Julia 0.5 does.
                 $(field==last(fields) ? nothing : :(write(io, ","))))
               for field in fields]...)
            # separating semicolon
            $(if !isempty(kwfields)
                :(write(io, ";")) end)
            # Keyword args
            $([:(write(io, $(string(get_sym(kwfield)))); write(io, "=");
                 show(io, obj.$(get_sym(kwfield)));
                 $(kwfield==last(kwfields) ? nothing : :(write(io, ","))))
               for kwfield in kwfields]...)
            write(io, ")")
        end)
    else
        show_expr = nothing
    end
    type_def =
        :(Base.@__doc__ $(Expr(:type, mutable, Expr(:<:, typ, parent_type),
                               Expr(:block, fields..., kwfields...,
                                    inner_constr))))
    construct_sign = :($QuickTypes.construct(::Type{$name}, $(new_args...)))
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
             show_expr,
             nothing))
end


""" Quick type definition. 

```julia
@qtype Car(size, nwheels::Int=4; brand::String="unnamed") <: Vehicle
```

expands into

```julia
type Car <: Vehicle
    size
    nwheels::Int
    brand::String
    Car(size, nwheels=4; brand="unnamed") = new(size, nwheels, brand)
end
```

Also supports parametric types: `@qtype Door{T}(size::T)`.

Note: `@qtype` automatically defines a `Base.show` method for the new type,
unless `_define_show=false` (eg. `@qtype(x, y; _define_show=false)`).
"""
macro qtype(def)
    return qexpansion(def, true)
end



""" Quick immutable definition. See ?qtype """
macro qimmutable(def)
    return qexpansion(def, false)
end



end # module
