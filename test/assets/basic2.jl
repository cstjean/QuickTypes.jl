using QuickTypes

abstract type Vehicle end

@qstruct Car{T<:Number, U}(size::T, nwheels::Int=18; manufacturer::U=nothing,
                           brand::String="off-brand") <: Vehicle

@qstruct_fp Plane(nwheels::Number; brand=:airbus) do
    @assert nwheels < 100
end <: Vehicle
