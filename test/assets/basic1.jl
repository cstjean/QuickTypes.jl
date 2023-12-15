using QuickTypes

abstract type Vehicle end

@qstruct Car{T<:Number, U}(size::T, nwheels::Int=4; manufacturer::U=nothing,
                           brand::String="off-brand") <: Vehicle

@qstruct_fp Plane(nwheels::Number; brand=:zoomba) do
    @assert nwheels < 100
end <: Vehicle
