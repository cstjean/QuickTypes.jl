using QuickTypes
using Base.Test

abstract Vehicle

@qtype Car{T<:Number, U}(size::T, nwheels::Int=4; manufacturer::U=nothing,
                         brand::String="off-brand") <: Vehicle

c = Car(10; manufacturer=("Danone", "Hershey"))

@test c.size==10
@test c.nwheels==4
@test c.manufacturer==("Danone", "Hershey")
@test c.brand=="off-brand"
# Check that the fields are in the right order
@test fieldnames(c) == [:size, :nwheels, :manufacturer, :brand]

################################################################################

@qimmutable Empty()
Empty()

################################################################################

@qimmutable Kwaroo(x; y=10)
@test Kwaroo(5) == Kwaroo(5; y=10)
