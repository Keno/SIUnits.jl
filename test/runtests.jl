using SIUnits
using SIUnits.ShortUnits
using Base.Test

# Basic arithmetic things
@test 1V + 2V == 3V
@test (1//2)V - 1V == (-1//2)V
@test_throws 1//2V - 1V
@test_throws 1V + 2s + 2kg

OneNewton = 1*(kg*m/s^2)
@test OneNewton*(1s)^2 == 1kg*m
@test OneNewton*s^2 == 1*kg*m

@test OneNewton/(kg*m) == 1Hertz^2

@test OneNewton^2 == 1kg^2*m^2/s^4

@test s == sqrt(1s^2)
@test sqrt(2)*m == sqrt(2m^2)

@test_throws sqrt(1s)

@test 1/s == 1Hz

# Issue #2

immutable note{T<:Real}
    pitch::quantity(T,Hz)     #has dimensions of inverse time
    duration::quantity(T,s) #has dimensions of time
    sustained::Bool
end

note{Float64}(1.0Hz,1s,true)

@test_throws immutable foo{T}
    bar::quantity(T,2s)
end

# Ranges (#4)
r1 = 1Hz:5Hz
@test length(r1) == 5


@test collect(1Hz:5Hz) == collect(1:5)Hz # Tests the iteration protocol
