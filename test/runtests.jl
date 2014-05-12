using SIUnits
using SIUnits.ShortUnits
using Base.Test

if VERSION <= v"0.3-"
    macro test_throws_compat(a,b)
        esc(:(@test_throws $b))
    end
else
    macro test_throws_compat(a,b)
        esc(:(@test_throws $a $b))
    end
end


# Basic arithmetic things
@test 1V + 2V == 3V
@test (1//2)V - 1V == (-1//2)V
@test_throws_compat ErrorException 1//2V - 1V
@test_throws_compat ErrorException 1V + 2s + 2kg

OneNewton = 1*(kg*m/s^2)
@test OneNewton*(1s)^2 == 1kg*m
@test OneNewton*s^2 == 1*kg*m

@test OneNewton/(kg*m) == 1Hertz^2

@test OneNewton^2 == 1kg^2*m^2/s^4

@test s == sqrt(1s^2)
@test sqrt(2)*m == sqrt(2m^2)

@test_throws_compat InexactError sqrt(1s)

@test 1/s == 1Hz

@test 1Hz*1s == 1
@test 1s/(1s) == 1

# Issue #2

immutable note{T<:Real}
    pitch::quantity(T,Hz)     #has dimensions of inverse time
    duration::quantity(T,s) #has dimensions of time
    sustained::Bool
end

note{Float64}(1.0Hz,1.0s,true)

@test_throws_compat ErrorException immutable foo{T}
    bar::quantity(T,2s)
end

# Non-SI Units
@test_approx_eq_eps 3mtorr 3*SIUnits.Milli*as(1torr,Pascal) eps()*Pa

# Unitful Powers
@test_throws_compat ErrorException 5.0^(3Pa)
@test_throws_compat ErrorException 5.0^(3torr)
@test 1^((5torr)/3Pa) == 1.0

# Interaction of si and non-si units
@test (1mm)^2*1torr == as(1torr,Pascal)*(1mm)^2
@test_throws_compat ErrorException as(1torr,s)

# Ranges (#4)
r1 = 1Hz:5Hz
@test length(r1) == 5

@test collect(1Hz:5Hz) == collect(1:5)Hz # Tests the iteration protocol

# Others

@test mod(2µm,4µm) == 2µm

# Issue #9
a = [1m 2N]
b = [1m 2N 3V]
@test a[1:2] == b[1:2]

# Issue #10
a = 1m
b = 2m
@test sqrt(a*b) == (a*b)^(1/2) == (a*b)^(1//2) == sqrt(2)*m

# Issue #11
au=[1m 2m 3m]
bu=[2N 3N 4N]
@test au*bu' == bu*au'
@test (au*bu')[1] == dot(vec(au),vec(bu))

# MathConsts
@test (3pi)*m == 3*(pi*m)
a = 1m
b = 2mm
@test (pi*a^2)/b == (pi/2)*(m^2/mm)

# Comparisons with numbers and different units
a = SIUnits.UnitQuantity{Float64}(3.0)
@test a < 4
@test 2.8 < a
@test_throws_compat MethodError 1m < 2kg
