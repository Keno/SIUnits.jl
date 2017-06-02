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

if VERSION <= v"0.4.0-dev+3338"
    const AssertionError = ErrorException
end

# Test types

for x in (1,1.0f0,1.0,complex(1.0,0.0))
    @test typeof(x*Meter) == quantity(typeof(x),Meter)
    @test SIUnits.unit(x*Meter) == Meter
end

# Basic arithmetic things
@test V*5 == 5*V
@test -(1V) == (-1)V
@test 1V + 2V == 3V
@test (1//2)V - 1V == (-1//2)V
@test_throws_compat ErrorException 1//2V - 1V
@test_throws_compat ErrorException 1V + 2s + 2kg
@test 1V + zero(1V) == 1V
@test 1V + zero(typeof(1V)) == 1V
@test 1V * one(1V) == 1V
@test 1V * one(typeof(1V)) == 1V
@test 3V / 3V == one(3)
@test 3.0V / 3.0V === one(3.)
@test isa(3.0V / 3.0V, typeof(one(3.)))
@test isa(one(3.0V), typeof(one(3.)))

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

@test (1.23V)^0 == one(1.23)
@test (1.23V)^(0//1) == one(1.23)
@test (1.23V)^(0s/(1s)) == one(1.23)

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
@test_throws_compat AssertionError as(1torr,s)

# Ranges (#4)
r1 = 1Hz:5Hz
@test length(r1) == 5

@test collect(1Hz:5Hz) == collect(1:5)Hz # Tests the iteration protocol
@test r1[1] == 1Hz # Test indexing

@test r1[2:4] == 2Hz:4Hz # Indexing ranges by ranges should be SIRanges

# Ranges: Ensure forwarded methods are working appropriately
@test eltype(r1) == typeof(1Hz)
@test first(r1) == step(r1) == 1Hz
@test last(r1) == 5Hz
@test r1 - 1Hz == 0Hz:4Hz
@test 1Hz - r1 == 0Hz:-1Hz:-4Hz
@test r1 + 1Hz == 1Hz + r1 == 2Hz:6Hz
@test r1 * 2s == 2s * r1 == 2:2:10
@test r1 * 1m == 1m * r1 == 1Hz*m:5Hz*m
@test r1 / 2Hz == 0.5:0.5:2.5

# Others

@test isnan(1.23K) == false
@test isnan(NaN*K) == true
@test isreal(1.23K) == true
@test isreal(1.23im*K) == false
@test real(1.0K + 2.0im*K) == 1.0K
@test imag(1.0K + 2.0im*K) == 2.0K
@test conj(1.0K + 2.0im*K) == 1.0K - 2.0im*K

@test mod(2µm,4µm) == 2µm
@test_throws_compat ErrorException mod(3kg,4s)

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

# Irrationals
@test (3pi)*m == 3*(pi*m)
a = 1m
b = 2mm
@test (pi*a^2)/b == (pi/2)*(m^2/mm)

# Comparisons with numbers and different units
a = SIUnits.UnitQuantity{Float64}(3.0)
@test a < 4
@test 2.8 < a
@test_throws_compat MethodError 1m < 2kg

# Issue #27
@test ([1s]/(1s))[1] == 1
@test ([1s]/(1.0s))[1] == 1
@test ([1.0s]/(1s))[1] == 1
@test ([1.0s]/(1.0s))[1] == 1

@test ([1s]*(1s))[1] == 1s*s
@test ([1s]*(1.0s))[1] == 1s*s
@test ([1.0s]*(1s))[1] == 1s*s
@test ([1.0s]*(1.0s))[1] == 1s*s

# Issue #49
@test m != kg != s != A != K != mol != rad != sr

# Issue #52
@test show(IOBuffer(), 1*Meter) == nothing

# Test angular units
@test 1rad + 2rad == 3rad
@test 1sr  + 2sr  == 3sr
for func in (sin,cos,tan,cot,sec,csc)
    @test func(1.23rad) == func(1.23)
    @test_approx_eq func(1.23deg) func(as(1.23deg,rad))
end

# Absolute value
@test abs(-1Volt) == 1Volt
@test abs2(-2Volt) == 4(Volt^2)
