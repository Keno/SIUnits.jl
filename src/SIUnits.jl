isdefined(Base, :__precompile__) && __precompile__()

module SIUnits

    using Compat

    import Base: ==, +, -, *, /, .+, .-, .*, ./, //, ^
    import Base: promote_rule, promote_type, convert, show, mod

    typealias UnitTuple NTuple{9,Int64}
    const EmptyTuple = (0,0,0,0,0,0,0,0,0)

    #Basic UnitTuple Operations
    +(xs::UnitTuple,ys::UnitTuple) = ([x+y for (x,y) in zip(xs,ys)]...)
    -(xs::UnitTuple,ys::UnitTuple) = ([x-y for (x,y) in zip(xs,ys)]...)
    -(xs::UnitTuple) = ([-x for x in xs]...)
    *(x::Integer,ys::UnitTuple) = ([x*y for y in ys]...)
    *(xs::UnitTuple,y::Integer) = ([x*y for x in xs]...)
    *{I<:Integer}(x::Rational{I},ys::UnitTuple) = ([convert(Int64,x*y) for y in ys]...)
    *{I<:Integer}(xs::UnitTuple,y::Rational{I}) = ([convert(Int64,x*y) for x in xs]...)
    /(xs::UnitTuple,y::Integer) = ([convert(Int64,x/y) for x in xs]...)

    immutable SIQuantity{T<:Number,Tup} <: Number
        val::T
    end

    typealias UnitQuantity{T} SIQuantity{T,EmptyTuple}

    SIQuantity{T<:Number}(x::T) = UnitQuantity{T}(x)

    immutable SIUnit{Tup} <: Number
    end

    if !isdefined(Base, :UnitRange)
        const Range = Ranges # Deprecations introduced early in the 0.3 cycle
        const UnitRange = Range1
    end

    abstract SIRanges{T,Tup} <: Range{SIQuantity{T,Tup}}

    immutable SIRange{R<:Range,T<:Real,Tup} <: SIRanges{T,Tup}
        val::R
    end

    unit{T,Tup}(x::SIRanges{T,Tup}) = SIUnit{Tup}()
    quantity{T,Tup}(x::SIRanges{T,Tup}) = SIQuantity{T,Tup}

    import Base: length, getindex, next, float64, float, int, show, start, step, last, done, first, eltype, one, zero

    one(x::SIQuantity) = one(x.val)
    one{T,Tup}(::Type{SIQuantity{T,Tup}}) = one(T)
    zero(x::SIQuantity) = zero(x.val) * unit(x)
    zero{T,Tup}(::Type{SIQuantity{T,Tup}}) = zero(T) * SIUnit{Tup}()

    # This is all nessecary because SIQuanity{T<:Real} !<: Real
    show(io::IO, x::SIRanges) = (show(io, x.val); show(io,unit(x)))
    function show(io::IO, r::SIRange)
        if step(r) == zero(quantity(r))
            print(io, "SIRange(",first(r),",",step(r),",",length(r),")")
        else
            print(io, first(r),':',step(r),':',last(r))
        end
    end
    show{T<:UnitRange}(io::IO, r::SIRange{T}) = print(io, first(r),':',last(r))
    getindex(r::SIRanges,i::Integer) = (quantity(r)(getindex(r.val,i)))
    getindex{T<:SIRanges}(r::T,i::Range) = T(getindex(r.val,i))
    function next(r::SIRanges, i)
        v, j = next(r.val,i)
        to_q(quantity(r),v), j
    end
    length(r::SIRanges) = length(r.val)
    start(r::SIRanges) = start(r.val)
    done(r::SIRanges,i) = done(r.val,i)
    eltype(r::SIRanges) = quantity(r)

    for func in (:first,:step,:last)
        @eval $(func)(r::SIRanges) = to_q(quantity(r),$(func)(r.val))
    end
    # Forward some linear range transformations to the wrapped range
    rangequantity{R<:Range}(::Type{R},tup::UnitTuple) = SIRange{R,eltype(R),tup}
    for func in (VERSION < v"0.3-" ? (:+, :-) : (:.+, :.-)) # version 0.3 swaps fallbacks
        @eval $(func){T,S,Tup}(x::SIRanges{T,Tup}, y::SIQuantity{S,Tup}) = (val = $(func)(x.val, y.val); SIRange{typeof(val),eltype(val),Tup}(val))
        @eval $(func){T,S,Tup}(x::SIQuantity{T,Tup}, y::SIRanges{S,Tup}) = (val = $(func)(x.val, y.val); SIRange{typeof(val),eltype(val),Tup}(val))
    end
    ./(x::SIRanges, y::SIQuantity) = (val = ./(x.val, y.val); rangequantity(typeof(val),tup(x)-tup(y))(val))
    .*(x::SIRanges, y::SIQuantity) = (val = .*(x.val, y.val); rangequantity(typeof(val),tup(x)+tup(y))(val))
    .*(x::SIQuantity, y::SIRanges) = (val = .*(x.val, y.val); rangequantity(typeof(val),tup(x)+tup(y))(val))
    # Version 0.2 assumes all Ranges have start and len fields in ==, and
    # the fallback in 0.3 needlessly iterates through all values
    ==(r::SIRanges, s::SIRanges) = r.val == s.val && tup(r) == tup(s)
    ==(s::SIRanges, r::Range) = s.val == r && tup(s) == EmptyTuple
    ==(r::Range, s::SIRanges) = r == s.val && tup(s) == EmptyTuple

    tup2u(tup) = SIUnit{tup}
    quantity(T::Type,tup::UnitTuple) = quantity(T,tup2u(tup)())

    export quantity, @quantity

    function quantity{S}(T,quant::SIQuantity{S})
        quant.val == one(S) || error("Quantity value must be unity!")
        quantity(T,unit(quant))
    end
    quantity{Tup}(T::(@compat Union{Type,TypeVar}),unit::SIUnit{Tup}) = SIQuantity{T,Tup}

    tup{Tup}(u::SIUnit{Tup}) = (Tup)
    tup{T,Tup}(u::SIQuantity{T,Tup}) = (Tup)
    tup{T,Tup}(u::SIRanges{T,Tup}) = (Tup)

    macro quantity(expr,unit)
        esc(:(SIUnits.SIQuantity{$expr,SIUnits.tup($unit)...}))
    end

    # Irrationals propagate through units. Fancy!!
    promote_rule{sym,Tup}(x::Type{Irrational{sym}},y::Type{SIUnit{Tup}}) =
        SIQuantity{Irrational{sym},Tup}
    promote_rule{sym,T,Tup}(x::Type{Irrational{sym}},y::Type{SIQuantity{T,Tup}}) =
        SIQuantity{promote_type(Irrational{sym},T)}

    promote_rule{T,S,TupS,TupT}(A::Type{SIQuantity{T,TupT}},B::Type{SIQuantity{S,TupS}}) = SIQuantity{promote_type(T,S)}
    promote_rule{T,TupS,TupT}(A::Type{SIQuantity{T,TupT}},B::Type{SIUnit{TupS}}) = SIQuantity{T}
    promote_rule{S,Tup}(x::Type{Bool},y::Type{SIQuantity{S,Tup}}) = SIQuantity{promote_type(Bool,S)}
    promote_rule{Tup}(x::Type{Bool},y::Type{SIUnit{Tup}}) = SIQuantity{Bool}
    promote_rule{T<:Number,S,Tup}(x::Type{T},y::Type{SIQuantity{S,Tup}}) = SIQuantity{promote_type(T,S)}
    promote_rule{T<:Number,Tup}(x::Type{T},y::Type{SIUnit{Tup}}) = SIQuantity{T}

    # One unspecified, units, one concrete (unspecified occurs as the promotion result from the rules above)
    promote_rule{T,S,Tup}(x::Type{SIQuantity{T}},y::Type{SIQuantity{S,Tup}}) = SIQuantity{promote_type(T,S)}

    # Unlike most other types, the promotion of two identitical SIQuantities is
    # not that type itself. As such, the promote_type behavior itself must be
    # overridden. C.f. https://github.com/Keno/SIUnits.jl/issues/27
    promote_type{T,Tup}(::Type{SIQuantity{T,Tup}}, ::Type{SIQuantity{T,Tup}}) = SIQuantity{T}

    if VERSION >= v"0.4-dev"
        eval(quote
            convert{T}(::Type{SIQuantity{T}},x::Dates.Period) = error("Conversion from Period to SIQuantity not defined")
        end)
    end
    convert{T<:Number,Tup}(::Type{SIQuantity{T}},x::SIUnit{Tup}) = SIQuantity{T,Tup}(one(T))
    convert{T<:Number}(::Type{SIQuantity{T}},x::T) = UnitQuantity{T}(x)
    convert{T<:Number,S<:Number}(::Type{SIQuantity{T}},x::S) = convert(SIQuantity{T},convert(T,x))
    convert{T<:Number}(::Type{SIQuantity{T}},x::SIQuantity{T}) = x
    convert{T<:Number,S,Tup}(::Type{SIQuantity{T}},x::SIQuantity{S,Tup}) = SIQuantity{T,Tup}(convert(T,x.val))

    to_q{T}(::Type{SIQuantity{T,EmptyTuple}},val::T) = val
    to_q{T,Tup}(::Type{SIQuantity{T,Tup}},val::T) = SIQuantity{T,Tup}(val)
    convert{T,Tup}(::Type{SIQuantity{T,Tup}},val::Number) = (SIQuantity{T,Tup}(convert(T,val)))

    function convert{T,S,Tup}(::Type{SIQuantity{T,Tup}},val::SIQuantity{S,Tup})
        SIQuantity{T,Tup}(convert(T,val.val))
    end

    for op in (:/,://)

        @eval function ($op){T}(x::Number,y::SIQuantity{T})
            val = ($op)(x,y.val)
            to_q(quantity(typeof(val),-tup(y)),val)
        end

        @eval function ($op)(x::SIQuantity,y::SIQuantity)
            val = $(op)(x.val,y.val)
            to_q(quantity(typeof(val),tup(x)-tup(y)),val)
        end

        @eval $(op)(x::SIUnit,y::SIUnit) = tup2u(tup(x)-tup(y))()
        @eval $(op){T}(x::SIQuantity{T},y::SIUnit) = to_q(quantity(T,tup(unit(x))-tup(y)),x.val)
        @eval $(op){T}(x::SIUnit,y::SIQuantity{T}) = to_q(quantity(T,tup(x)-tup(unit(y))),($op)(1,y.val))

        @eval $(op)(x::Number,y::SIUnit) = x*tup2u(-tup(y))()
    end

    inv(y::SIUnit) = tup2u(-tup(y))()

    function +{T,S,Tup}(
        x::SIQuantity{T,Tup},y::SIQuantity{S,Tup})
        val = x.val+y.val
        SIQuantity{typeof(val),Tup}(val)
    end

    function -{T,S,Tup}(
        x::SIQuantity{T,Tup},y::SIQuantity{S,Tup})
        val = x.val-y.val
        SIQuantity{typeof(val),Tup}(val)
    end

    function -{T,S,TupS,TupT}(x::SIQuantity{T,TupT},y::SIQuantity{S,TupS})
        error("Unit mismatch. Got ($(repr(unit(x)))) - ($(repr(unit(y))))")
    end

    function +{T,S,TupS,TupT}(
        x::SIQuantity{T,TupT},y::SIQuantity{S,TupS})
        error("Unit mismatch. Got ($(repr(unit(x)))) + ($(repr(unit(y))))")
    end

    function -{T,Tup}(x::SIQuantity{T,Tup})
        val = -(x.val)
        SIQuantity{typeof(val),Tup}(val)
    end

    function ^{T,Tup}(
        x::SIQuantity{T,Tup},i::Integer)
        if i == 0
            return one(T)
        end
        val = x.val^i
        SIQuantity{typeof(val),Tup*i}(val)
    end

    function ^{T,Tup}(
        x::SIQuantity{T,Tup},r::Rational)
        if r == 0
            return one(T)
        end
        val = x.val^r
        SIQuantity{typeof(val),Tup*r}(val)
    end

    ^{T,Tup}(x::SIQuantity{T,Tup},r::AbstractFloat) = x^rationalize(r)

    function ^{T,S,TupS,TupT}(
        x::SIQuantity{T,TupT},y::SIQuantity{S,TupS})
        error("Can not raise a number to a unitful quantity. Got ($(repr(unit(x))))^($(repr(unit(y))))")
    end

    ^{T,S,Tup}(x::SIQuantity{T,Tup},y::SIQuantity{S,EmptyTuple}) = x.val^(y.val)

    ==(x::SIQuantity,y::SIQuantity) = (tup(x) == tup(y)) && (x.val == y.val)
    =={T}(x::SIQuantity{T},y::SIUnit) = (tup(x) == tup(y)) && (x.val == one(T))
    =={T}(x::SIUnit,y::SIQuantity{T}) = (tup(x) == tup(y)) && (one(T) == y.val)
    ==(x::SIUnit,y::SIUnit) = tup(x) == tup(y)

    import Base: sqrt, abs, colon, isless, isfinite, isreal, real, imag, isnan

    function colon{T,S,X,Tup}(start::SIQuantity{T,Tup},step::SIQuantity{S,Tup},stop::SIQuantity{X,Tup})
        val = colon(start.val,step.val,stop.val)
        SIRange{typeof(val),eltype(val),Tup}(val)
    end

    function colon{T,S,Tup}(start::SIQuantity{T,Tup},stop::SIQuantity{S,Tup})
        val = colon(start.val,stop.val)
        SIRange{typeof(val),eltype(val),Tup}(val)
    end

    function sqrt{T,Tup}(x::SIQuantity{T,Tup})
        val = sqrt(x.val)
        SIQuantity{typeof(val),Tup/2}(val)
    end

    function abs{T,Tup}(x::SIQuantity{T,Tup})
        SIQuantity{T,Tup}(abs(x.val))
    end

    function isfinite{T,Tup}(x::SIQuantity{T,Tup})
        isfinite(x.val)
    end

    isnan(x::SIQuantity) = isnan(x.val)
    isreal(x::SIQuantity) = isreal(x.val)
    real(x::SIQuantity) = typeof(x)(real(x.val))
    imag(x::SIQuantity) = typeof(x)(imag(x.val))

    function isless{T}(x::SIQuantity{T,EmptyTuple}, y::SIQuantity{T,EmptyTuple})
        return isless(x.val,y.val)
    end
    function isless{T,S}(x::SIQuantity{T,EmptyTuple}, y::SIQuantity{S,EmptyTuple})
        return isless(x.val,y.val)
    end
    function isless{T}(x::SIQuantity{T,EmptyTuple}, y::Real)
        return isless(x.val,y)
    end
    function isless{T}(x::Real, y::SIQuantity{T,EmptyTuple})
        return isless(x,y.val)
    end
    function isless{T,S,TupT}(x::SIQuantity{T,TupT},y::SIQuantity{S,TupT})
        return isless(x.val,y.val)
    end

    function mod{T,S,TupS,TupT}(
        x::SIQuantity{T,TupT},y::SIQuantity{S,TupS})
        error("Unit mismatch. Got mod($(repr(unit(x))),$(repr(unit(y))))")
    end

    function mod{T,S,Tup}(x::SIQuantity{T,Tup},y::SIQuantity{S,Tup})
        val = mod(x.val,y.val)
        SIQuantity{typeof(val),Tup}(val)
    end

    import Base: sin, cos, tan, cot, sec, csc
    for func in (:sin,:cos,:tan,:cot,:sec,:csc)
        @eval $func{T}(θ::SIQuantity{T,(0,0,0,0,0,0,0,1,0)}) = $func(θ.val)
    end

    # Forwarding methods that do not affect units
    import Base: conj
    conj(x::SIQuantity) = typeof(x)(conj(x.val))

    float64(x::SIQuantity) = float64(x.val)
    float(x::SIQuantity) = float(x.val)
    int(x::SIQuantity) = int(x.val)

    *(x::SIUnit,y::SIUnit) = tup2u(tup(x)+tup(y))()
    *{T}(x::SIUnit,y::SIQuantity{T}) = to_q(quantity(T,tup(y)+tup(x)),y.val)
    *{T}(x::SIQuantity{T},y::SIUnit) = to_q(quantity(T,tup(y)+tup(x)),x.val)
    *(x::Irrational,y::SIUnit) = quantity(typeof(x),y)(x)
    function *(x::SIQuantity,y::SIQuantity)
        ret = x.val * y.val
        to_q(quantity(typeof(ret),tup(x)+tup(y)),ret)
    end


    function ^{Tup}(x::SIUnit{Tup},i::Integer)
        SIUnit{i*Tup}()
    end

    unit{T,Tup}(x::SIQuantity{T,Tup}) = SIUnit{Tup}()

    export SIPrefix, Meter, KiloGram, Second, Ampere, Kelvin, Mole, Candela, Radian, Steradian, Kilo, Mega, Giga,
        Tera, Peta, Exa, Zetta, Centi, Milli, Micro, Nano, Pico, Femto, Atto, Zepto, Yocto,
        Gram, Joule, Coulomb, Volt, Farad, Newton, Ohm, CentiMeter, Siemens, Hertz, Watt, Pascal

    const UnitNames = (:kg,:m,:s,:A,:K,:mol,:can,:rad,:sr)
    const SIPrefix  = SIUnit{(0,0,0,0,0,0,0,0,0)}()
    const Meter     = SIUnit{(1,0,0,0,0,0,0,0,0)}()
    const KiloGram  = SIUnit{(0,1,0,0,0,0,0,0,0)}()
    const Second    = SIUnit{(0,0,1,0,0,0,0,0,0)}()
    const Ampere    = SIUnit{(0,0,0,1,0,0,0,0,0)}()
    const Kelvin    = SIUnit{(0,0,0,0,1,0,0,0,0)}()
    const Mole      = SIUnit{(0,0,0,0,0,1,0,0,0)}()
    const Candela   = SIUnit{(0,0,0,0,0,0,1,0,0)}()
    const Radian    = SIUnit{(0,0,0,0,0,0,0,1,0)}()
    const Steradian = SIUnit{(0,0,0,0,0,0,0,0,1)}()

    const Kilo       = (1000)SIPrefix
    const Mega       = (10^6)SIPrefix
    const Giga       = (10^9)SIPrefix
    const Tera       = (10^12)SIPrefix
    const Peta       = (10^15)SIPrefix
    const Exa        = (10^18)SIPrefix
    const Zetta      = (10^21)SIPrefix
    const Yotta      = (10^24)SIPrefix
    const Centi      = (1//100)SIPrefix
    const Milli      = (1//1000)SIPrefix
    const Micro      = (1//10^6)SIPrefix
    const Nano       = (1//10^9)SIPrefix
    const Pico       = (1//10^12)SIPrefix
    const Femto      = (1//10^15)SIPrefix
    const Atto       = (1//10^18)SIPrefix
    const Zepto      = (1//10^21)SIPrefix
    const Yocto      = (1//10^24)SIPrefix

    const Gram       = (1//1000)KiloGram
    const Joule      = KiloGram*Meter^2/Second^2
    const Coulomb    = Ampere*Second
    const Volt       = Joule/Coulomb
    const Farad      = Coulomb^2/Joule
    const Newton     = KiloGram*Meter/Second^2
    const Ohm        = Volt/Ampere
    const Hertz      = inv(Second)
    const Siemens    = inv(Ohm)
    const Watt       = Joule/Second
    const Pascal     = Newton/Meter^2

    const CentiMeter = Centi*Meter


    # Pretty Printing - Text
    typealias NameValuePair Tuple{Symbol,Int64}
    char_superscript(::Type{Val{'-'}}) = '\u207b'
    char_superscript(::Type{Val{'1'}}) = '\u00b9'
    char_superscript(::Type{Val{'2'}}) = '\u00b2'
    char_superscript(::Type{Val{'3'}}) = '\u00b3'
    char_superscript(::Type{Val{'4'}}) = '\u2074'
    char_superscript(::Type{Val{'5'}}) = '\u2075'
    char_superscript(::Type{Val{'6'}}) = '\u2076'
    char_superscript(::Type{Val{'7'}}) = '\u2077'
    char_superscript(::Type{Val{'8'}}) = '\u2078'
    char_superscript(::Type{Val{'9'}}) = '\u2079'
    char_superscript(::Type{Val{'0'}}) = '\u2070'
    superscript(x::Int64) = map(repr(x)) do c
        char_superscript(Val{c})
    end

    is_nonzero_value(p::NameValuePair) = p[2] != 0
    function show(io::IO,p::NameValuePair)
        print(io,p[1])
        if p[2] != 1
            print(io,superscript(p[2]))
        end
    end

    function show{Tup}(io::IO,::SIUnit{Tup})
        filtered_pairs = filter(is_nonzero_value,zip(UnitNames,Tup))
        print(io,join(map(string,filtered_pairs),""))
    end

    function show{T,Tup}(io::IO,x::SIQuantity{T,Tup})
        print(io,x.val," ",unit(x))
    end

    function sidims{Tup}(::SIUnit{Tup})
        (Tup)
    end

    function sidims{T,Tup}(::SIQuantity{T,Tup})
        (Tup)
    end

    export @prettyshow

    macro prettyshow(unit,string)
        esc(quote function Base.show(io::IO,::SIUnits.SIUnit{SIUnits.sidims($(unit))...})
            print(io,$(string))
        end
        function Base.Multimedia.writemime(io::IO,::MIME"text/mathtex+latex",::SIUnits.SIUnit{SIUnits.sidims($(unit))...})
            Base.Multimedia.writemime(io,MIME("text/mathtex+latex"),$(string))
        end
        end)
    end

# Pretty Printing - LaTeX

    using TexExtensions

    import Base: writemime

    macro l(x)
        esc(quote
            $x != 0 && push!($x>0?num:den,string("\\text{",$(string(x)),"\}",abs($x) == 1 ? " " : string("^{",abs($x),"}")))
        end)
    end

    function Base.Multimedia.writemime{Tup}(io::IO,::MIME"text/mathtex+latex",x::SIUnit{Tup})
        num = ASCIIString[]
        den = ASCIIString[]
        @l kg
        @l m
        @l s
        @l A
        @l K
        @l mol
        @l cd
        @l rad
        @l sr
        if !isempty(den)
            if isempty(num)
                write(io,"\\frac{1}{",join(den,"\\;"),"}")
            else
                write(io,"\\frac{",join(num,"\\;"),"}{",join(den,"\\;"),"}")
            end
        else
            write(io,join(num,"\\;"))
        end
    end

    function Base.Multimedia.writemime{T,Tup}(io::IO,::MIME"text/mathtex+latex",x::SIQuantity{T,Tup})
        writemime(io,MIME("text/mathtex+latex"),x.val)
        write(io,"\\;")
        Base.Multimedia.writemime(io,MIME("text/mathtex+latex"),unit(x))
    end

# Non-SI Units
immutable NonSIUnit{BaseUnit<:SIUnit,Unit}; end
immutable NonSIQuantity{T<:Number,Unit<:NonSIUnit} <: Number
    val::T
end

# Non-SI promote rules
promote_rule(x::Type{Irrational},y::Type{NonSIUnit}) =
    NonSIQuantity{x,y}
promote_rule{sym,T<:Number,Unit}(x::Type{Irrational{sym}},y::Type{NonSIQuantity{T,Unit}}) =
    NonSIQuantity{promote_type(Irrational{sym},T),Unit}

promote_rule{T<:Number,S<:Number,U1,U2}(
    A::Type{NonSIQuantity{T,U1}},B::Type{SIQuantity{S,U2}}) = NonSIQuantity{promote_type(T,S)}
promote_rule{T<:Number,U1}(
    A::Type{NonSIQuantity{T,U1}},U2::Type{NonSIUnit}) = NonSIQuantity{T}
promote_rule{S<:Number,U}(x::Type{Bool},y::Type{NonSIQuantity{S,U}}) = NonSIQuantity{promote_type(Bool,S),U}
promote_rule(x::Type{Bool},U::Type{NonSIUnit}) = NonSIQuantity{Bool,U}
promote_rule{T<:Number,S<:Number,U}(x::Type{T},y::Type{NonSIQuantity{S,U}}) = NonSIQuantity{promote_type(T,S),U}
promote_rule{T<:Number}(x::Type{T},U::Type{NonSIUnit}) = NonSIQuantity{T,U}

# Interaction between SI and non-SI quantities
promote_rule{S<:Number,T<:Number,U,Tup}(x::Type{NonSIQuantity{S,U}},y::Type{SIQuantity{T,Tup}}) =
    SIQuantity{promote_type(S,T)}
promote_rule{S<:Number,T<:Number,U,Tup}(x::Type{SIQuantity{T,Tup}},y::Type{NonSIQuantity{S,U}}) =
    SIQuantity{promote_type(S,T)}

siquantity{B}(T,U::NonSIUnit{B}) = quantity(T,B())
siquantity{B}(T,U::Type{NonSIUnit{B}}) = quantity(T,B())
#convert{T,S,U}(::Type{SIQuantity{T}},x::NonSIQuantity{S,U}) = (siquantity(promote_type(T,S),U())(x.val))


*{T<:NonSIUnit}(x::Number,t::T) = NonSIQuantity{typeof(x),T}(x)

baseunit{BaseUnit}(x::NonSIUnit{BaseUnit}) = BaseUnit()
baseunit{T,Unit}(x::NonSIQuantity{T,Unit}) = baseunit(unit(x))
unit{T,Unit}(x::NonSIQuantity{T,Unit}) = Unit()
quantity(T::(@compat Union{Type,TypeVar}),x::NonSIUnit) = NonSIQuantity{T,typeof(x)}
quantity(T::(@compat Union{Type,TypeVar}),U::Type{NonSIUnit}) = NonSIQuantity{T,U}

/{T,U}(x::NonSIQuantity{T,U},y::SIQuantity) = convert(SIQuantity,x)/y
/(x::NonSIUnit,y::SIUnit) = convert(SIQuantity,x)/y
/(x::SIUnit,y::NonSIUnit) = x/convert(SIQuantity,y)

/(x::SIQuantity,y::NonSIUnit) = x/convert(SIQuantity,y)
/(x::NonSIUnit,y::SIQuantity) = convert(SIQuantity,x)/y
-{T,U}(x::NonSIQuantity{T,U}) = NonSIQuantity{T,U}(-x.val)

^(x::(@compat Union{NonSIQuantity,NonSIUnit}),i::Integer) = convert(SIQuantity,x)^i

show{BaseUnit,Unit}(io::IO,x::NonSIUnit{BaseUnit,Unit}) = write(io,string(Unit))
function show(io::IO,x::NonSIQuantity)
    show(io,x.val)
    print(io," ")
    show(io,unit(x))
end

function Base.Multimedia.writemime{BaseUnit,Unit}(io::IO,::MIME"text/mathtex+latex",x::NonSIUnit{BaseUnit,Unit})
    write(io,"\\text{",string(Unit),"}")
end

function Base.Multimedia.writemime(io::IO,::MIME"text/mathtex+latex",x::NonSIQuantity)
    writemime(io,MIME("text/mathtex+latex"),x.val)
    write(io,"\\;")
    Base.Multimedia.writemime(io,MIME("text/mathtex+latex"),unit(x))
end

# No, these two are not the same. Sometimes we get SIQuantities that are not specialized
# on the type out of promotion, hence the first method, but we still need the second method
# to be more specific that the convert methods of plain SIUnits above.
convert(::Type{SIQuantity},x::NonSIQuantity) = x.val * convert(SIQuantity,unit(x))
convert{T<:Number}(::Type{SIQuantity{T}},x::NonSIQuantity) = x.val * convert(SIQuantity,unit(x))

convert{T}(::Type{NonSIQuantity{T}},U::NonSIUnit) = NonSIQuantity{T,U}(one(T))
convert{T<:Number,U}(::Type{NonSIQuantity{T,U}},x::T) = UnitQuantity{T}(x)
#convert{T}(::Type{NonSIQuantity{T}},x::T) = UnitQuantity{T}(x)
#convert{T,S}(::Type{NonSIQuantity{T}},x::S) = convert(NonSIQuantity{T},convert(T,x))
if VERSION >= v"0.4-dev"
    eval(quote
        convert{T,U}(::Type{NonSIQuantity{T,U}},x::Dates.Period) = error("Conversion from Period to NonSIQuantity not defined")
    end)
end
convert{T,U}(::Type{NonSIQuantity{T,U}},x::Number) = convert(NonSIQuantity{T,U},convert(T,x))
convert{T,U}(::Type{NonSIQuantity{T,U}},x::NonSIQuantity{T,U}) = x
convert{T<:Number,S,U1,U2}(::Type{NonSIQuantity{T,U1}},x::NonSIQuantity{S,U2}) = NonSIQuantity{T,U2}(convert(T,x.val))

export as

function as{U<:NonSIUnit}(x::SIQuantity,y::U)
    val = x/y
    @assert !(typeof(val)<:SIQuantity)
    NonSIQuantity{typeof(val),U}(val)
end

function as{U<:NonSIUnit,Q<:SIQuantity}(X::AbstractArray{Q},y::U)
    val = [x/y for x in X]
    @assert !(typeof(eltype(val))<:SIQuantity)
    NonSIQuantity{typeof(val),U}(val)
end

function as(x::NonSIQuantity,y::SIUnit)
    @assert baseunit(x) == y
    convert(SIQuantity,x)
end

as(x::NonSIQuantity,y::SIQuantity) = as(x,unit(y))

include("nonsiunits.jl")
include("shortunits.jl")

end # module
