module SIUnits

    immutable SIQuantity{T<:Number,m,kg,s,A,K,mol,cd} <: Number
        val::T
    end

    typealias UnitQuanity{T} SIQuantity{T,0,0,0,0,0,0,0}

    SIQuantity{T<:Number}(x::T) = UnitQuanity{T}(x)

    immutable SIUnit{m,kg,s,A,K,mol,cd} <: Number
    end 

    immutable SIRange{T<:Real,m,kg,s,A,K,mol,cd} <: Ranges{T}
        val::Range{T}
    end


    import Base: length, getindex, next, float64, float

    length(x::SIRange) = length(x.val)
    getindex{T,m,kg,s,A,K,mol,cd}(x::SIRange{T,m,kg,s,A,K,mol,cd},i::Integer) = (show((x.val,i));SIQuantity{T,m,kg,s,A,K,mol,cd}(getindex(x.val,i)))
    next{T,m,kg,s,A,K,mol,cd}(r::SIRange{T,m,kg,s,A,K,mol,cd},  i) = (SIQuantity{T,m,kg,s,A,K,mol,cd}(next(r.val,i)[1]),i+1)


    import Base: +, -, *, /, //, ^, promote_rule, convert, show, ==

    macro uc(op...)
        if length(op) == 0
            esc(:((m,kg,s,A,K,mol,cd)...))
        elseif length(op) == 1
            op = op[1]
            esc(:((($op)(mT,mS),($op)(kgT,kgS),($op)(sT,sS),($op)(AT,AS),($op)(KT,KS),($op)(molT,molS),($op)(cdT,cdS))))
        end
    end

    macro uc2()
        esc(:((mS,kgS,sS,AS,KS,molS,cdS,mT,kgT,sT,AT,KT,molT,cdT)))
    end

    macro uc1()
        esc(:(m,kg,s,A,K,mol,cd))
    end

    macro opqu(op,body)
        esc(quote
            function $(op){T,mS,kgS,sS,AS,KS,molS,cdS,mT,kgT,sT,AT,KT,molT,cdT}(x::SIQuantity{T,mT,kgT,sT,AT,KT,molT,cdT},y::SIUnit{mS,kgS,sS,AS,KS,molS,cdS})
                $body
            end
        end)
    end

    macro opuq(op,body)
        esc(quote
            function $(op){S,mS,kgS,sS,AS,KS,molS,cdS,mT,kgT,sT,AT,KT,molT,cdT}(x::SIUnit{mT,kgT,sT,AT,KT,molT,cdT},y::SIQuantity{S,mS,kgS,sS,AS,KS,molS,cdS})
                $body
            end
        end)
    end

    macro opuu(op,body)
        esc(quote
            function $(op){mS,kgS,sS,AS,KS,molS,cdS,mT,kgT,sT,AT,KT,molT,cdT}(x::SIUnits.SIUnit{mT,kgT,sT,AT,KT,molT,cdT},y::SIUnits.SIUnit{mS,kgS,sS,AS,KS,molS,cdS})
                $body
            end
        end)
    end

    promote_rule{T,S,mS,kgS,sS,AS,KS,molS,cdS,mT,kgT,sT,AT,KT,molT,cdT}(
        A::Type{SIQuantity{T,mT,kgT,sT,AT,KT,molT,cdT}},B::Type{SIQuantity{S,mS,kgS,sS,AS,KS,molS,cdS}}) = SIQuantity{promote_type(T,S)}
    promote_rule{T,mS,kgS,sS,AS,KS,molS,cdS,mT,kgT,sT,AT,KT,molT,cdT}(
        A::Type{SIQuantity{T,mT,kgT,sT,AT,KT,molT,cdT}},B::Type{SIUnit{mS,kgS,sS,AS,KS,molS,cdS}}) = SIQuantity{T}
    promote_rule{S,m,kg,s,A,K,mol,cd}(x::Type{Bool},y::Type{SIQuantity{S,m,kg,s,A,K,mol,cd}}) = SIQuantity{promote_type(Bool,S)}
    promote_rule{m,kg,s,A,K,mol,cd}(x::Type{Bool},y::Type{SIUnit{m,kg,s,A,K,mol,cd}}) = SIQuantity{Bool}
    promote_rule{T,S,m,kg,s,A,K,mol,cd}(x::Type{T},y::Type{SIQuantity{S,m,kg,s,A,K,mol,cd}}) = SIQuantity{promote_type(T,S)}
    promote_rule{T,m,kg,s,A,K,mol,cd}(x::Type{T},y::Type{SIUnit{m,kg,s,A,K,mol,cd}}) = SIQuantity{T}
    promote_rule{T,S<:Number,m,kg,s,A,K,mol,cd}(x::Type{SIQuantity{T,m,kg,s,A,K,mol,cd}},y::Type{S}) = SIQuantity{promote_type(T,S)}

    convert{T,m,kg,s,A,K,mol,cd}(::Type{SIQuantity{T}},x::SIUnit{m,kg,s,A,K,mol,cd}) = SIQuantity{T,m,kg,s,A,K,mol,cd}(one(T))
    convert{T}(::Type{SIQuantity{T}},x::T) = UnitQuanity{T}(x)
    convert{T,S}(::Type{SIQuantity{T}},x::S) = convert(SIQuantity{T},convert(T,x))
    convert{T}(::Type{SIQuantity{T}},x::SIQuantity{T}) = x
    convert{T,S,m,kg,s,A,K,mol,cd}(::Type{SIQuantity{T}},x::SIQuantity{S,m,kg,s,A,K,mol,cd}) = SIQuantity{T,m,kg,s,A,K,mol,cd}(convert(T,x.val))

    to_q{T,m,kg,s,A,K,mol,cd}(::Type{SIQuantity{T,m,kg,s,A,K,mol,cd}},val::T) = (0 == m == kg == s == A == K == mol == cd) ? val : SIQuantity{T,m,kg,s,A,K,mol,cd}(val)
    convert{T,S,m,kg,s,A,K,mol,cd}(::Type{SIQuantity{T,m,kg,s,A,K,mol,cd}},val::S) = (SIQuantity{T,m,kg,s,A,K,mol,cd}(convert(T,val)))
    function convert{T,S,mS,kgS,sS,AS,KS,molS,cdS,mT,kgT,sT,AT,KT,molT,cdT}(::Type{SIQuantity{T,mT,kgT,sT,AT,KT,molT,cdT}},val::SIQuantity{S,mS,kgS,sS,AS,KS,molS,cdS})
        if mS != mT || kgS != kgT || sS != sT || AS != AT || KS != KT || molS != molT || cdS != cdT
            error("Dimension mismatch in convert. Attempted to convert a ($(repr(SIUnit{mS,kgS,sS,AS,KS,molS,cdS}))) to ($(repr(SIUnit{mT,kgT,sT,AT,KT,molT,cdT})))")
        end
        SIQuantity{T,mT,kgT,sT,AT,KT,molT,cdT}(convert(T,val.val))
    end

    function *{T,S,mS,kgS,sS,AS,KS,molS,cdS,mT,kgT,sT,AT,KT,molT,cdT}(
        x::SIQuantity{T,mT,kgT,sT,AT,KT,molT,cdT},y::SIQuantity{S,mS,kgS,sS,AS,KS,molS,cdS}) 
        ret = x.val * y.val
        to_q(SIQuantity{typeof(ret),(@uc +)...},ret)
    end

    for op in (:/,://)

        @eval function ($op){T,m,kg,s,A,K,mol,cd}(x::Number,y::SIQuantity{T,m,kg,s,A,K,mol,cd})
            val = ($op)(x,y.val)
            SIQuantity{typeof(val),-m,-kg,-s,-A,-K,-mol,-cd}(val)
        end

        @eval function ($op){T,S,mS,kgS,sS,AS,KS,molS,cdS,mT,kgT,sT,AT,KT,molT,cdT}(
            x::SIQuantity{T,mT,kgT,sT,AT,KT,molT,cdT},y::SIQuantity{S,mS,kgS,sS,AS,KS,molS,cdS}) 
            ret = $(op)(x.val,y.val)
            to_q(SIQuantity{typeof(ret),(@uc -)...},ret)
        end

        @eval @opuu $op SIUnit{(@uc -)...}()
        @eval @opqu $op to_q(SIQuantity{T,(@uc -)...},x.val)
        @eval @opuq $op to_q(SIQuantity{T,(@uc -)...},($op)(1,y.val))

    end


    function +{T,S,m,kg,s,A,K,mol,cd}(
        x::SIQuantity{T,m,kg,s,A,K,mol,cd},y::SIQuantity{S,m,kg,s,A,K,mol,cd})
        val = x.val+y.val
        SIQuantity{typeof(val),m,kg,s,A,K,mol,cd}(val)
    end

    function -{T,S,m,kg,s,A,K,mol,cd}(
        x::SIQuantity{T,m,kg,s,A,K,mol,cd},y::SIQuantity{S,m,kg,s,A,K,mol,cd}) 
        val = x.val-y.val
        SIQuantity{typeof(val),m,kg,s,A,K,mol,cd}(val)
    end

    function -{T,S,mS,kgS,sS,AS,KS,molS,cdS,mT,kgT,sT,AT,KT,molT,cdT}(
        x::SIQuantity{T,mT,kgT,sT,AT,KT,molT,cdT},y::SIQuantity{S,mS,kgS,sS,AS,KS,molS,cdS}) 
        error("Unit mismatch. Got ($(repr(unit(x)))) - ($(repr(unit(y))))")
    end     

    function +{T,S,mS,kgS,sS,AS,KS,molS,cdS,mT,kgT,sT,AT,KT,molT,cdT}(
        x::SIQuantity{T,mT,kgT,sT,AT,KT,molT,cdT},y::SIQuantity{S,mS,kgS,sS,AS,KS,molS,cdS}) 
        error("Unit mismatch. Got ($(repr(unit(x)))) + ($(repr(unit(y))))")
    end    

    #function -(x::SIQuantity,y::SIQuantity)
    #    error("Unit mismatch. Got ($(repr(unit(x)))) - ($(repr(unit(y))))")
    #end

    function -{T,m,kg,s,A,K,mol,cd}(x::SIQuantity{T,m,kg,s,A,K,mol,cd})
        val = -(x.val)
        SIQuantity{typeof(val),m,kg,s,A,K,mol,cd}(val)
    end

    function ^{T,m,kg,s,A,K,mol,cd}(
        x::SIQuantity{T,m,kg,s,A,K,mol,cd},i::Integer) 
        if i == 0
            return one(T)
        end
        val = x.val^i
        SIQuantity{typeof(val),m*i,kg*i,s*i,A*i,K*i,mol*i,cd*i}(val)
    end


    function ^{T,m,kg,s,A,K,mol,cd}(
        x::SIQuantity{T,m,kg,s,A,K,mol,cd},r::Rational) 
        if r == 0
            return one(T)
        end
        val = x.val^r
        SIQuantity{typeof(val),convert(Int,m*r),convert(Int,kg*r),convert(Int,s*r),convert(Int,A*r),
        convert(Int,K*r),convert(Int,mol*r),convert(Int,cd*r)}(val)
    end

    function =={T,S,mS,kgS,sS,AS,KS,molS,cdS,mT,kgT,sT,AT,KT,molT,cdT}(
        x::SIQuantity{T,mT,kgT,sT,AT,KT,molT,cdT},y::SIQuantity{S,mS,kgS,sS,AS,KS,molS,cdS})
        return (mT == mS && kgT == kgS && sT == sS && AT == AS && molT == molS && cdT == cdS) && (x.val == y.val)
    end

    function =={T,mS,kgS,sS,AS,KS,molS,cdS,mT,kgT,sT,AT,KT,molT,cdT}(
        x::SIQuantity{T,mT,kgT,sT,AT,KT,molT,cdT},y::SIUnit{mS,kgS,sS,AS,KS,molS,cdS})
        return (mT == mS && kgT == kgS && sT == sS && AT == AS && molT == molS && cdT == cdS) && (x.val == one(T))
    end

    function =={S,mS,kgS,sS,AS,KS,molS,cdS,mT,kgT,sT,AT,KT,molT,cdT}(
        x::SIUnit{mT,kgT,sT,AT,KT,molT,cdT},y::SIQuantity{S,mS,kgS,sS,AS,KS,molS,cdS})
        return (mT == mS && kgT == kgS && sT == sS && AT == AS && molT == molS && cdT == cdS) && (one(S) == y.val)
    end

    import Base: sqrt, abs, colon, isless, isfinite

    function colon{T,S,X,m,kg,s,A,K,mol,cd}(start::SIQuantity{T,m,kg,s,A,K,mol,cd},step::SIQuantity{S,m,kg,s,A,K,mol,cd},stop::SIQuantity{X,m,kg,s,A,K,mol,cd})
        val = colon(start.val,step.val,stop.val)
        SIRange{eltype(val),m,kg,s,A,K,mol,cd}(val)
    end

    function sqrt{T,m,kg,s,A,K,mol,cd}(x::SIQuantity{T,m,kg,s,A,K,mol,cd})
        val = sqrt(x.val)
        SIQuantity{typeof(val),convert(Int,m/2),convert(Int,kg/2),convert(Int,s/2),convert(Int,A/2),
        convert(Int,K/2),convert(Int,mol/2),convert(Int,cd/2)}(val)   
    end

    function abs{T,m,kg,s,A,K,mol,cd}(x::SIQuantity{T,m,kg,s,A,K,mol,cd})
        SIQuantity{T,m,kg,s,A,K,mol,cd}(abs(x.val))
    end

    function isfinite{T,m,kg,s,A,K,mol,cd}(x::SIQuantity{T,m,kg,s,A,K,mol,cd})
        isfinite(x.val)
    end

    function isless{T,S,mS,kgS,sS,AS,KS,molS,cdS,mT,kgT,sT,AT,KT,molT,cdT}(
        x::SIQuantity{T,mT,kgT,sT,AT,KT,molT,cdT},y::SIQuantity{S,mS,kgS,sS,AS,KS,molS,cdS}) 
        return isless(x.val,y.val)
    end

    float64(x::SIQuantity) = float64(x.val)
    float(x::SIQuantity) = float(x.val)

    # Arithmetic on SIUnits

    @opuu * SIUnit{(@uc +)...}()
    @opqu * to_q(SIQuantity{T,(@uc +)...},x.val)
    @opuq * to_q(SIQuantity{T,(@uc +)...},y.val)

    function ^{m,kg,s,A,K,mol,cd}(
        x::SIUnit{m,kg,s,A,K,mol,cd},i::Integer) 
        SIUnit{m*i,kg*i,s*i,A*i,K*i,mol*i,cd*i}()
    end

    unit{T,m,kg,s,A,K,mol,cd}(x::SIQuantity{T,m,kg,s,A,K,mol,cd}) = SIUnit{m,kg,s,A,K,mol,cd}()

    export SIPrefix, Meter, KiloGram, Second, Ampere, Kelvin, Mole, Candela, Kilo, Mega, Giga,
        Tera, Centi, Milli, Micro, Nano, Pico, Femto, Gram, Joule, Coulomb, Volt, Farad, Newton,
        Ohm, CentiMeter, Siemens, Hertz

    const SIPrefix = SIUnit{0,0,0,0,0,0,0}()
    const Meter    = SIUnit{1,0,0,0,0,0,0}()
    const KiloGram = SIUnit{0,1,0,0,0,0,0}()
    const Second   = SIUnit{0,0,1,0,0,0,0}()
    const Ampere   = SIUnit{0,0,0,1,0,0,0}()
    const Kelvin   = SIUnit{0,0,0,0,1,0,0}()
    const Mole     = SIUnit{0,0,0,0,0,1,0}()
    const Candela  = SIUnit{0,0,0,0,0,0,1}()

    const Kilo       = 1000SIPrefix
    const Mega       = 10^6SIPrefix
    const Giga       = 10^9SIPrefix
    const Tera      = 10^12SIPrefix
    const Centi      = (1//100)SIPrefix
    const Milli      = (1//1000)SIPrefix
    const Micro      = (1//10^6)SIPrefix
    const Nano       = (1//10^9)SIPrefix
    const Pico       = (1//10^12)SIPrefix
    const Femto      = (1//10^15)SIPrefix

    const Gram       = (1//1000)KiloGram
    const Joule      = KiloGram*Meter^2/Second^2
    const Coulomb    = Ampere*Second
    const Volt       = Joule/Coulomb
    const Farad      = Coulomb^2/Joule
    const Newton     = KiloGram*Meter/Second^2
    const Ohm        = Volt/Ampere
    const Hertz      = 1/Second
    const Siemens    = 1/Ohm

    const CentiMeter = Centi*Meter


# Pretty Printing - Text 
    superscript(i) = map(repr(i)) do c
        c   ==  '-' ? '\u207b' :
        c   ==  '1' ? '\u00b9' :
        c   ==  '2' ? '\u00b2' :
        c   ==  '3' ? '\u00b3' :
        c   ==  '4' ? '\u2074' :
        c   ==  '5' ? '\u2075' :
        c   ==  '6' ? '\u2076' :
        c   ==  '7' ? '\u2077' :
        c   ==  '8' ? '\u2078' :
        c   ==  '9' ? '\u2079' :
        c   ==  '0' ? '\u2070' :
        error("Unexpected Chatacter")
    end

    function show{m,kg,s,A,K,mol,cd}(io::IO,x::SIUnit{m,kg,s,A,K,mol,cd})
        kg != 0 && print(io,"kg",(kg == 1 ? " " :superscript(kg)))
        m != 0 && print(io,"m",(m == 1 ? " " : superscript(m)))
        s != 0 && print(io,"s",(s == 1 ? " " :superscript(s)))
        A != 0 && print(io,"A",(A == 1 ? " " :superscript(A)))
        K != 0 && print(io,"K",(K == 1 ? " " :superscript(K)))
        mol != 0 && print(io,"mol",(mol == 1 ? " " :superscript(mol)))
        cd != 0 && print(io,"cd",(cd == 1 ? " " :uperscript(cd)))
    end

    function show{T,m,kg,s,A,K,mol,cd}(io::IO,x::SIQuantity{T,m,kg,s,A,K,mol,cd})
        show(io,x.val)
        print(io," ")
        show(io,unit(x))
    end

    function sidims{m,kg,s,A,K,mol,cd}(::SIUnit{m,kg,s,A,K,mol,cd})
        (m,kg,s,A,K,mol,cd)
    end

    function sidims{T,m,kg,s,A,K,mol,cd}(::SIQuantity{T,m,kg,s,A,K,mol,cd})
        (m,kg,s,A,K,mol,cd)
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

    function Base.Multimedia.writemime{m,kg,s,A,K,mol,cd}(io::IO,::MIME"text/mathtex+latex",x::SIUnit{m,kg,s,A,K,mol,cd})
        num = ASCIIString[]
        den = ASCIIString[]
        @l kg
        @l m
        @l s
        @l A
        @l K
        @l mol
        @l cd
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

    function Base.Multimedia.writemime{T,m,kg,s,A,K,mol,cd}(io::IO,::MIME"text/mathtex+latex",x::SIQuantity{T,m,kg,s,A,K,mol,cd})
        writemime(io,MIME("text/mathtex+latex"),x.val)
        write(io,"\\;")
        Base.Multimedia.writemime(io,MIME("text/mathtex+latex"),unit(x))
    end

# Non-SI Units
immutable NonSIUnit{BaseUnit<:SIUnit,Unit}; end
immutable NonSIQuantity{T,Unit<:NonSIUnit} <: Number
    val::T
end

unit{T,Unit}(x::NonSIQuantity{T,Unit}) = Unit()

/(x::SIQuantity,y::NonSIUnit) = x/convert(SIQuantity,y)
/(x::NonSIUnit,y::SIQuantity) = convert(SIQuantity,x)/y

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

convert(::Type{SIQuantity},x::NonSIQuantity) = x.val * convert(SIQuantity,x)

export ElectronVolt, as

# Energy Units

const ElectronVolt = NonSIUnit{typeof(Joule),:eV}()
convert(::Type{SIQuantity},::typeof(ElectronVolt)) = 1.60217656535e-19Joule

function as{U<:NonSIUnit}(x::SIQuantity,y::U)
    val = x/y
    @assert !(typeof(val)<:SIQuantity)
    NonSIQuantity{typeof(val),U}(val)
end

include("shortunits.jl")

end # module
