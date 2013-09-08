module SIUnits

    immutable SIQuantity{T<:Number,m,kg,s,A,K,mol,cd} <: Number
        val::T
    end

    import Base: +, -, *, /, ^, promote_rule, convert, show

    typealias UnitQuanity{T} SIQuantity{T,0,0,0,0,0,0,0}

    promote_rule{T,S,mS,kgS,sS,AS,KS,molS,cdS,mT,kgT,sT,AT,KT,molT,cdT}(
        A::Type{SIQuantity{T,mT,kgT,sT,AT,KT,molT,cdT}},B::Type{SIQuantity{S,mS,kgS,sS,AS,KS,molS,cdS}}) = SIQuantity{promote_type(T,S)}
    promote_rule{S,m,kg,s,A,K,mol,cd}(x::Type{Bool},y::Type{SIQuantity{S,m,kg,s,A,K,mol,cd}}) = SIQuantity{promote_type(Bool,S)}
    promote_rule{T,S,m,kg,s,A,K,mol,cd}(x::Type{T},y::Type{SIQuantity{S,m,kg,s,A,K,mol,cd}}) = SIQuantity{promote_type(T,S)}
    promote_rule{T,S,m,kg,s,A,K,mol,cd}(x::Type{SIQuantity{T,m,kg,s,A,K,mol,cd}},y::Type{S}) = SIQuantity{promote_type(T,S)}

    convert{T}(::Type{SIQuantity{T}},x::T) = UnitQuanity{T}(x)
    convert{T,S}(::Type{SIQuantity{T}},x::S) = convert(SIQuantity{T},convert(T,x))
    convert{T}(::Type{SIQuantity{T}},x::SIQuantity{T}) = x
    convert{T,S,m,kg,s,A,K,mol,cd}(::Type{SIQuantity{T}},x::SIQuantity{S,m,kg,s,A,K,mol,cd}) = SIQuantity{T,m,kg,s,A,K,mol,cd}(convert(T,x.val))

    function *{T,S,mS,kgS,sS,AS,KS,molS,cdS,mT,kgT,sT,AT,KT,molT,cdT}(
        x::SIQuantity{T,mT,kgT,sT,AT,KT,molT,cdT},y::SIQuantity{S,mS,kgS,sS,AS,KS,molS,cdS}) 
        if 0 == mT+mS == kgT+kgS == sT+sS == AT+AS == KT+KS == molT+molS == cdT+cdS
            return x.val * y.val
        else
            return SIQuantity{promote_type(T,S),mT+mS,kgT+kgS,sT+sS,AT+AS,KT+KS,molT+molS,cdT+cdS}(x.val*y.val)
        end
    end

    function /{T,S,mS,kgS,sS,AS,KS,molS,cdS,mT,kgT,sT,AT,KT,molT,cdT}(
        x::SIQuantity{T,mT,kgT,sT,AT,KT,molT,cdT},y::SIQuantity{S,mS,kgS,sS,AS,KS,molS,cdS}) 
        if 0 == mT-mS == kgT-kgS == sT-sS == AT-AS == KT-KS == molT-molS == cdT-cdS
            return x.val / y.val
        else
            SIQuantity{promote_type(T,S),mT-mS,kgT-kgS,sT-sS,AT-AS,KT-KS,molT-molS,cdT-cdS}(x.val/y.val)
        end
    end

    function +{T,S,m,kg,s,A,K,mol,cd}(
        x::SIQuantity{T,m,kg,s,A,K,mol,cd},y::SIQuantity{S,m,kg,s,A,K,mol,cd}) 
        SIQuantity{promote_type(T,S),m,kg,s,A,K,mol,cd}(x.val+y.val)
    end

    function -{T,S,m,kg,s,A,K,mol,cd}(
        x::SIQuantity{T,m,kg,s,A,K,mol,cd},y::SIQuantity{S,m,kg,s,A,K,mol,cd}) 
        SIQuantity{promote_type(T,S),m,kg,s,A,K,mol,cd}(x.val-y.val)
    end

    function ^{T,m,kg,s,A,K,mol,cd}(
        x::SIQuantity{T,m,kg,s,A,K,mol,cd},i::Integer) 
        if i == 0
            return one(T)
        end
        val = x.val^i
        SIQuantity{typeof(val),m*i,kg*i,s*i,A*i,K*i,mol*i,cd*i}(val)
    end

    const Meter    = SIQuantity{Int,1,0,0,0,0,0,0}(1)
    const KiloGram = SIQuantity{Int,0,1,0,0,0,0,0}(1)
    const Second   = SIQuantity{Int,0,0,1,0,0,0,0}(1)
    const Ampere   = SIQuantity{Int,0,0,0,1,0,0,0}(1)
    const Kelvin   = SIQuantity{Int,0,0,0,0,1,0,0}(1)
    const Mole     = SIQuantity{Int,0,0,0,0,0,1,0}(1)
    const Candela  = SIQuantity{Int,0,0,0,0,0,0,1}(1)

    const Gram       = (1//1000)KiloGram
    const CentiMeter = (1//100)Meter

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

    function show{T,m,kg,s,A,K,mol,cd}(io::IO,x::SIQuantity{T,m,kg,s,A,K,mol,cd})
        show(io,x.val)
        m != 0 && print(io,"m",superscript(m))
        kg != 0 && print(io,"kg",superscript(kg))
        s != 0 && print(io,"cd",superscript(s))
        A != 0 && print(io,"A",superscript(A))
        K != 0 && print(io,"K",superscript(K))
        mol != 0 && print(io,"mol",superscript(mol))
        cd != 0 && print(io,"cd",uperscript(cd))
    end

    export Meter, KiloGram, Second, Ampere, Kelvin, Mole, Candela, Gram, CentiMeter

end # module
