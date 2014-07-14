# Small (currently handmaintained) list of abbreviations for units in commonly found order 
# of magnitude. There's a delicate balance to be struck between polluting the namespace
# and being useful. We might in ther future give up and just generate all the orders of
# maginuted for everything
module ShortUnits
    export μg, mg, g, kg, μm, mm, cm, m, km, mJ, J, MJ, HJ, fF, pF, nF, μF, mF,
        F, pΩ, nΩ, μΩ, mΩ, Ω, kΩ, MΩ, mV, V, MV, mHz, Hz, MHz, GHz, THz, fA, pA,
        nA, μA, mA, A, C, N, mol, ns, ms, μs, s, S, nW, μW, mW, W, MW, GW, eV,
        K, mPa, Pa, kPa, MPa, GPa, nm, torr, mtorr

    using SIUnits

    const μg    = Micro * Gram
    const mg    = Milli * Gram
    const g     = Gram
    const kg    = KiloGram

    const nm    = Nano * Meter
    const μm    = Micro * Meter
    const mm    = Milli * Meter
    const cm    = CentiMeter
    const m     = Meter
    const km    = Kilo*Meter

    const mJ    = Milli*Joule
    const J     = Joule
    const MJ    = Mega*Joule
    const GJ    = Giga*Joule

    const fF    = Femto*Farad
    const pF    = Pico*Farad
    const nF    = Nano*Farad
    const μF    = Micro*Farad
    const mF    = Milli*Farad
    const F     = Farad

    const pΩ    = Pico*Ohm
    const nΩ    = Nano*Ohm
    const μΩ    = Micro*Ohm
    const mΩ    = Milli*Ohm
    const Ω     = Ohm
    const kΩ    = Kilo*Ohm
    const MΩ    = Mega*Ohm   

    const mV    = Milli*Volt
    const V     = Volt 
    const MV    = Mega*Volt

    const ns    = Nano*Second
    const ms    = Milli*Second
    const μs    = Micro*Second
    const s     = Second

    const S     = Siemens

    const mHz   = Milli*Hertz
    const Hz    = Hertz
    const MHz   = Mega*Hertz
    const GHz   = Giga*Hertz
    const THz   = Tera*Hertz

    const fA    = Femto*Ampere
    const pA    = Pico*Ampere
    const nA    = Nano*Ampere
    const μA    = Micro*Ampere
    const mA    = Milli*Ampere
    const A     = Ampere

    const mPa   = Milli*Pascal
    const Pa    = Pascal
    const kPa   = Kilo*Pascal
    const MPa   = Mega*Pascal
    const GPa   = Giga*Pascal

    const C     = Coulomb

    const N     = Newton

    const nW    = Nano*Watt
    const μW    = Micro*Watt
    const mW    = Milli*Watt
    const W     = Watt
    const MW    = Mega*Watt
    const GW    = Giga*Watt

    const mol   = Mole

    const K     = Kelvin

    const eV    = ElectronVolt

    const torr  = Torr
    const mtorr = Milli*Torr
end
