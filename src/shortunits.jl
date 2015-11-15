# Small (currently handmaintained) list of abbreviations for units in commonly-found orders
# of magnitude. There's a delicate balance to be struck between polluting the namespace
# and being useful. We might in the future give up and just generate all the orders of
# magnitude for everything.
module ShortUnits
    export µg, mg, g, kg, µm, mm, cm, m, km, mJ, J, MJ, HJ, fF, pF, nF, µF, mF,
        F, pΩ, nΩ, µΩ, mΩ, Ω, kΩ, MΩ, mV, V, MV, mHz, Hz, kHz, MHz, GHz, THz, fA, pA,
        nA, µA, mA, A, C, N, mol, ns, ms, µs, s, S, nW, µW, mW, W, MW, GW, eV,
        K, mPa, Pa, kPa, MPa, GPa, nm, torr, mtorr, atm, rad, deg, sr,
        MeV, GeV, TeV

    using SIUnits

    const µg    = Micro * Gram
    const mg    = Milli * Gram
    const g     = Gram
    const kg    = KiloGram

    const nm    = Nano * Meter
    const µm    = Micro * Meter
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
    const µF    = Micro*Farad
    const mF    = Milli*Farad
    const F     = Farad

    const pΩ    = Pico*Ohm
    const nΩ    = Nano*Ohm
    const µΩ    = Micro*Ohm
    const mΩ    = Milli*Ohm
    const Ω     = Ohm
    const kΩ    = Kilo*Ohm
    const MΩ    = Mega*Ohm

    const mV    = Milli*Volt
    const V     = Volt
    const MV    = Mega*Volt

    const ns    = Nano*Second
    const ms    = Milli*Second
    const µs    = Micro*Second
    const s     = Second

    const S     = Siemens

    const mHz   = Milli*Hertz
    const Hz    = Hertz
    const kHz   = Kilo*Hertz
    const MHz   = Mega*Hertz
    const GHz   = Giga*Hertz
    const THz   = Tera*Hertz

    const fA    = Femto*Ampere
    const pA    = Pico*Ampere
    const nA    = Nano*Ampere
    const µA    = Micro*Ampere
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
    const µW    = Micro*Watt
    const mW    = Milli*Watt
    const W     = Watt
    const MW    = Mega*Watt
    const GW    = Giga*Watt

    const mol   = Mole

    const K     = Kelvin

    const eV    = ElectronVolt
    const MeV   = Mega*ElectronVolt
    const GeV   = Giga*ElectronVolt
    const TeV   = Tera*ElectronVolt

    const torr  = Torr
    const mtorr = Milli*Torr

    const atm   = Atmosphere

    const rad   = Radian
    const deg   = Degree
    const sr    = Steradian

    # Support the other unicode codepoint for $\mu$
    # (namely the one we get on the REPL)
    export μg, μm, μF, μA, μW, μs, μΩ
    const μg = µg
    const μm = µm
    const μF = µF
    const μA = µA
    const μW = µW
    const μs = µs
    const μΩ = µΩ

end
