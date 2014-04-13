export ElectronVolt, Torr

const ElectronVolt = NonSIUnit{typeof(Joule),:eV}()
convert(::Type{SIQuantity},::typeof(ElectronVolt)) = 1.60217656535e-19Joule

const Torr = NonSIUnit{typeof(Pascal),:torr}()
convert(::Type{SIQuantity},::typeof(Torr)) = 133.322368Pascal
