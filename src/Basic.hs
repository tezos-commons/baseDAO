module Basic
  ( Parameter(..)
  , emptyStorage
  , measureBoaConstrictor
  ) where

import Lorentz

emptyStorage :: Natural
emptyStorage = 0

data MeasurementMethod
  = ParrotStep
  | MonkeyStep
  | ElephantStep

$(customGeneric "MeasurementMethod" rightBalanced)

deriving anyclass instance IsoValue MeasurementMethod
deriving anyclass instance HasAnnotation MeasurementMethod

instance TypeHasDoc MeasurementMethod where
  typeDocName _ = "MeasurmentMethod"
  typeDocMdDescription =
    "This type defines the way of measuring boa length. \
    \Single boa constrictor corresponds to 38 parrot steps, 31 monkey step \
    \and 9 elephant steps."

data Parameter
  = MeasureBoaConstrictor ("method" :! MeasurementMethod)
  | Zero ()
  deriving stock Generic
  deriving anyclass IsoValue

instance ParameterHasEntrypoints Parameter where
  type ParameterEntrypointsDerivation Parameter = EpdPlain

type Storage = Natural

measureBoaConstrictor :: Contract Parameter Storage
measureBoaConstrictor = defaultContract $ contractName "Boa constrictor measurement" $ do
  doc $ DDescription "This contract measures boa constrictor."
  unpair
  dip drop
  entryCase @Parameter (Proxy @PlainEntrypointsKind)
    ( #cMeasureBoaConstrictor /-> do
        doc $ DDescription "Measure the boa constrictor with given method."
        fromNamed #method
        caseT @MeasurementMethod
          ( #cParrotStep /-> push @Natural 38
          , #cMonkeyStep /-> push @Natural 31
          , #cElephantStep /-> push @Natural 9
          )
    , #cZero /-> do
        doc $ DDescription "Put zero to the storage."
        drop
        push @Natural 0
    )
  nil
  pair
