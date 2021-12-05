module Linear.Compat (module Linear.Compat) where

import GHC.Records.Compat qualified as GHC
import Linear.V2
import Linear.V3

instance GHC.HasField "x" (V2 a) a where
  hasField (V2 x y) = (\nx -> V2 nx y, x)

instance GHC.HasField "y" (V2 a) a where
  hasField (V2 x y) = (\ny -> V2 x ny, y)

instance GHC.HasField "x" (V3 a) a where
  hasField (V3 x y z) = (\nx -> V3 nx y z, x)

instance GHC.HasField "y" (V3 a) a where
  hasField (V3 x y z) = (\ny -> V3 x ny z, y)

instance GHC.HasField "z" (V3 a) a where
  hasField (V3 x y z) = (\nz -> V3 x y nz, z)
