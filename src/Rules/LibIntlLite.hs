{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.LibIntlLite (libintlLiteRule) where

import Base
import CMakeBuilder

data LibIntlLite = LibIntlLite
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult LibIntlLite = ()

libintlLiteRule :: Rules ()
libintlLiteRule = do
  buildLibintlLite <-
    useCMake $
      (cmakeBuilder "libintl-lite")
        { cmakeFlags = const ["-DENABLE_NLS=OFF"],
          preBuild = BuildAction $ \_ src -> do
                    cmd_ (Cwd src) "git checkout ."
                    cmd_ (Cwd src) "git apply ../patches/libintl-build.patch"
        }
  "libintl-lite" ~> buildWithAndroidEnv buildLibintlLite LibIntlLite
