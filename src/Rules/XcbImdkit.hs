{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.XcbImdkit (xcbImdkitRule) where

import Base
import CMakeBuilder

data XcbImdkit = XcbImdkit
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult XcbImdkit = ()

xcbImdkitRule :: Rules ()
xcbImdkitRule = do
  buildXcbImdkit <-
    useCMake $
      (cmakeBuilder "xcb-imdkit")
        { cmakeFlags = const ["-DBUILD_SHARED_LIBS=OFF", "-DCMAKE_POSITION_INDEPENDENT_CODE=ON", "-DUSE_SYSTEM_UTHASH=OFF"]
          -- disable logging to cwd; remove absolute path by __FILE__ macro
--          preBuild = BuildAction $ \_ src -> do
--            cmd_ (Cwd src) "git checkout ."
--            cmd_ (Cwd src) "git apply ../patches/xcb-imdkit.patch"
        }
  "xcb-imdkit" ~> buildWithAndroidEnv buildXcbImdkit XcbImdkit
