module Rules.Fcitx5
  ( hostFcitx5Rule,
    spellDictRule,
  )
where

import Base

hostFcitx5Rule :: Rules ()
hostFcitx5Rule = do
  "host-fcitx5" ~> do
    need ["xcb-imdkit"]
    let fcitx5Src = "fcitx5"
    let buildDir = outputDir </> "fcitx5-build-host"
    let hostPrefix = outputDir </> "host"

    -- 获取 xcb-imdkit 的静态库路径
    xcbImdkitLib <- getLibraryPath "xcb-imdkit" "libxcb-imdkit.a"
    xcbImdkitInclude <- getIncludePath "xcb-imdkit"

    cmd_
      "cmake"
      "-B"
      buildDir
      "-G"
      "Ninja"
      [ "-DCMAKE_BUILD_TYPE=Release",
        "-DCMAKE_INSTALL_PREFIX=" <> hostPrefix,
        "-DENABLE_TEST=OFF",
        "-DENABLE_COVERAGE=ON",
        "-DENABLE_ENCHANT=ON",
        "-DENABLE_X11=ON",
        "-DENABLE_WAYLAND=ON",
        "-DENABLE_DBUS=ON",
        "-DENABLE_DOC=OFF",
        "-DENABLE_SERVER=ON",
        "-DENABLE_KEYBOARD=ON",
        "-DENABLE_XDGAUTOSTART=ON",
        "-DENABLE_EMOJI=OFF",
        "-DENABLE_LIBUUID=OFF"
        , "-DCMAKE_FIND_LIBRARY_SUFFIXES=.a"
        , "-DXCBImdkit_LIBRARY=" ++ xcbImdkitLib
        , "-DXCBImdkit_INCLUDE_DIR=" ++ xcbImdkitInclude
--        , "-DXCBImdkit_DIR=" <> outputDir </> "xcb-imdkit/x86_64/lib/cmake/XCBImdkit",
        , "-DXCBImdkit_DIR=/prebuilder/build/xcb-imdkit/x86_64/lib/cmake/XCBImdkit"
      ]
      fcitx5Src
    cmd_
      "cmake"
      "--build"
      buildDir
      "--target"
      [ "Fcitx5Utils",
        "comp-spell-dict"
      ]
    -- ignore install errors
    Exit _ <- cmd "cmake" "--install" buildDir
    -- install "comp-spell-dict" manually
    copyFile' (buildDir </> "src" </> "modules" </> "spell" </> "comp-spell-dict") $ hostPrefix </> "bin" </> "comp-spell-dict"
    pure ()

--------------------------------------------------------------------------------

spellDictRule :: Rules ()
spellDictRule = do
  outputDir </> "en_dict.txt" %> \out -> do
    src <- getConfig' "en_dict"
    sha256 <- getConfig' "en_dict_sha256"
    fcitxDataUrl <- getConfig' "fcitx_data_url"
    tar <- download fcitxDataUrl src sha256
    cmd_ "tar" "xf" tar "-C" outputDir (takeFileName out)
  outputDir </> "en_dict.fscd" %> \out -> do
    let src = outputDir </> "en_dict.txt"
    need [src, "host-fcitx5"]
    execute "comp-spell-dict" "--comp-dict" src out
  "spell-dict" ~> do
    copyFile' (outputDir </> "en_dict.fscd") $ outputDir </> "spell-dict" </> "en_dict.fscd"
