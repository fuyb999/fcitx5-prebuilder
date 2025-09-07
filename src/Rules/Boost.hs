{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.Boost (boostRule) where

import Base
import CMakeBuilder
import Data.List.Extra (intercalate)

data Boost = Boost
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult Boost = ()

boostRule :: Rules ()
boostRule = do
  buildBoost <-
    useCMake $
      (cmakeBuilder "boost")
        { source = \out -> do
            boostVersion <- getConfig' "boost_version"
            sha256 <- getConfig' "boost_sha256"
            let boostTag = "boost-" <> boostVersion
                boostTar = boostTag <> "-cmake" <.> "tar" <.> "xz"  -- Original format
                boostUrl = "https://github.com/boostorg/boost/releases/download" </> boostTag <> "/"  -- Original URL
            _ <- download boostUrl boostTar sha256
            cmd_ (Cwd out) "tar" "xf" boostTar
            pure $ out </> boostTag,
          cmakeFlags =
            const
              [ "-DCMAKE_INSTALL_MESSAGE=NEVER",
                "-DBOOST_EXCLUDE_LIBRARIES="
                  <> intercalate
                    ";"
                    [ "asio",
                      "charconv",
                      "chrono",
                      "cobalt",
                      "context",
                      "contract",
                      "coroutine",
                      "date_time",
                      "fiber",
                      "graph",
                      "json",
                      "locale",
                      "log",
                      "math",
                      "nowide",
                      "process",
                      "program_options",
                      "serialization",
                      "stacktrace",
                      "test",
                      "thread",
                      "timer",
                      "type_erasure",
                      "url",
                      "wave",
                      "wserialization"
                    ],
                "-DBOOST_IOSTREAMS_ENABLE_BZIP2=OFF",
                "-DBOOST_IOSTREAMS_ENABLE_ZLIB=OFF",
                "-DBOOST_IOSTREAMS_ENABLE_LZMA=OFF",
                "-DBOOST_IOSTREAMS_ENABLE_ZSTD=OFF",
                "-DBOOST_INSTALL_LAYOUT=system"
              ]
        }

  -- Simple approach: Build regex manually after CMake build
  "/prebuilder" </> outputDir </> "boost-build-x86_64" </> ".regex-built" %> \outFile -> do
    let outDir = "/prebuilder" </> outputDir </> "boost" </> "x86_64"
    boostVersion <- getConfig' "boost_version"
    let boostTag = "boost-" <> boostVersion
        src = outputDir </> boostTag
        buildDir = takeDirectory outFile  </> "b2_build"

    -- Build regex using b2
    cmd_ (Cwd src) "./bootstrap.sh"
    cmd_ (Cwd src) "./b2"
        "--with-regex"
        ("--build-dir=" <> buildDir)
        "variant=release"
        "link=static"
        "install"
        ("--prefix=" <> outDir)

    writeFile' outFile ""

  "boost" ~> do
    env <- getAndroidEnv
    let abiList = getABIList env
        firstAbi = head abiList

    -- Build with CMake first
    _ <- buildBoost $ WithAndroidEnv Boost env

    -- Then build regex for each ABI (rename 'abi' to avoid conflict)
    forM_ abiList $ \currentAbi -> do
        let markerFile = "/prebuilder" </> outputDir </> "boost-build-x86_64" </> ".regex-built"
        need [markerFile]

    liftIO $ do
      getDirectoryFilesIO
        (outputDir </> "boost" </> firstAbi </> "include" </> "boost")
        ["//*"]
        >>= mapM_
          ( \x ->
              copyFileAndCreateDir (outputDir </> "boost" </> firstAbi </> "include" </> "boost" </> x) $
                outputDir </> "boost" </> "include" </> "boost" </> x
          )
      forM_ abiList $ \a -> do
        -- symlink headers for each abi to reduce size
        let path = outputDir </> "boost" </> a </> "include"
        whenM (doesPathExist path) $ removePathForcibly path
        createDirectoryLink (".." </> "include") path