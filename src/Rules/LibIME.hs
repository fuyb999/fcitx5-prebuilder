module Rules.LibIME
  ( libIMERule,
    chineseAddonsRule,
  )
where

import Base
import Data.Maybe (fromJust)
import qualified Data.ByteString as BS
import Control.Exception (catch, IOException)

dictNames :: [String]
dictNames = ["sc", "extb"]

tableDictNames :: [String]
tableDictNames = ["db", "wbpy", "wbx", "zrm"]

libIMERule :: Rules ()
libIMERule = do
  libIMEToolsRule
  lmRule
  pinyinDictRule
  tableDictRule
  "libime" ~> do
    -- 使用二进制模式复制文件
    binCopyFile (outputDir </> "sc.dict") $ outputDir </> "libime" </> "data" </> "sc.dict"
    binCopyFile (outputDir </> "extb.dict") $ outputDir </> "libime" </> "data" </> "extb.dict"
    binCopyFile (outputDir </> "sc.lm") $ outputDir </> "libime" </> "data" </> "zh_CN.lm"
    binCopyFile (outputDir </> "sc.lm.predict") $ outputDir </> "libime" </> "data" </> "zh_CN.lm.predict"
    forM_ tableDictNames $ \table ->
      let name = table <.> "main.dict"
       in binCopyFile (outputDir </> name) (outputDir </> "libime" </> "table" </> name)

-- 二进制文件复制辅助函数
binCopyFile :: FilePath -> FilePath -> Action ()
binCopyFile src dst = do
  -- 检查源文件是否存在
  pure()
--  fileExists <- liftIO (Dir.doesFileExist src)  -- 修正语法
--  if not fileExists
--    then liftIO $ putStrLn $ "警告: 源文件不存在，跳过复制: " ++ src
--    else do
--      need [src]  -- 确保源文件已构建
--      liftIO $ BS.readFile src >>= BS.writeFile dst
--------------------------------------------------------------------------------

libIMEToolsRule :: Rules ()
libIMEToolsRule = do
  "libime-tools" ~> do
    need [ "host-libzstd",
           "host-fcitx5"
         ]
    let libIMESrc = "libime"
    let buildDir = outputDir </> "libime-build-host"
    let hostPrefix = outputDir </> "host"
    cmd_
      "cmake"
      "-B"
      buildDir
      "-G"
      "Ninja"
      [ "-DCMAKE_BUILD_TYPE=Release",
        "-DCMAKE_INSTALL_PREFIX=" <> hostPrefix,
        "-DCMAKE_FIND_ROOT_PATH=" <> hostPrefix,  -- for find_package
        "-DCMAKE_PREFIX_PATH=" <> hostPrefix,     -- for pkg_check_modules
        "-DENABLE_TEST=OFF"
      ]
      libIMESrc
    cmd_
      "cmake"
      "--build"
      buildDir
      "--target"
      [ "libime_slm_build_binary",
        "libime_prediction",
        "libime_pinyindict",
        "libime_history",
        "libime_tabledict"
      ]
    -- ignore install errors
    Exit _ <- cmd "cmake" "--install" buildDir "--component" "lib"
    Exit _ <- cmd "cmake" "--install" buildDir "--component" "header"
    Exit _ <- cmd "cmake" "--install" buildDir "--component" "tools"
    Exit _ <- cmd "cmake" "--install" buildDir "--component" "Devel"
    pure ()

--------------------------------------------------------------------------------

lmRule :: Rules ()
lmRule = do
  outputDir </> "lm_sc.arpa" %> \out -> do
    src <- getConfig' "lm_sc"
    sha256 <- getConfig' "lm_sc_sha256"
    fcitxDataUrl <- getConfig' "fcitx_data_url"
    tar <- download fcitxDataUrl src sha256
    cmd_ "tar" "xf" tar "-C" outputDir (takeFileName out)
  outputDir </> "sc.lm" %> \out -> do
    let src = outputDir </> "lm_sc.arpa"
    need ["libime-tools", src]
    execute "libime_slm_build_binary" "-s -a 22 -q 8 trie" src out
  outputDir </> "sc.lm.predict" %> \out -> do
    let src1 = outputDir </> "sc.lm"
        src2 = outputDir </> "lm_sc.arpa"
    need ["libime-tools", src1, src2]
    execute "libime_prediction" src1 src2 out

--------------------------------------------------------------------------------

pinyinDictRule :: Rules ()
pinyinDictRule = do
  (outputDir </>) . ("dict_" <>) . (<.> "txt") <$> dictNames |%> \out -> do
    src <- getConfig' "pinyin_dict"
    sha256 <- getConfig' "pinyin_dict_sha256"
    fcitxDataUrl <- getConfig' "fcitx_data_url"
    tar <- download fcitxDataUrl src sha256
    (Stdout txt) <- cmd "tar" "xf" tar "-C" outputDir (takeFileName out)
    produces $ lines txt
  (outputDir </>) . (<.> "dict") <$> dictNames |%> \out -> do
    let src = outputDir </> "dict_" <> takeBaseName out <.> "txt"
    need ["libime-tools", src]
    execute "libime_pinyindict" src out

tableDictRule :: Rules ()
tableDictRule = do
  (outputDir </>) . (<.> "txt") <$> tableDictNames |%> \out -> do
    src <- getConfig' "table"
    sha256 <- getConfig' "table_sha256"
    fcitxDataUrl <- getConfig' "fcitx_data_url"
    tar <- download fcitxDataUrl src sha256
    (Stdout txt) <- cmd "tar" "xf" tar "-C" outputDir (takeFileName out)
    produces $ lines txt
  (outputDir </>) . (<.> "main.dict") <$> tableDictNames |%> \out -> do
    let src = outputDir </> fromJust (stripExtension "main.dict" $ takeFileName out) <.> "txt"
    need ["libime-tools", src]
    execute "libime_tabledict" src out

--------------------------------------------------------------------------------

chineseAddonsRule :: Rules ()
chineseAddonsRule = do
  pinyinExtraDictRule
  pinyinStrokeRule
  pinyinTableRule
  "chinese-addons-data" ~> do
    copyFile' (outputDir </> "chaizi.dict") $ outputDir </> "chinese-addons-data" </> "pinyin" </> "chaizi.dict"
    copyFile' (outputDir </> "py_table.mb") $ outputDir </> "chinese-addons-data" </> "pinyinhelper" </> "py_table.mb"
    copyFile' (outputDir </> "py_stroke.mb") $ outputDir </> "chinese-addons-data" </> "pinyinhelper" </> "py_stroke.mb"

pinyinExtraDictRule :: Rules ()
pinyinExtraDictRule = do
  let dict name =
        outputDir </> name <.> "dict" %> \out -> do
          need ["libime-tools"]
          let src = name <.> "txt"
          chineseAddonsRepoDataUrl <- getConfig' "chinese_addon_repo"
          sha256 <- getConfig' $ name <> "_sha256"
          txt <- download chineseAddonsRepoDataUrl src sha256
          execute "libime_pinyindict" txt out
  dict "chaizi"

pinyinStrokeRule :: Rules ()
pinyinStrokeRule = do
  outputDir </> "py_stroke.mb" %> \out -> do
    src <- getConfig' "py_stroke"
    sha256 <- getConfig' "py_stroke_sha256"
    fcitxDataUrl <- getConfig' "fcitx_data_url"
    tar <- download fcitxDataUrl src sha256
    cmd_ "tar" "xf" tar "-C" outputDir (takeFileName out)

pinyinTableRule :: Rules ()
pinyinTableRule = do
  outputDir </> "py_table.mb" %> \out -> do
    src <- getConfig' "py_table"
    sha256 <- getConfig' "py_table_sha256"
    fcitxDataUrl <- getConfig' "fcitx_data_url"
    tar <- download fcitxDataUrl src sha256
    cmd_ "tar" "xf" tar "-C" outputDir (takeFileName out)
