#!/usr/bin/env nix-shell
#!nix-shell -i runhaskell -p "haskellPackages.ghcWithPackages(p: with p; [hspec process])" -p nix -p bubblewrap

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Process
import Test.Hspec
import System.Environment

-- Run `./Test.hs --match "/Sanity/"` to ensure that non wrapped
-- binaries fails on NixOS. This is done using bubblewrap in order to hide the
-- system paths.

-- | Utils function: run a command and returns its output.
processOutput = processOutput' . bwrap

processOutput' (p : args) = Text.strip . Text.pack <$> readCreateProcess ((proc (Text.unpack p) (Text.unpack <$> args)) {std_err = Inherit}) ""

-- * OpenGL

-- | Returns the path to the nixGLXXX binary.
getNixGLBin version = (<> ("/bin/" <> version)) <$> processOutput' ["nix-build", "./", "-A", version]

-- | Returns the vendor string associated with a glxinfo wrapped by a nixGL.
getVendorString io = do
  output <- Text.lines <$> io
  pure $ Text.unpack <$> find ("OpenGL version string" `Text.isPrefixOf`) output

-- | Checks that a nixGL wrapper works with glxinfo 32 & 64 bits.
checkOpenGL_32_64 glxinfo32 glxinfo64 vendorName nixGLName = do
  beforeAll (getNixGLBin nixGLName) $ do
    it "32 bits" $ \nixGLBin -> do
      Just vendorString <- getVendorString (processOutput [nixGLBin, glxinfo32, "-B"])
      vendorString `shouldContain` vendorName

    it "64 bits" $ \nixGLBin -> do
      Just vendorString <- getVendorString (processOutput [nixGLBin, glxinfo64, "-B"])
      vendorString `shouldContain` vendorName

-- * Vulkan

-- | Heuristic to detect if vulkan work. `driverName` must appears in the output
checkVulkanIsWorking io = do
  res <- io
  res `shouldSatisfy` ("driverName" `Text.isInfixOf`)

checkVulkanIsNotWorking io = do
  res <- io
  res `shouldNotSatisfy` ("driverName" `Text.isInfixOf`)

-- | Checks that a nixGL wrapper works with glxinfo 32 & 64 bits.
checkVulkan_32_64 vulkaninfo32 vulkaninfo64 vendorName nixGLName = do
  beforeAll (getNixGLBin nixGLName) $ do
    it "32 bits" $ \nixGLBin -> do
      checkVulkanIsWorking (processOutput [nixGLBin, vulkaninfo32])

    it "64 bits" $ \nixGLBin -> do
      checkVulkanIsWorking (processOutput [nixGLBin, vulkaninfo64])

-- Hide system paths on nixos in order to ensure that we are correctly failing.
bwrap cmd =
  [ "bwrap",
    "--ro-bind",
    "/nix",
    "/nix",
    "--bind",
    "/run/current-system",
    "/run/current-system",
    "--bind",
    "/tmp",
    "/tmp",
    "--bind",
    "/run/user",
    "/run/user"
  ]
    <> cmd

main = do
  args <- getArgs

  -- By default it uses the nixpkgs of your system but you can override it with
  -- ./Test channel:nixos-unstable
  -- or
  -- ./Test https://github.com/nixos/nixpkgs/archive/xxxxsh256sumcommit.tar.gz
  let currentChannel = case args of
                     [x] -> Text.pack x
                     [] -> "<nixpkgs>"

  putStrLn "Running tests for nixGL"
  putStrLn "It can take a while, this will build and test all drivers in the background"
  glxinfo64 <- (<> "/bin/glxinfo") <$> processOutput' ["nix-build", currentChannel, "-A", "glxinfo"]
  glxinfo32 <- (<> "/bin/glxinfo") <$> processOutput' ["nix-build", currentChannel, "-A", "pkgsi686Linux.glxinfo"]

  vulkaninfo64 <- (<> "/bin/vulkaninfo") <$> processOutput' ["nix-build", currentChannel, "-A", "vulkan-tools"]
  vulkaninfo32 <- (<> "/bin/vulkaninfo") <$> processOutput' ["nix-build", currentChannel, "-A", "pkgsi686Linux.vulkan-tools"]

  let checkOpenGL = checkOpenGL_32_64 glxinfo32 glxinfo64
      checkVulkan = checkVulkan_32_64 vulkaninfo32 vulkaninfo64

  hspec $ do
    -- This category ensure that tests are failing if not run with nixGL
    -- This allows testing on nixOS
    describe "Sanity" $ do
      describe "OpenGL" $ do
        it "fails with unwrapped glxinfo64" $ do
          vendorString <- getVendorString (processOutput [glxinfo64, "-B"])
          vendorString `shouldBe` Nothing

        it "fails with unwrapped glxinfo32" $ do
          vendorString <- getVendorString (processOutput [glxinfo32, "-B"])
          vendorString `shouldBe` Nothing
      describe "Vulkan" $ do
        it "fails with unwrapped vulkaninfo64" $ do
          checkVulkanIsNotWorking (processOutput [vulkaninfo64])

        it "fails with unwrapped vulkaninfo32" $ do
          checkVulkanIsNotWorking (processOutput [vulkaninfo32])

    describe "NixGL" $ do
      describe "Mesa" $ do
        describe "OpenGL" $ do
          checkOpenGL "Mesa" "nixGLIntel"
        describe "Vulkan" $ do
          checkVulkan "Mesa" "nixVulkanIntel"

      describe "Nvidia - Bumblebee" $ do
        describe "OpenGL" $ do
          checkOpenGL "NVIDIA" "nixGLNvidiaBumblebee"
        describe "Vulkan" $ do
          checkVulkan "NVIDIA" "nixVulkanNvidiaBumblebee"

      describe "Nvidia" $ do
        describe "OpenGL" $ do
          checkOpenGL "NVIDIA" "nixGLNvidia"
        describe "Vulkan" $ do
          checkVulkan "NVIDIA" "nixVulkanNvidia"
