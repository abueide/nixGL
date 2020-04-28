{ ## Nvidia informations.
  # Version of the system kernel module. Let it to null to enable auto-detection.
  nvidiaVersion ? null,
  # Hash of the Nvidia driver .run file. null is fine, but fixing a value here
  # will be more reproducible and more efficient.
  nvidiaHash ? null,
  # Enable 32 bits driver
  # This is one by default, you can switch it to off if you want to reduce a
  # bit the size of nixGL closure.
  enable32bits ? true,
  writeTextFile, shellcheck, pcre, runCommand, linuxPackages, fetchurl, lib,
  bumblebee, libglvnd, vulkan-validation-layers, mesa_drivers,
  pkgsi686Linux,zlib, libdrm, xorg, wayland, gcc
}:

let
  # The nvidia version. Either fixed by the `nvidiaVersion` argument, or
  # auto-detected.
  _nvidiaVersion = if nvidiaVersion != null
  then nvidiaVersion
  else
    # This is the auto-detection mecanism. This is ugly.
    # We read /proc/driver/nvidia/version which is set by the Nvidia driver kernel module.
    # This fails if the nvidia driver kernel module is not loaded.
    # I'd like to just read the file using `${/proc/driver/nvidia/version}` and
    # then let nix invalidate the derivation if the content of this file
    # changes, but that's not possible, see
    # https://github.com/NixOS/nix/issues/3539
    # But /proc is readable at build time! So runCommand works fine.
    import (runCommand "auto-detect-nvidia" {
      time = builtins.currentTime;
    }
    ''
        # Written this way so if the version file does not exists, the script crashs
        VERSION="$(${pcre}/bin/pcregrep -o1 'Module +([0-9]+\.[0-9]+)' /proc/driver/nvidia/version)"
        echo "\"$VERSION\"" > $out
    '');

    addNvidiaVersion = drv: drv.overrideAttrs(oldAttrs: {
      name = oldAttrs.name + "-${_nvidiaVersion}";
    });

    writeExecutable = { name, text } : writeTextFile {
      inherit name text;

      executable = true;
      destination = "/bin/${name}";


      checkPhase = ''
       ${shellcheck}/bin/shellcheck "$out/bin/${name}"

       # Check that all the files listed in the output binary exists
       for i in $(${pcre}/bin/pcregrep  -o0 '/nix/store/.*?/[^ ":]+' $out/bin/${name})
       do
         ls $i > /dev/null || (echo "File $i, referenced in $out/bin/${name} does not exists."; exit -1)
       done
      '';
    };

in
  rec {
    nvidia = (linuxPackages.nvidia_x11.override {
    }).overrideAttrs(oldAttrs: rec {
      name = "nvidia-${_nvidiaVersion}";
      src = let url ="http://download.nvidia.com/XFree86/Linux-x86_64/${_nvidiaVersion}/NVIDIA-Linux-x86_64-${_nvidiaVersion}.run";
      in if nvidiaHash != null
      then fetchurl {
        inherit url;
        sha256 = nvidiaHash;
      } else
      builtins.fetchurl url;
      useGLVND = true;
    });

    nvidiaLibsOnly = nvidia.override {
      libsOnly = true;
      kernel = null;
    };

  nixGLNvidiaBumblebee = addNvidiaVersion (writeExecutable {
    name = "nixGLNvidiaBumblebee";
    text = ''
      #!/usr/bin/env sh
      export LD_LIBRARY_PATH=${lib.makeLibraryPath [nvidia]}:$LD_LIBRARY_PATH
      ${bumblebee.override {nvidia_x11 = nvidia; nvidia_x11_i686 = nvidia.lib32;}}/bin/optirun --ldpath ${lib.makeLibraryPath ([libglvnd nvidia] ++ lib.optionals enable32bits [nvidia.lib32 pkgsi686Linux.libglvnd])} "$@"
    '';
  });

  # TODO: 32bit version? Not tested.
  nixNvidiaWrapper = api: addNvidiaVersion (writeExecutable {
    name = "nix${api}Nvidia";
    text = ''
      #!/usr/bin/env sh
      ${lib.optionalString (api == "Vulkan") ''export VK_LAYER_PATH=${vulkan-validation-layers}/share/vulkan/explicit_layer.d''}

        ${lib.optionalString (api == "Vulkan") ''export VK_ICD_FILENAMES=${nvidia}/share/vulkan/icd.d/nvidia.json${lib.optionalString enable32bits ":${nvidia.lib32}/share/vulkan/icd.d/nvidia.json"}:$VK_ICD_FILENAMES''}
        export LD_LIBRARY_PATH=${lib.makeLibraryPath ([
          libglvnd
          nvidiaLibsOnly
        ] ++ lib.optional (api == "Vulkan") vulkan-validation-layers
        ++ lib.optionals enable32bits [nvidia.lib32 pkgsi686Linux.libglvnd])
      }:''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}
        "$@"
      '';
    });

  # TODO: 32bit version? Not tested.
  nixGLNvidia = nixNvidiaWrapper "GL";

  # TODO: 32bit version? Not tested.
  nixVulkanNvidia = nixNvidiaWrapper "Vulkan";

  nixGLIntel = writeExecutable {
    name = "nixGLIntel";
    # add the 32 bits drivers if needed
    text = let
      drivers = [mesa_drivers] ++ lib.optional enable32bits pkgsi686Linux.mesa_drivers;
    in ''
      #!/usr/bin/env sh
      export LIBGL_DRIVERS_PATH=${lib.makeSearchPathOutput "lib" "lib/dri" drivers}
      export LD_LIBRARY_PATH=${
        lib.makeLibraryPath drivers
      }:$LD_LIBRARY_PATH
      "$@"
    '';
  };

  nixVulkanIntel = writeExecutable {
    name = "nixVulkanIntel";
    text = let
      # generate a file with the listing of all the icd files
      icd = runCommand "mesa_icd" {}
        (
          # 64 bits icd
          ''ls ${mesa_drivers}/share/vulkan/icd.d/*.json > f
          ''
          #  32 bits ones
          + lib.optionalString enable32bits ''ls ${pkgsi686Linux.mesa_drivers}/share/vulkan/icd.d/*.json >> f
          ''
          # concat everything as a one line string with ":" as seperator
          + ''cat f | xargs | sed "s/ /:/g" > $out''
          );
      in ''
     #!/usr/bin/env bash
     if [ -n "$LD_LIBRARY_PATH" ]; then
       echo "Warning, nixVulkanIntel overwriting existing LD_LIBRARY_PATH" 1>&2
     fi
     export VK_LAYER_PATH=${vulkan-validation-layers}/share/vulkan/explicit_layer.d
     ICDS=$(cat ${icd})
     export VK_ICD_FILENAMES=$ICDS:$VK_ICD_FILENAMES
     export LD_LIBRARY_PATH=${lib.makeLibraryPath [
       zlib
       libdrm
       xorg.libX11
       xorg.libxcb
       xorg.libxshmfence
       wayland
       gcc.cc
     ]}:$LD_LIBRARY_PATH
     exec "$@"
    '';
  };

  nixGLCommon = nixGL:
  runCommand "nixGLCommon" {
  buildInuts = [nixGL];
  }
  ''
  mkdir -p "$out/bin"
  # star because nixGLNvidia... have version prefixed name
  cp ${nixGL}/bin/* "$out/bin/nixGL";
  '';

  # The output derivation contains nixGL which point either to
  # nixGLNvidia or nixGLIntel using an heuristic.
  nixGLDefault =
    if builtins.pathExists "/proc/driver/nvidia/version"
    then nixGLCommon nixGLNvidia
    else nixGLCommon nixGLIntel;

  }