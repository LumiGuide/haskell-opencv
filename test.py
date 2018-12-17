#!/nix/store/59kynf2j381njbcn0mcyafm9kw51sa8l-python3-3.7.1/bin/python

import subprocess
import sys

p = subprocess.run( ["nix-build", "-A", "haskellPackages.opencv"]
                    , capture_output=True
                    , cwd = "/home/bas.van.dijk/engineering/haskell-opencv"
                    , universal_newlines=True
                    )


#print(p.stdout)
print(p.stderr)

if "Segmentation fault" in p.stderr:
    sys.exit(1)
elif p.returncode != 0:
    sys.exit(125)
