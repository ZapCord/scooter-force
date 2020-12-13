# scooter-force
This is a pre-release version of visualizing forces on a pressure-pad mounted to a leg scooter.
The .csv input is the 1d csv created by a KITRONYX Tinn(Snowboard) with an MS9758 sensor 16x10 matrix pressure pad.
Recommended only to use 0.3.0 and above due to performance related bug fixing.

Running in R:
Assuming you have Shiny installed:
use: 
```
runGitHub("scooter-force","ZapCord",ref="v0.4.0")
```

Alternatively, download the release, unzip, and run by using: 
```
runApp('scooter-force-0.4.0')
```
