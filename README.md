# mac2linuxkbd

Key rebindings to match a macOS style on Linux.

This program uses Interception Tools to capture the system key events and 
replace them if there is a pattern to do so. Becase of the nature of the
inner workings of Interception Tools, this software can work on X11,
Wayland and TTY. *HOWEVER* this sofware just works with X11 because
we need to get the current application information. This is just one
function that can be changed to support Wayland.

Bindings can be changed in Configs.hs. The current ones are design
to work with ISO ES keyboards.

In some keyboards two keys are incorrectly swapped by default. While this can be fixed using this tool, I prefer to use this possible solutions:
- https://linux-tips.com/t/apple-magic-keyboard-mapping-problem-under-linux/767
- https://wiki.archlinux.org/title/Apple_Keyboard#%3C_and_%3E_have_changed_place_with_^_and_%C2%B0_(or_@_and_#,_or_%60_and_~)

*Tested with Ubuntu 20.04 and GHC 9.2*

### USAGE:

```haskell
make -- Builds the tool.
sudo make install -- Self explanatory.
sudo make enable -- Enable Systemd service.
sudo make disable -- Disable service.
sudo make start -- Start service.
sudo make stop -- Stop service.
sudo make uninstall
sudo make InterceptionToolsII -- Download, build and install Interception Tools.
sudo make InterceptionToolsUU -- Uninstall Interception Tools.
```
