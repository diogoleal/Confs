#!/bin/sh

DIR_EXTENSIONS="/home/diogo/.local/share/gnome-shell/extensions"

# GNOME extensions
git clone https://github.com/Tudmotu/gnome-shell-extension-clipboard-indicator.git ${DIR_EXTENSIONS}/clipboard-indicator@tudmotu.com
gnome-extensions enable clipboard-indicator@tudmotu.com

gsettings set org.gnome.software download-updates true
gsettings set org.gnome.software download-updates-notify true
gsettings set org.gnome.software first-run false
gsettings set org.gnome.desktop.input-sources xkb-options "['caps:swapescape']"
gsettings set org.gnome.desktop.peripherals.touchpad disable-while-typing true
gsettings set org.gnome.desktop.peripherals.touchpad tap-to-click true
gsettings set org.gnome.desktop.peripherals.touchpad two-finger-scrolling-enabled true
gsettings set org.gnome.desktop.interface clock-show-weekday true
gsettings set org.gnome.shell favorite-apps "[
                                              'com.vivaldi.Vivaldi.desktop',
                                              'org.gnome.Nautilus.desktop',
                                              'org.gnome.Ptyxis.desktop',
                                              'sublime_text.desktop',
                                              'net.cozic.joplin_desktop.desktop',
                                              'io.ente.auth.desktop',
                                              'org.telegram.desktop'.
                                              'org.onlyoffice.desktopeditors.desktop']"
gnome-session-quit --logout
