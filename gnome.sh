#!/bin/sh

DIR_EXTENSIONS="/home/diogo/.local/share/gnome-shell/extensions"

sudo dnf upgrade -y --refresh
sudo dnf remove firefox -y

# Flatpak
flatpak --user remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
flatpak install flathub --user -y net.ankiweb.Anki \
                                  org.telegram.desktop \
                                  io.podman_desktop.PodmanDesktop \
                                  dev.geopjr.Tuba \
                                  io.dbeaver.DBeaverCommunity \
                                  org.gnome.World.PikaBackup \
                                  org.mozilla.firefox \
                                  org.freedesktop.Platform.ffmpeg-full/x86_64/23.08 \
                                  com.helix_editor.Helix


# GNOME extensions
git clone https://github.com/Tudmotu/gnome-shell-extension-clipboard-indicator.git ${DIR_EXTENSIONS}/clipboard-indicator@tudmotu.com
gnome-extensions enable clipboard-indicator@tudmotu.com

# GNOME
gsettings set org.gnome.software download-updates true
gsettings set org.gnome.software download-updates-notify true
gsettings set org.gnome.software first-run false
gsettings set org.gnome.desktop.input-sources xkb-options "['caps:swapescape']"
gsettings set org.gnome.desktop.peripherals.touchpad disable-while-typing true
gsettings set org.gnome.desktop.peripherals.touchpad tap-to-click true
gsettings set org.gnome.desktop.peripherals.touchpad two-finger-scrolling-enabled true
gsettings set org.gnome.desktop.interface clock-show-weekday true
gsettings set org.gnome.shell favorite-apps "[
                                              'org.mozilla.firefox.desktop',
                                              'org.gnome.Nautilus.desktop',
                                              'org.gnome.Boxes.desktop',
                                              'jetbrains-pycharm-ce.desktop',
                                              'terminator.desktop',
                                              'virt-manager.desktop']"
gnome-session-quit --logout
