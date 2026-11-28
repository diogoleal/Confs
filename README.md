# My dotfiles

Personal Linux dotfiles, managed with a `Makefile`. Configs live in this repo and get **symlinked** into `$HOME` — once a target has been run, editing the tracked file here changes the live config immediately, no rebuild step needed.

Primarily built for Arch Linux (`make arch`), with legacy Fedora/Ubuntu targets kept around from earlier setups.

## Usage

```sh
make <target>      # symlink/install one piece of config
make all            # setup + arch + fish + kubectl + emacs + kitty + zellij + virt + lite-xl
make pacman          # alias for `make all`
make clean           # remove the symlinks/binaries this Makefile created
```

This repo must live at `~/Workspace/Confs` — the `Makefile` hardcodes that path (`DIR_CONF`) to resolve the symlinks correctly.

### Targets

| Target | What it does |
|---|---|
| `setup` | Creates `~/bin`, `~/lib`, `~/Workspace`; symlinks `.gitconfig` |
| `arch` | Installs packages via `pacman`/`yay`, enables bluetooth, installs the pacman cache-cleaning hook |
| `fish` | Installs Fisher + symlinks fish config/functions/completions, sets fish as the default shell |
| `emacs` | Symlinks Emacs config and the Omarchy theme/font hook, installs and (re)starts the `emacs.service` user unit |
| `lite-xl` | Installs Lite XL, symlinks its config and Omarchy theme plugin, and links the theme/font hook |
| `kitty` | Symlinks the kitty terminal config |
| `zellij` | Symlinks the zellij config |
| `kubectl` | Downloads `kubectl` into `~/bin` |
| `go` | Downloads and extracts the latest Go release into `~/bin` |
| `flatpak` | Adds Flathub and installs a small set of Flatpak apps |
| `virt` | Installs QEMU/KVM + `virt-manager`, enables `libvirtd` |
| `krew-install` | Installs `krew` (kubectl plugin manager) |
| `clean` | Removes everything the targets above symlinked/installed |

There is no test suite or lint step; CI is limited to `.github/workflows/opencode.yml`.

## Layout

### `.config/`

- **`fish/`** — fish shell config (the default shell once `make fish` runs)
  - `config.fish` — PATH setup, `direnv`/`pyenv`/`kubectl` completion hooks, small helper functions (`e` for emacsclient, `S`/`_` for sudo, `tempo`/`myip`/`sprunge`), env vars for containerd and libvirt
  - `functions/alias.fish` — extra shell functions (`tchau` to power off, `v` for vim, plus duplicates of a few helpers above)
  - `functions/mantainer_void.fish` — helper to sync a local `void-packages` checkout with upstream
  - `k9s.fish` — fish completion script for `k9s`
- **`kitty/`** — kitty terminal config (`kitty.conf`; FiraCode font, splits layout, custom mouse/scroll/window/tab keybindings). `kitty.conf.mod` is untracked/local-only and not managed by `make`.
- **`zellij/`** — zellij terminal multiplexer config (`config.kdl`). Patches just the default keybinds that collide with kitty's own OS-level shortcuts (`Ctrl+q`, `Ctrl+t`, `Alt+h`/`Alt+l`, `Ctrl+c`/`Ctrl+f` inside scroll/search/rename modes), leaving the rest of zellij's defaults untouched.
- **`lite-xl/`** — [Lite XL](https://lite-xl.com), a light, open source editor similar to Sublime Text
  - `init.lua` — 4-space soft tabs, 100-column ruler, trims trailing whitespace on save, hides build/venv folders, and adds Sublime keys (`Ctrl+K Ctrl+B` toggles the tree, `Alt+Z` toggles word wrap). `Ctrl+Shift+P`, `Ctrl+P`, `Ctrl+D` and `Ctrl+Shift+D`/`K` already work as in Sublime.
  - `plugins/omarchy.lua` — takes its colors from `omarchy-theme-color` and its font from `omarchy font current`. It checks a stamp file every second and re-syncs when the file changes, so a running editor follows `omarchy theme set` / `omarchy font set`. Run `omarchy:sync` from the command palette to re-sync by hand.
- **`systemd/user/`** — `emacs.service`, a user unit that runs the Emacs daemon with the graphical session so `emacsclient` (see the `e` fish function) attaches instantly. The first start installs every package, so it can take a few minutes.
- **`omarchy/hooks/`** — linked into Omarchy's `theme-set.d` and `font-set.d` hooks: `emacs-sync` tells a running Emacs to re-read the Omarchy theme and font, and `lite-xl-sync` touches the stamp file that Lite XL checks.
- **`htop/`, `pipewire/`** — present for reference/backup only, **not** wired to any `make` target:
  - `htop/htoprc` — htop UI/meter layout
  - `pipewire/pipewire.conf.d/99-input-denoising.conf` — adds an RNNoise-based "Noise Canceling source" filter-chain (via `librnnoise_ladspa.so`) for mic denoising

### Other top-level paths

- **`.emacs.d/`** — Emacs config (`package.el` + `use-package`): `init.el`, `early-init.el`, `lisp/omarchy.el`. Guia completo com atalhos em [`.emacs.d/README.md`](.emacs.d/README.md)
  - **Omarchy** — `lisp/omarchy.el` builds the `omarchy` theme from `omarchy-theme-color` and takes the font from `omarchy-font-current`, so Emacs follows `omarchy theme set` / `omarchy font set` live. Without Omarchy it falls back to `modus-vivendi`.
  - **Claude** — `C-c c` opens the [claude-code-ide](https://github.com/manzaltu/claude-code-ide.el) menu (Claude Code CLI in vterm, with Emacs as its IDE: selection, diagnostics, ediff for proposed edits, xref/imenu/tree-sitter tools). `C-c a` is the [gptel](https://github.com/karthink/gptel) prefix for Claude through the API (`ANTHROPIC_API_KEY` or an `api.anthropic.com` entry in `~/.authinfo.gpg`).
  - **Editing** — vertico/orderless/consult/embark/corfu for completion, eglot + flymake for the language servers installed by `make arch`, magit, treemacs (`F8`), vterm (`C-c t`).
- **`.gitconfig`** — personal git config (contains email/proxy settings; not for reuse elsewhere)
- **`etc/`** — system configs symlinked to `/etc/` by `make arch` (pacman hooks, PackageKit, pulse, crontab)
- **`dnf/`** — legacy Fedora `dnf` config
- **`gnome.sh`** — one-off GNOME `gsettings`/extension setup script, run manually (not wired into `make`)
- **`olds/`** — deprecated (zsh, vim, X11, old `setup.sh`); not maintained
- **`setup.sh`** — stale Fedora-only bootstrap script; prefer the `Makefile` targets instead
