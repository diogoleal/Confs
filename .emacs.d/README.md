# Emacs

Configuração do Emacs 31 (build Wayland) para o Omarchy, com integração ao Claude.

## Instalação

```sh
make emacs
```

Esse comando:

1. instala o `emacs-wayland`, as dependências do vterm e os servidores de linguagem;
2. cria os links de `init.el`, `early-init.el` e `lisp/omarchy.el` em `~/.emacs.d/`;
3. liga o hook `emacs-sync` em `~/.config/omarchy/hooks/theme-set.d/` e `font-set.d/`;
4. copia, habilita e reinicia o `emacs.service`.

Na primeira vez que o Emacs inicia, ele baixa todos os pacotes do MELPA/ELPA e compila o vterm, então pode levar alguns minutos.

## Como funciona

| Arquivo | Papel |
|---|---|
| `early-init.el` | Roda antes de tudo: ajusta o GC, tira as barras da janela e pinta a primeira janela com o fundo do tema Omarchy, para não piscar branco |
| `init.el` | Configuração principal com `package.el` + `use-package`. Os pacotes são instalados sozinhos na primeira execução |
| `lisp/omarchy.el` | Gera o tema `omarchy` a partir das cores de `omarchy-theme-color` e usa a fonte de `omarchy-font-current` |
| `../.config/omarchy/hooks/emacs-sync` | Hook do Omarchy que chama `(omarchy-sync)` no Emacs que estiver aberto |
| `../.config/systemd/user/emacs.service` | Sobe o daemon junto com a sessão gráfica, para que `emacsclient` abra na hora |

O Emacs roda como **daemon**. Para abrir uma janela, use `emacsclient -c`; no terminal, use a função `e` do fish (`emacsclient -c -nw`).

### Omarchy

- Ao trocar o tema com `omarchy theme set` ou a fonte com `omarchy font set`, o hook atualiza na hora o Emacs que estiver aberto.
- Com o Emacs fechado, ele lê o tema e a fonte atuais na próxima vez que iniciar.
- Para forçar a atualização na mão: `M-x omarchy-sync`.
- O tamanho da fonte vem de `omarchy-font-height`, por padrão `120`, que equivale a 12pt.
- Fora do Omarchy, o Emacs usa o tema `modus-vivendi` e a fonte CaskaydiaCove Nerd Font.
- As janelas abrem sem decoração, porque as bordas ficam por conta do Hyprland.

### Claude

Há duas integrações:

- **Claude Code** ([claude-code-ide](https://github.com/manzaltu/claude-code-ide.el)): roda o CLI `claude` num vterm ao lado. O Emacs funciona como a IDE dele:
  - o Claude vê o arquivo aberto, a seleção e os diagnósticos do flymake;
  - as edições que ele propõe aparecem no ediff;
  - ele tem acesso às ferramentas do Emacs: xref, imenu, tree-sitter e projeto.

  Usa o login do Claude Code, sem chave de API.
- **gptel** ([gptel](https://github.com/karthink/gptel)): Claude pela API, em qualquer buffer. O modelo padrão é o `claude-opus-5-5` e pode ser trocado no `C-c a m`. A chave da API pode vir de dois lugares:
  - da variável `ANTHROPIC_API_KEY`;
  - de uma linha no `~/.authinfo.gpg`:
    ```
    machine api.anthropic.com login apikey password <sua-chave>
    ```

## Teclas de atalho

### Claude

| Tecla | Ação |
|---|---|
| `C-c c` | Menu do Claude Code: iniciar, continuar, parar, enviar a seleção etc. |
| `C-c a a` | Abre um chat com o Claude (gptel) |
| `C-c a s` | Envia para o Claude o texto até o cursor ou a seleção |
| `C-c a r` | Pede ao Claude para reescrever a seleção |
| `C-c a f` | Adiciona um arquivo ao contexto do gptel |
| `C-c a m` | Menu do gptel: modelo, contexto, opções |

### Busca e navegação

| Tecla | Ação |
|---|---|
| `C-s` | Busca no buffer (`consult-line`) |
| `C-c s` | Busca no projeto com ripgrep |
| `C-x b` | Troca de buffer, incluindo arquivos recentes |
| `C-c p f` | Abre um arquivo do projeto |
| `C-c p g` | Busca no projeto com ripgrep (`consult-project-extra`) |
| `M-g g` | Vai para uma linha |
| `M-g i` | Vai para uma função ou seção (imenu) |
| `M-y` | Histórico do kill ring |
| `C-.` | Ações sobre o item sob o cursor ou o candidato (embark) |
| `C-h B` | Lista todos os atalhos ativos |

### Código (eglot + flymake)

| Tecla | Ação |
|---|---|
| `M-.` / `M-,` | Vai para a definição / volta |
| `C-c l r` | Renomeia o símbolo |
| `C-c l a` | Code actions |
| `C-c l f` | Formata o buffer |
| `C-c l d` | Abre a documentação do símbolo |
| `M-n` / `M-p` | Próximo / anterior erro |

### Janelas, buffers e ferramentas

| Tecla | Ação |
|---|---|
| `C-<tab>` | Vai para a próxima janela |
| `M-<setas>` | Redimensiona a janela |
| `C-c <esquerda>` / `C-c <direita>` | Desfaz / refaz mudanças no layout das janelas (winner) |
| `C-c C-w <n>` | Muda para o layout de janelas `n` (eyebrowse) |
| `C-c b k` | Fecha todos os buffers com uma extensão |
| `C-x g` | Magit (status do Git) |
| `C-c t` | Terminal (vterm) |
| `<f8>` | Abre/fecha a árvore de arquivos (treemacs) |
| `<f9>` | Liga/desliga a coloração dos parênteses |

### Edição

| Tecla | Ação |
|---|---|
| `C-<baixo>` / `C-<cima>` | Adiciona um cursor na próxima / anterior ocorrência |
| `C-c C-<baixo>` | Adiciona um cursor em todas as ocorrências |
| `C-<escape>` | Sai dos múltiplos cursores |
| `TAB` | Indenta ou, se a linha já estiver indentada, completa |

O completion (corfu) aparece sozinho depois de 2 caracteres. Se esquecer o resto de um atalho, pare no meio do prefixo, por exemplo `C-c`: o which-key mostra as opções.

## O que está instalado

| Área | Pacotes |
|---|---|
| Completion | vertico, orderless, marginalia, consult, consult-project-extra, embark, embark-consult, corfu, cape |
| Interface | doom-modeline, dashboard, nerd-icons (+ corfu/completion/dired), treemacs, treemacs-nerd-icons, eyebrowse, which-key, highlight-symbol, highlight-parentheses, rainbow-delimiters |
| Git | magit, treemacs-magit, git-gutter |
| Código | eglot, flymake, multiple-cursors, pyvenv, bash-completion |
| Linguagens | go-mode, yaml-mode, k8s-mode, dockerfile-mode, terraform-mode (formata ao salvar), groovy-mode (`Jenkinsfile`), markdown-mode (`.md` abre em `gfm-mode`) |
| Terminal | vterm |
| Claude | claude-code-ide, gptel |

### Servidores de linguagem (eglot)

O eglot inicia sozinho quando você abre arquivos destas linguagens:

| Linguagem | Servidor |
|---|---|
| Python | `pylsp` / `pyright` |
| Shell | `bash-language-server` |
| Go | `gopls` |
| C/C++ | `clangd` (pacote `clang`) |
| YAML | `yaml-language-server` |
| JSON | `vscode-json-languageserver` |

Terraform não tem servidor configurado: o `terraform-ls` só existe no AUR.

## Comportamentos padrão

- Sem arquivos de backup, auto-save ou lock files.
- Os buffers recarregam sozinhos quando o arquivo muda no disco, inclusive quando é o Claude que edita.
- Os números de linha e o destaque de espaços no fim da linha só aparecem em buffers de código, texto e config. No vterm e no minibuffer, não.
- O Emacs guarda o histórico do minibuffer, a posição do cursor em cada arquivo e os arquivos recentes.
- A indentação usa espaços, com largura 2.
- Ao salvar, o arquivo ganha uma quebra de linha no final.
- As personalizações feitas pelo `M-x customize` vão para `~/.emacs.d/custom.el`, que fica fora do repositório.

## Limitações conhecidas

- A pré-visualização de Markdown usa o `multimarkdown`, que não está instalado porque só existe no AUR.
- Depois de trocar o tema do Omarchy, os buffers de vterm que já estavam abertos mantêm as cores antigas. Os novos já abrem com o tema atual.
