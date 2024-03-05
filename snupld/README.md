# snupld

snupld is a language server for SnuPL/2 which supports the following features:

- Error Diagnostics (aka. red squiggly lines)
- Code Completion
- Find References
- Go to Definition
- Hover Information
- Rename Symbol
- Semantic Highlighting
- Document Symbol

## Installation

You can get a prebuilt binary [here](https://github.com/VioletXF/snupl-lsp/releases/). If you want to build snupld with your snuplc implementation, follow the steps below:

First you need to implement following methods in `snuplc` since `snupld`'s scope detection is implemented in a very dumb way (or feel free to implement & contribute your own scope detection in `snupld`):
```cpp
// return the very first token of the statement sequence (e.g. "begin")
CToken CAstScope::GetStatementSequenceBeginToken();

// return the very first token of the scope (e.g. "procedure", "function", "module")
CToken CAstScope::GetScopeBeginToken();

// return the end token of the scope (e.g. "end")
CToken CAstScope::GetEndToken();
```

Then build `snupld` along with `snuplc`:
```bash
SNUPLC_DIR=/path/to/snuplc make install # make sure to locate the entire snuplc directory, not the '/src' directory
```

You might need superuser permission to install the binary to `/usr/local/bin`:

```bash
sudo SNUPLC_DIR=/path/to/snuplc make install
```

### Vim

Install [vim-lsp](https://github.com/prabirshrestha/vim-lsp) and add the following to your vimrc:

```vim
autocmd BufNewFile,BufRead *.mod set filetype=snupl2
au User lsp_setup call lsp#register_server({
  \ 'name': 'snupld',
  \ 'cmd': {server_info->['snupld', '--stdio']},
  \ 'whitelist': ['snupl2'],
  \ })
```

### VSCode

Install the SnuPL/2 extension from [lsp/vscode-extension](../vscode-extension/README.md).
