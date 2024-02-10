# SnuPL/2 VSCode Extension

- Supports Syntax Highlighting
- Supports SnuPL/2 Language Server (requires [snupld](../snupld/README.md))

## Installation

Install prebuilt vsix package by:
```bash
code --install-extension snupl2-lsp-1.0.0.vsix
```

Of course you can make a vsix package by yourself using vsce:
```bash 
vsce package --out snupl2-lsp.vsix 
code --install-extension snupl2-lsp.vsix
```

Then restart VSCode.
