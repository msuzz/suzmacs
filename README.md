

# suzmacs
This is my personal Emacs configuration that I have gradually constructed over
the past couple of years. It is a reflection of my programming interests, and
as such mainly centres around Java, Perl, C and C++. However, there are many
general quality-of-life improvements, and the frameworks used do support many
other languages with a bit of configuration.

This config has been created and tested on Emacs running on Cygwin.

Features include:  

  - Everything self-contained in ~/.emacs.d directory  
  - Seperate folder for themes (~/emacs.d/themes)  
  - MELPA enabled (naturally)  
  - Many customisations including maximise-on-startup, column indicators at 80,  
    100 and 120, minimal toolbars, line/column numbers, etc...  
  - Recent files list with C-x C-r  
  - Redo+ (Undo with C-/, redo with C-?)  
  - Reload current file with C-x C-v  
  - Visual regexp (search/replace replacement bound to C-c 5)  
  - guess-mode to guess tab width settings based on file  
  - Smart tabs enabled for C, C++, Java, perl (C-x h C-M-\ to retab whole file)  
  - which-key (Displays keybinding completions in a popup automatically)  
  - Treemacs (C-x t t to show/hide tree, M-0 to switch to tree window)  
  - Flycheck (Syntax checker)  
  - Projectile (Project management)  
  - LSP mode configured for Java and C/C++ (M-. for definitions, M-? for references)  
  - DAP mode for debugging Java and C/C++ (C-c b b for breakpoint toggle, C-c b d to debug)  
  - RealGUD (run external debuggers)  
  - Company (text-completion framework)  
  - Magit (Git user-interface, C-x g to open, C-x M-g for shortcuts)  
  - Override perl-mode with cperl-mode  
  - Markdown preview mode (M-x markdown-live-preview-mode)  

# Installation
Clone this repo to ~/.emacs.d and you should be good to go. Make sure there
is no existing .emacs file in your home directory.

Emacs will take a long time to start as packages are installed/compiled. You
may need to restart a couple of times for the process to complete.

# Specifying theme and font
If you want to change the theme or font, I recommend you do this in Emacs
itself, which will write the changes to ~/.emacs.d/custom.el.

For the theme:

> `M-x customize-themes`

For the font:

> `M-x customize-face`

custom.el is included in the .gitignore file, so your personal preferences
shall remain untouched if you want to keep this repo up to date.

Additional themes can be installed via MELPA or by placement in ~/.emacs.d/themes/.
This folder is also included in the .gitignore, so any custom themes will be safe
when pulling updates.

# Notes and TODO

  - LSP mode requires the executable 'ccls' from LLVM to be in your PATH.  
	- On Linux, installing LLVM from your package manager should work.  
	- On Windows, the LLVM project provides an [installer](https://releases.llvm.org/)
      with an option to add the executables folder to your PATH.  
    - On Cygwin, you will need to compile ccls yourself (see [here](https://github.com/MaskRay/ccls/wiki/Build)).  
	
	The path to the 'ccls' binary can be specified by adding this line to your custom.el:
	
    > `(setq ccls-executable "/path/to/ccls")`
	
  - DAP mode requires the [vscode-cpptools](https://github.com/microsoft/vscode-cpptools)
    VS Code extension to be installed. DAP includes a function to do this for you:
    
    > `M-x dap-cpptools-setup`
    
    This does not work on Cygwin however, and you will have to manually install:
    
    The .vsix file from the Github releases page can be opened as an archive. The
    'extension' folder must be placed in the ~/.emacs.d/.extension/vscode/cpptools'
    folder. Finally, run dap-cpptools-setup as above.
    
  - Git operations in Magit run *very* slowly on Windows and Cygwin. Compiling
	libgit and enabling support in Magit is supposed to help, but I could not
	get this to work on Cygwin.

  - Things will be modularised in the future. Currently, gnu-emacs.el is a highly
    monolithic file, and I plan to break it out into separate lisp files for variables,
    packages, etc. This will make things faster if you don't need a bunch of the
    stuff provided.
